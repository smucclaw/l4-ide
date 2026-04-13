-- | End-to-end compilation pipeline: L4 source → MLIR → WASM.
--
-- Orchestrates the full flow:
--   1. Parse & typecheck L4 source (via jl4-core's Shake rules, resolving imports)
--   2. Lower the resolved AST to MLIR IR
--   3. Emit MLIR textual IR to disk
--   4. Invoke mlir-opt to lower through standard passes
--   5. Invoke mlir-translate to emit LLVM IR
--   6. Invoke llc / wasm-ld to produce a .wasm binary
--
-- Steps 4–6 require the MLIR/LLVM toolchain to be installed.
-- The pipeline also supports stopping at intermediate stages
-- (emit .mlir only, emit .ll only) for debugging.
module L4.MLIR.Pipeline
  ( -- * Pipeline configuration
    PipelineConfig (..)
  , OutputTarget (..)
  , defaultConfig
    -- * Running the pipeline
  , compileToPipeline
  , compileToMLIR
  , compileToWasm
    -- * Toolchain invocation
  , runMLIROpt
  , runMLIRTranslate
  , runWasmLd
  , findToolchainTool
  ) where

import L4.MLIR.IR (MLIRModule(..))
import L4.MLIR.Emit (renderMLIR)
import L4.MLIR.Lower (lowerProgramWithInfo, dedupAndSynthExterns)
import L4.MLIR.Runtime.Builtins (builtinDeclarations)
import L4.MLIR.Schema (WasmBundle, bundleExports, writeBundleFile)

import L4.Syntax (Module, Resolved)
import L4.TypeCheck.Types (InfoMap)

import qualified LSP.Core.Shake as Shake
import qualified LSP.L4.Rules as Rules
import LSP.L4.Oneshot (oneshotL4ActionAndErrors)
import Language.LSP.Protocol.Types (normalizedFilePathToUri)

import L4.EvaluateLazy (readFixedNowEnv, resolveEvalConfig)
import L4.TracePolicy (cliDefaultPolicy)

import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), replaceExtension, takeBaseName)
import System.Process (readProcessWithExitCode)

-- | What to produce at the end of the pipeline.
data OutputTarget
  = EmitMLIR    -- ^ Stop after emitting .mlir (for inspection / debugging)
  | EmitLLVM    -- ^ Lower to LLVM IR and stop
  | EmitWasm    -- ^ Full pipeline: produce .wasm binary
  deriving stock (Eq, Show)

-- | Pipeline configuration.
data PipelineConfig = PipelineConfig
  { outputTarget :: OutputTarget
  , outputDir    :: FilePath       -- ^ Where to write output artifacts
  , outputName   :: Maybe String   -- ^ Override output filename (without extension)
  , optimLevel   :: Int            -- ^ Optimization level (0-3)
  , verbose      :: Bool           -- ^ Print toolchain commands
  , keepIntermediate :: Bool       -- ^ Keep intermediate .mlir / .ll files
  }
  deriving stock (Show)

defaultConfig :: PipelineConfig
defaultConfig = PipelineConfig
  { outputTarget = EmitWasm
  , outputDir = "."
  , outputName = Nothing
  , optimLevel = 2
  , verbose = False
  , keepIntermediate = False
  }

-- ---------------------------------------------------------------------------
-- Step 1: Parse, resolve imports, and typecheck
-- ---------------------------------------------------------------------------

-- | Parse, resolve imports, and typecheck an L4 file using jl4-core's
-- Shake rules. Returns the typechecked module (with all imports resolved)
-- and any diagnostic messages.
typecheckL4File :: FilePath -> IO (Either [Text] (Rules.TypeCheckResult, [Rules.TypeCheckResult]))
typecheckL4File filepath = do
  fixedNow <- readFixedNowEnv
  evalConfig <- resolveEvalConfig fixedNow cliDefaultPolicy

  (errs, mTc) <- oneshotL4ActionAndErrors evalConfig filepath $ \nfp -> do
    let uri = normalizedFilePathToUri nfp
    _ <- Shake.addVirtualFileFromFS nfp
    Shake.use Rules.TypeCheck uri

  case mTc of
    Just tc | tc.success -> pure $ Right (tc, tc.dependencies)
    Just _tc -> pure $ Left (errs ++ ["Typecheck completed with errors"])
    Nothing -> pure $ Left (if null errs then ["Typecheck failed"] else errs)

-- ---------------------------------------------------------------------------
-- Step 2-3: Lower to MLIR and emit
-- ---------------------------------------------------------------------------

-- | Compile an L4 file through the full pipeline (parse → typecheck → MLIR → target).
compileToPipeline :: PipelineConfig -> FilePath -> IO (Either [Text] FilePath)
compileToPipeline config filepath = do
  when config.verbose $
    Text.IO.putStrLn $ "Typechecking " <> Text.pack filepath <> "..."

  tcResult <- typecheckL4File filepath
  case tcResult of
    Left errs -> pure $ Left errs
    Right (tc, deps) -> do
      let mainModule = tc.module'
          depModules = map (.module') deps
          mlirMod = lowerModuleWithDepsInfo tc.infoMap mainModule depModules
          baseName = maybe (takeBaseName filepath) id config.outputName
          -- Build the function schema bundle — this is what the `run`
          -- subcommand (and any external caller, including jl4-service)
          -- reads to marshal JSON arguments into WASM calls.
          schemaBundle = bundleExports
            (Text.pack (baseName <> ".wasm"))
            (computeVersion filepath)  -- placeholder — real one in Compiler.hs
            mainModule

      case config.outputTarget of
        EmitMLIR -> do
          let outPath = config.outputDir </> baseName <> ".mlir"
          emitMLIRFile mlirMod outPath
          writeSchemaAlongside config baseName schemaBundle
          when config.verbose $
            Text.IO.putStrLn $ "Wrote " <> Text.pack outPath
          pure $ Right outPath

        EmitLLVM -> do
          let mlirPath = config.outputDir </> baseName <> ".mlir"
              llPath = config.outputDir </> baseName <> ".ll"
          emitMLIRFile mlirMod mlirPath
          writeSchemaAlongside config baseName schemaBundle
          result <- runMLIROpt config mlirPath (replaceExtension mlirPath ".opt.mlir")
          case result of
            Left err -> pure $ Left [err]
            Right optPath -> do
              result2 <- runMLIRTranslate config optPath llPath
              unless config.keepIntermediate $ pure ()  -- TODO: cleanup
              case result2 of
                Left err -> pure $ Left [err]
                Right p  -> pure $ Right p

        EmitWasm -> do
          result <- compileToWasm config filepath mlirMod baseName
          case result of
            Right _ -> writeSchemaAlongside config baseName schemaBundle
            _ -> pure ()
          pure result

-- | Write the @.schema.json@ sidecar file next to the .wasm binary.
writeSchemaAlongside :: PipelineConfig -> String -> WasmBundle -> IO ()
writeSchemaAlongside config baseName bundle = do
  let schemaPath = config.outputDir </> baseName <> ".schema.json"
  writeBundleFile schemaPath bundle
  when config.verbose $
    Text.IO.putStrLn $ "Wrote schema: " <> Text.pack schemaPath

-- | Placeholder version — a real implementation would hash the source.
computeVersion :: FilePath -> Text
computeVersion _ = "0.1.0"

-- | Lower an L4 module with its resolved dependencies and the
-- typechecker's 'InfoMap'. Dependencies contribute their type
-- declarations and extern function declarations; their bodies are
-- not re-emitted.
lowerModuleWithDepsInfo :: InfoMap -> Module Resolved -> [Module Resolved] -> MLIRModule
lowerModuleWithDepsInfo info mainMod deps =
  let baseMlir = lowerProgramWithInfo info mainMod deps
      builtins = builtinDeclarations
      -- Dedup + synth-externs happens *after* the builtins are
      -- prepended, so the post-pass sees every declaration in the
      -- final module when deciding which names are already resolved.
      merged = dedupAndSynthExterns (builtins ++ baseMlir.moduleOps)
  in baseMlir { moduleOps = merged }

-- | Compile directly to MLIR IR (no file output).
compileToMLIR :: FilePath -> IO (Either [Text] MLIRModule)
compileToMLIR filepath = do
  tcResult <- typecheckL4File filepath
  case tcResult of
    Left errs -> pure $ Left errs
    Right (tc, deps) -> do
      let mainModule = tc.module'
          depModules = map (.module') deps
      pure $ Right $ lowerModuleWithDepsInfo tc.infoMap mainModule depModules

-- | Write MLIR textual IR to a file.
emitMLIRFile :: MLIRModule -> FilePath -> IO ()
emitMLIRFile mlirMod path = Text.IO.writeFile path (renderMLIR mlirMod)

-- ---------------------------------------------------------------------------
-- Step 4-6: MLIR toolchain invocation
-- ---------------------------------------------------------------------------

-- | Compile the full MLIR → WASM pipeline.
compileToWasm :: PipelineConfig -> FilePath -> MLIRModule -> String -> IO (Either [Text] FilePath)
compileToWasm config _srcPath mlirMod baseName = do
  let mlirPath = config.outputDir </> baseName <> ".mlir"
      optPath  = config.outputDir </> baseName <> ".opt.mlir"
      llPath   = config.outputDir </> baseName <> ".ll"
      objPath  = config.outputDir </> baseName <> ".o"
      wasmPath = config.outputDir </> baseName <> ".wasm"

  -- Emit MLIR
  emitMLIRFile mlirMod mlirPath
  when config.verbose $
    Text.IO.putStrLn $ "Emitted MLIR: " <> Text.pack mlirPath

  -- Lower MLIR (convert-scf-to-cf, convert-func-to-llvm, convert-arith-to-llvm, etc.)
  r1 <- runMLIROpt config mlirPath optPath
  case r1 of
    Left err -> pure $ Left [err]
    Right _ -> do
      -- Convert to LLVM IR
      r2 <- runMLIRTranslate config optPath llPath
      case r2 of
        Left err -> pure $ Left [err]
        Right _ -> do
          -- Compile LLVM IR to object file (wasm32)
          r3 <- runLLC config llPath objPath
          case r3 of
            Left err -> pure $ Left [err]
            Right _ -> do
              -- Link to .wasm
              r4 <- runWasmLd config objPath wasmPath
              case r4 of
                Left err -> pure $ Left [err]
                Right _ -> do
                  when config.verbose $
                    Text.IO.putStrLn $ "Compiled WASM: " <> Text.pack wasmPath
                  pure $ Right wasmPath

-- | Run mlir-opt with lowering passes for WASM target.
runMLIROpt :: PipelineConfig -> FilePath -> FilePath -> IO (Either Text FilePath)
runMLIROpt config inputPath outputPath = do
  tool <- findToolchainTool "mlir-opt"
  case tool of
    Nothing -> pure $ Left "mlir-opt not found in PATH. Install MLIR/LLVM toolchain."
    Just mlirOpt -> do
      let passes = concat
            [ ["--convert-scf-to-cf"]
            , ["--convert-cf-to-llvm"]
            , ["--convert-func-to-llvm"]
            , ["--convert-arith-to-llvm"]
            , ["--finalize-memref-to-llvm"]
            , ["--reconcile-unrealized-casts"]
            , ["-o", outputPath]
            , [inputPath]
            ]
      runTool config mlirOpt passes outputPath

-- | Run mlir-translate to convert LLVM dialect to LLVM IR.
runMLIRTranslate :: PipelineConfig -> FilePath -> FilePath -> IO (Either Text FilePath)
runMLIRTranslate config inputPath outputPath = do
  tool <- findToolchainTool "mlir-translate"
  case tool of
    Nothing -> pure $ Left "mlir-translate not found in PATH. Install MLIR/LLVM toolchain."
    Just mlirTranslate -> do
      let args = ["--mlir-to-llvmir", "-o", outputPath, inputPath]
      runTool config mlirTranslate args outputPath

-- | Run llc to compile LLVM IR to wasm32 object file.
runLLC :: PipelineConfig -> FilePath -> FilePath -> IO (Either Text FilePath)
runLLC config inputPath outputPath = do
  tool <- findToolchainTool "llc"
  case tool of
    Nothing -> pure $ Left "llc not found in PATH. Install LLVM toolchain."
    Just llc -> do
      let args =
            [ "-march=wasm32"
            , "-filetype=obj"
            , "-O" <> show config.optimLevel
            , "-o", outputPath
            , inputPath
            ]
      runTool config llc args outputPath

-- | Run wasm-ld to link object files into a .wasm binary.
runWasmLd :: PipelineConfig -> FilePath -> FilePath -> IO (Either Text FilePath)
runWasmLd config inputPath outputPath = do
  tool <- findToolchainTool "wasm-ld"
  case tool of
    Nothing -> pure $ Left "wasm-ld not found in PATH. Install LLVM/LLD toolchain."
    Just wasmLd -> do
      let args =
            [ "--no-entry"           -- No main() required
            , "--export-dynamic"     -- Export all non-local symbols
            , "--allow-undefined"    -- Runtime imports resolved at load time
            , "--initial-memory=1048576"  -- 1MB initial memory
            , "-o", outputPath
            , inputPath
            ]
      runTool config wasmLd args outputPath

-- | Find a toolchain binary, checking PATH.
findToolchainTool :: String -> IO (Maybe FilePath)
findToolchainTool = findExecutable

-- | Run an external tool and return the output path on success.
runTool :: PipelineConfig -> FilePath -> [String] -> FilePath -> IO (Either Text FilePath)
runTool config tool args outputPath = do
  when config.verbose $
    Text.IO.putStrLn $ Text.pack $ unwords (tool : args)
  (exitCode, _stdout, stderr_) <- readProcessWithExitCode tool args ""
  case exitCode of
    ExitSuccess -> pure $ Right outputPath
    ExitFailure code -> pure $ Left $ Text.pack $
      tool <> " failed (exit " <> show code <> "):\n" <> stderr_
