{-# LANGUAGE OverloadedStrings #-}

-- | @jl4-mlir@ CLI — compile and run L4 programs via MLIR/WASM.
--
-- == Subcommands
--
-- [@wasm FILE.l4@]
--   Compile an L4 source file to a @.wasm@ binary plus a
--   @.schema.json@ sidecar. The schema documents every @\@export@\-ed
--   function's parameter names, types, and JSON property mapping — it
--   is byte-compatible with the @jl4-service@ HTTP @/eval@ endpoint
--   schema, so a compiled @.wasm@ can be dropped into any system that
--   already speaks the service protocol.
--
-- [@run FILE.wasm@]
--   Execute an exported function. The request body is a JSON document
--   in the same shape the service expects (see 'FnArguments'):
--   @{\"arguments\": {...}, \"startTime\": ..., \"events\": [...]}@.
--   The response is a JSON document in the service's 'ResponseWithReason'
--   shape. There are deliberately no positional CLI args for function
--   inputs — this keeps the protocol identical between the local WASM
--   runner and the HTTP service.
module Main (main) where

import Control.Monad (when, unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson.Pretty
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Options.Applicative
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (takeBaseName, takeDirectory, dropExtension, (</>))
import System.IO (stderr)
import System.Process (readProcessWithExitCode)
import System.Directory (findExecutable, doesFileExist)

import L4.MLIR.Pipeline
import L4.MLIR.Schema (FunctionExport(..))
import L4.MLIR.Marshal

-- ---------------------------------------------------------------------------
-- CLI types
-- ---------------------------------------------------------------------------

data Command
  = WasmCmd WasmOptions
  | RunCmd RunOptions
  | ListCmd ListOptions

data WasmOptions = WasmOptions
  { wasmInput     :: FilePath
  , wasmOutput    :: Maybe FilePath
  , wasmTarget    :: OutputTarget
  , wasmOptLevel  :: Int
  , wasmVerbose   :: Bool
  , wasmKeepFiles :: Bool
  }

data RunOptions = RunOptions
  { runWasmFile  :: FilePath        -- ^ Either @.wasm@ or @.schema.json@
  , runFunction  :: Text            -- ^ Sanitized API name, e.g. @calculate-bonus@
  , runInputFile :: Maybe FilePath  -- ^ Defaults to stdin
  , runRuntime   :: WasmRuntime
  , runPretty    :: Bool
  }

data ListOptions = ListOptions
  { listWasmFile   :: FilePath
  , listSchemaOnly :: Bool
  }

data WasmRuntime = Wasmtime | Wasmer | Node
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

commandParser :: Parser Command
commandParser = subparser
  ( command "wasm" (info (WasmCmd  <$> wasmOptionsParser)
      (progDesc "Compile an L4 source file to .wasm + .schema.json"))
  <> command "run"  (info (RunCmd  <$> runOptionsParser)
      (progDesc "Execute an exported function with a JSON input (stdin or --input)"))
  <> command "list" (info (ListCmd <$> listOptionsParser)
      (progDesc "List exported functions in a compiled .wasm bundle"))
  )

wasmOptionsParser :: Parser WasmOptions
wasmOptionsParser = WasmOptions
  <$> strArgument (metavar "FILE.l4" <> help "L4 source file to compile")
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE"
      <> help "Output file path (extension is replaced as needed)"))
  <*> outputTargetParser
  <*> option auto (long "opt-level" <> short 'O' <> metavar "N" <> value 2
      <> help "Optimization level 0-3 (default: 2)")
  <*> switch (long "verbose" <> short 'v' <> help "Print toolchain commands")
  <*> switch (long "keep-intermediate" <> short 'k'
      <> help "Keep intermediate .mlir and .ll files")

outputTargetParser :: Parser OutputTarget
outputTargetParser =
      flag' EmitMLIR (long "mlir-only" <> help "Stop after emitting .mlir")
  <|> flag' EmitLLVM (long "llvm-only" <> help "Stop after emitting .ll")
  <|> pure EmitWasm

runOptionsParser :: Parser RunOptions
runOptionsParser = RunOptions
  <$> strArgument (metavar "FILE.wasm"
      <> help "Compiled WASM file (the matching .schema.json must be alongside)")
  <*> strOption (long "function" <> short 'f' <> metavar "NAME"
      <> help "Exported function to call (sanitized API name, e.g. 'calculate-bonus')")
  <*> optional (strOption (long "input" <> short 'i' <> metavar "FILE.json"
      <> help "JSON input file (default: stdin)"))
  <*> runtimeParser
  <*> switch (long "pretty" <> help "Pretty-print the JSON response")

runtimeParser :: Parser WasmRuntime
runtimeParser =
      flag' Wasmtime (long "wasmtime" <> help "Use wasmtime runtime")
  <|> flag' Wasmer (long "wasmer" <> help "Use wasmer runtime")
  <|> flag' Node  (long "node"    <> help "Use Node.js WebAssembly")
  <|> pure Node

listOptionsParser :: Parser ListOptions
listOptionsParser = ListOptions
  <$> strArgument (metavar "FILE.wasm"
      <> help "Compiled WASM file or matching .schema.json")
  <*> switch (long "schema" <> help "Print the full JSON schema instead of just function names")

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cmd <- execParser $ info (commandParser <**> helper)
    ( fullDesc
    <> header "jl4-mlir — L4 → MLIR → WASM compiler"
    <> progDesc "Compile L4 programs to WebAssembly and execute them with jl4-service-compatible JSON I/O"
    )
  case cmd of
    WasmCmd opts -> runWasmCmd opts
    RunCmd opts  -> runRunCmd opts
    ListCmd opts -> runListCmd opts

-- ---------------------------------------------------------------------------
-- wasm subcommand
-- ---------------------------------------------------------------------------

runWasmCmd :: WasmOptions -> IO ()
runWasmCmd opts = do
  let config = PipelineConfig
        { outputTarget = opts.wasmTarget
        , outputDir    = maybe (takeDirectory opts.wasmInput) takeDirectory opts.wasmOutput
        , outputName   = (takeBaseName <$> opts.wasmOutput) <|> Just (takeBaseName opts.wasmInput)
        , optimLevel   = opts.wasmOptLevel
        , verbose      = opts.wasmVerbose
        , keepIntermediate = opts.wasmKeepFiles
        }
  result <- compileToPipeline config opts.wasmInput
  case result of
    Left errs -> do
      Text.IO.hPutStrLn stderr "Compilation failed:"
      mapM_ (\e -> Text.IO.hPutStrLn stderr $ "  " <> e) errs
      exitFailure
    Right outPath -> do
      Text.IO.putStrLn $ "Output: " <> Text.pack outPath
      exitSuccess

-- ---------------------------------------------------------------------------
-- list subcommand
-- ---------------------------------------------------------------------------

runListCmd :: ListOptions -> IO ()
runListCmd opts = do
  bundle <- loadBundleOrExit opts.listWasmFile
  if opts.listSchemaOnly
    then LBSC.putStrLn (Aeson.Pretty.encodePretty (bundle.brRawSchema))
    else do
      Text.IO.putStrLn $ "Exports from " <> bundle.brWasmFile <> ":"
      mapM_ printExport (bundle.brExports)
  where
    printExport fe = do
      Text.IO.putStrLn $ "  " <> fe.apiName
        <> " → " <> fe.returnType
        <> (if fe.isDeontic then " (DEONTIC)" else "")
      unless (Text.null fe.description) $
        Text.IO.putStrLn $ "    " <> fe.description

-- ---------------------------------------------------------------------------
-- run subcommand
-- ---------------------------------------------------------------------------

runRunCmd :: RunOptions -> IO ()
runRunCmd opts = do
  bundle <- loadBundleOrExit opts.runWasmFile
  let wasmPath = resolveWasmPath opts.runWasmFile bundle

  funcExport <- case findExport opts.runFunction bundle of
    Nothing -> do
      Text.IO.hPutStrLn stderr $ "Function not found: " <> opts.runFunction
      Text.IO.hPutStrLn stderr "Available exports:"
      mapM_ (\fe -> Text.IO.hPutStrLn stderr $ "  " <> fe.apiName) (bundle.brExports)
      exitFailure
    Just fe -> pure fe

  requestJson <- case opts.runInputFile of
    Just path -> LBS.readFile path
    Nothing   -> LBS.getContents

  fnArgs <- case Aeson.eitherDecode requestJson of
    Left err -> do
      Text.IO.hPutStrLn stderr $ "Invalid JSON request: " <> Text.pack err
      exitFailure
    Right args -> pure (args :: FnArguments)

  resolved <- case resolveArgs funcExport fnArgs of
    Left err -> do
      Text.IO.hPutStrLn stderr $ "Request validation failed: " <> err
      exitFailure
    Right r -> pure r

  result <- case opts.runRuntime of
    Node     -> runNode wasmPath funcExport resolved
    Wasmtime -> pure $ Left "wasmtime runtime backend is not yet wired to the schema-driven marshaler. Use --node for now."
    Wasmer   -> pure $ Left "wasmer runtime backend is not yet wired to the schema-driven marshaler. Use --node for now."

  case result of
    Left err -> do
      Text.IO.hPutStrLn stderr $ "Runtime error: " <> err
      exitFailure
    Right resp -> do
      let out = if opts.runPretty
            then Aeson.Pretty.encodePretty resp
            else Aeson.encode resp
      LBSC.putStrLn out
      exitSuccess

-- ---------------------------------------------------------------------------
-- Schema bundle loading
-- ---------------------------------------------------------------------------

-- | Parsed schema bundle we actually use at runtime. Keeps the raw JSON
-- so @list --schema@ can re-emit it byte-for-byte.
data BundleRead = BundleRead
  { brWasmFile  :: Text
  , brExports   :: [FunctionExport]
  , brRawSchema :: Aeson.Value
  }

instance Aeson.FromJSON BundleRead where
  parseJSON val = Aeson.withObject "WasmBundle" (\o -> do
    wasmFile <- o Aeson..: "wasmFile"
    functionsObj :: Aeson.Object <- o Aeson..: "functions"
    exports <- traverse Aeson.parseJSON
      [ v | (_, v) <- Aeson.KeyMap.toList functionsObj ]
    pure $ BundleRead wasmFile exports val) val

loadBundleOrExit :: FilePath -> IO BundleRead
loadBundleOrExit path = do
  let schemaPath = schemaPathFor path
  exists <- doesFileExist schemaPath
  when (not exists) $ do
    Text.IO.hPutStrLn stderr $
      "Schema file not found: " <> Text.pack schemaPath
      <> "\n(Did you compile with `jl4-mlir wasm`? The schema is emitted alongside the .wasm.)"
    exitFailure
  contents <- LBS.readFile schemaPath
  case Aeson.eitherDecode contents of
    Left err -> do
      Text.IO.hPutStrLn stderr $ "Invalid schema file: " <> Text.pack err
      exitFailure
    Right b -> pure b

-- | Given either a @.wasm@ or @.schema.json@ path, return the schema path.
schemaPathFor :: FilePath -> FilePath
schemaPathFor p
  | ".schema.json" `isSuffixOf` p = p
  | ".wasm" `isSuffixOf` p = dropExtension p <> ".schema.json"
  | otherwise = p <> ".schema.json"

-- | Resolve the @.wasm@ path from the user-supplied path + schema bundle.
resolveWasmPath :: FilePath -> BundleRead -> FilePath
resolveWasmPath path bundle
  | ".wasm" `isSuffixOf` path = path
  | ".schema.json" `isSuffixOf` path =
      takeDirectory path </> Text.unpack (bundle.brWasmFile)
  | otherwise = path

-- | Find an exported function by its sanitized API name.
findExport :: Text -> BundleRead -> Maybe FunctionExport
findExport name bundle =
  case [fe | fe <- bundle.brExports, fe.apiName == name] of
    (fe : _) -> Just fe
    [] -> Nothing

-- ---------------------------------------------------------------------------
-- Runtime: Node.js
-- ---------------------------------------------------------------------------

runNode :: FilePath -> FunctionExport -> ResolvedArgs -> IO (Either Text ResponseWithReason)
runNode wasmPath fe resolved = do
  mTool <- findExecutable "node"
  case mTool of
    Nothing -> pure $ Left "node not found in PATH"
    Just node -> do
      let jsCode = generateNodeRunner wasmPath fe resolved
      (exitCode, stdout_, stderr_) <- readProcessWithExitCode node ["-e", jsCode] ""
      case exitCode of
        ExitSuccess ->
          case Aeson.eitherDecode (LBSC.pack stdout_) of
            Left err -> pure $ Left $
              "Malformed runtime output: " <> Text.pack err
              <> "\n-- raw stdout --\n" <> Text.pack stdout_
            Right r -> pure $ Right r
        ExitFailure _ -> pure $ Left $ Text.pack stderr_

-- | Generate the Node.js runner script.
--
-- The script loads the .wasm, supplies stubs for the @env.__l4_*@
-- runtime imports, and marshals each positional argument into a WASM
-- value based on its schema type. Strings become pointers to UTF-8
-- bytes in linear memory, records become laid-out structs, and lists
-- become linked-list nodes.
--
-- The final result is printed as a JSON @ResponseWithReason@ document.
generateNodeRunner :: FilePath -> FunctionExport -> ResolvedArgs -> String
generateNodeRunner wasmPath fe resolved =
  let argValuesJson =
        LBSC.unpack $ Aeson.encode $ Aeson.toJSON
          [ argJson v | (_, _, v) <- resolved.raPositional ]
      schemaTypesJson =
        LBSC.unpack $ Aeson.encode $ Aeson.toJSON
          [ p | (_, p, _) <- resolved.raPositional ]
  in unlines
    [ "const fs = require('fs');"
    , "const buf = fs.readFileSync(" <> show wasmPath <> ");"
    , ""
    , "let memory, memView, memU8;"
    , "let heapPtr = 1024;"
    , "function allocBytes(n) {"
    , "  const p = heapPtr;"
    , "  heapPtr += (n + 7) & ~7;"
    , "  return p;"
    , "}"
    , ""
    , "function writeString(s) {"
    , "  const bytes = Buffer.from(String(s), 'utf8');"
    , "  const p = allocBytes(bytes.length + 1);"
    , "  memU8.set(bytes, p);"
    , "  memU8[p + bytes.length] = 0;"
    , "  return p;"
    , "}"
    , ""
    , "function marshalArg(value, schema) {"
    , "  if (value === null || value === undefined) return 0;"
    , "  if (schema && schema.enum && schema.enum.length > 0) {"
    , "    // Enum: index into the enum list (matches compiler's i32 tag)"
    , "    const idx = schema.enum.indexOf(String(value));"
    , "    return idx >= 0 ? idx : 0;"
    , "  }"
    , "  switch (schema && schema.type) {"
    , "    case 'number':  return Number(value);"
    , "    case 'boolean': return value ? 1 : 0;"
    , "    case 'string':  return writeString(value);"
    , "    case 'array':   return marshalList(value, (schema && schema.items) || {});"
    , "    case 'object':  return marshalStruct(value, schema);"
    , "    default: return Number(value) || 0;"
    , "  }"
    , "}"
    , ""
    , "function marshalStruct(value, schema) {"
    , "  if (!schema || !schema.properties) return 0;"
    , "  const order = schema.propertyOrder || Object.keys(schema.properties);"
    , "  const size = order.length * 8;"
    , "  const p = allocBytes(size);"
    , "  order.forEach((name, idx) => {"
    , "    const fieldSchema = schema.properties[name] || {};"
    , "    const v = (value || {})[name];"
    , "    const marshaled = marshalArg(v, fieldSchema);"
    , "    if (fieldSchema.type === 'number') {"
    , "      memView.setFloat64(p + idx * 8, Number(marshaled), true);"
    , "    } else {"
    , "      memView.setBigUint64(p + idx * 8, BigInt(marshaled), true);"
    , "    }"
    , "  });"
    , "  return p;"
    , "}"
    , ""
    , "function marshalList(values, itemSchema) {"
    , "  if (!Array.isArray(values) || values.length === 0) return 0;"
    , "  let head = 0;"
    , "  for (let i = values.length - 1; i >= 0; i--) {"
    , "    const p = allocBytes(16);"
    , "    const item = marshalArg(values[i], itemSchema);"
    , "    if (itemSchema && itemSchema.type === 'number') {"
    , "      memView.setFloat64(p, Number(item), true);"
    , "    } else {"
    , "      memView.setBigUint64(p, BigInt(item), true);"
    , "    }"
    , "    memView.setBigUint64(p + 8, BigInt(head), true);"
    , "    head = p;"
    , "  }"
    , "  return head;"
    , "}"
    , ""
    , "const imports = {"
    , "  env: {"
    , "    __l4_pow:   Math.pow,"
    , "    __l4_min:   Math.min,"
    , "    __l4_max:   Math.max,"
    , "    __l4_abs:   Math.abs,"
    , "    __l4_floor: Math.floor,"
    , "    __l4_ceil:  Math.ceil,"
    , "    __l4_round: Math.round,"
    , "    __l4_str_concat: (a, b) => a,"
    , "    __l4_str_eq:     (a, b) => (a === b) ? 1 : 0,"
    , "    __l4_str_len:    (_p) => 0,"
    , "    __l4_to_string:  (_n) => 0,"
    , "    __l4_list_count: (ptr) => {"
    , "      let n = 0, p = ptr;"
    , "      while (p !== 0 && n < 100000) {"
    , "        p = Number(memView.getBigUint64(p + 8, true));"
    , "        n++;"
    , "      }"
    , "      return n;"
    , "    },"
    , "    __l4_list_empty: () => 0,"
    , "    __l4_alloc: allocBytes,"
    , "    __l4_free:  (_p) => {},"
    , "  }"
    , "};"
    , ""
    , "WebAssembly.instantiate(buf, imports).then(({instance}) => {"
    , "  memory  = instance.exports.memory;"
    , "  memView = new DataView(memory.buffer);"
    , "  memU8   = new Uint8Array(memory.buffer);"
    , ""
    , "  const fn = instance.exports[" <> show (Text.unpack fe.wasmSymbol) <> "];"
    , "  if (!fn) {"
    , "    console.error('WASM export not found: " <> Text.unpack fe.wasmSymbol <> "');"
    , "    console.error('Available exports:', Object.keys(instance.exports).join(', '));"
    , "    process.exit(1);"
    , "  }"
    , ""
    , "  const argValues = " <> argValuesJson <> ";"
    , "  const argSchemas = " <> schemaTypesJson <> ";"
    , ""
    , "  const marshaled = argValues.map((v, i) => marshalArg(v, argSchemas[i]));"
    , "  const raw = fn(...marshaled);"
    , "  const result = unmarshalResult(raw, " <> show (Text.unpack fe.returnType) <> ");"
    , "  console.log(JSON.stringify({ result: { value: result } }));"
    , "}).catch(err => {"
    , "  console.error('WASM error:', err.message || err);"
    , "  process.exit(1);"
    , "});"
    , ""
    , "function unmarshalResult(raw, returnType) {"
    , "  if (returnType === 'BOOLEAN') return !!raw;"
    , "  if (returnType === 'NUMBER')  return Number(raw);"
    , "  if (returnType === 'STRING')  return raw;"
    , "  if (returnType && returnType.indexOf('MAYBE') === 0) return raw;"
    , "  return raw;"
    , "}"
    ]

-- | Convert a positional argument (with optional value) to a JSON value.
argJson :: Maybe FnLiteral -> Aeson.Value
argJson Nothing  = Aeson.Null
argJson (Just v) = Aeson.toJSON v
