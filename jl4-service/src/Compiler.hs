module Compiler (
  compileBundle,
  buildFromCborBundle,
  computeVersion,
  toDecl,
) where

import qualified Backend.Jl4 as Jl4
import Backend.Jl4 (ModuleContext, CompiledModule(..), typecheckModule, buildImportEnvironment, getFunctionDefinition)
import Backend.Api (EvalBackend (..), FunctionDeclaration (..))
import Backend.CodeGen (isDeonticType)
import Backend.FunctionSchema (Parameters (..), Parameter (..), typeToParameter, declaresFromModule)
import BundleStore (SerializedBundle (..), StoredMetadata (..))
import Types
import Control.Monad (forM, unless)
import Control.Monad.Trans.Except (runExceptT)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (toJSON)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time (getCurrentTime)
import L4.Export (ExportedFunction (..), ExportedParam (..), getExportedFunctions, extractImplicitAssumeParams)
import L4.Syntax (Module, Resolved, Declare(..), Type'(..), RawName(..))
import Logging (Logger, logInfo, logWarn)
import qualified LSP.L4.Rules as Rules
import System.FilePath (takeExtension)

-- | Compile an in-memory source map into a deployment-ready function registry.
--
-- Returns a list of 'SerializedBundle's (one per compiled file that had exports)
-- so callers can persist CBOR caches for fast restart.
--
-- Steps:
-- 1. Identify all .l4 files that may contain exported functions
-- 2. Build ModuleContext from all sources
-- 3. For each file, typecheck and discover @export annotations
-- 4. For each export, create a RunFunction + CompiledModule
-- 5. Build metadata with SHA-256 version
compileBundle
  :: Logger
  -> Map FilePath Text
  -> IO (Either Text (Map Text ValidatedFunction, DeploymentMetadata, [SerializedBundle]))
compileBundle logger sources = do
  let l4Files = Map.toList $ Map.filterWithKey (\k _ -> takeExtension k == ".l4") sources
  if null l4Files
    then pure $ Left "No .l4 files found in bundle"
    else do
      -- Build module context from all sources (for import resolution)
      let moduleContext = sources

      -- Try each file for exported functions
      results <- forM l4Files $ \(filepath, content) ->
        compileSingleFile logger filepath content moduleContext

      let allFunctions = concatMap (either (const []) fst) results
          allBundles = concatMap (either (const []) snd) results
          errors = [err | Left err <- results]

      if null allFunctions && not (null errors)
        then pure $ Left $ Text.intercalate "\n" errors
        else do
          -- Build function map
          let fnMap = Map.fromList [(fn.fnImpl.name, fn) | fn <- allFunctions]

          -- Build metadata
          now <- getCurrentTime
          let summaries = [FunctionSummary fn.fnImpl.name fn.fnImpl.description fn.fnImpl.parameters | fn <- allFunctions]
              version = computeVersion sources
              meta = DeploymentMetadata
                { metaFunctions = summaries
                , metaVersion = version
                , metaCreatedAt = now
                }

          unless (null allFunctions) $
            logInfo logger "Compilation complete"
              [ ("functionCount", toJSON (length allFunctions))
              ]

          pure $ Right (fnMap, meta, allBundles)

-- | Compile a single .l4 file's exported functions.
-- Returns a list of ValidatedFunctions and optionally a SerializedBundle
-- (if typechecking succeeded and there were exports).
compileSingleFile
  :: Logger
  -> FilePath
  -> Text
  -> ModuleContext
  -> IO (Either Text ([ValidatedFunction], [SerializedBundle]))
compileSingleFile logger filepath content moduleContext = do
  -- Typecheck the module
  (errs, mTcRes) <- typecheckModule filepath content moduleContext
  case mTcRes of
    Nothing -> do
      unless (null errs) $
        logWarn logger "Typecheck failed"
          [ ("file", toJSON filepath)
          , ("errors", toJSON errs)
          ]
      pure $ Left (Text.intercalate "\n" errs)

    Just Rules.TypeCheckResult{module' = resolvedModule, environment = env, entityInfo = ei, errors = tcErrors} -> do
      -- Discover exported functions
      let exports = getExportedFunctions resolvedModule
      if null exports
        then pure $ Right ([], [])
        else do
          -- Extract implicit ASSUMEs from type errors
          let implicitParams = extractImplicitAssumeParams tcErrors

          -- Create ValidatedFunction for each export
          fns <- forM exports $ \export -> do
            let fnDecl = exportToFunction resolvedModule implicitParams export
                apiDecl = toDecl fnDecl
            (runFn, mCompiled) <- Jl4.createFunction filepath apiDecl content moduleContext
            pure ValidatedFunction
              { fnImpl = fnDecl
              , fnEvaluator = Map.fromList [(JL4, runFn)]
              , fnCompiled = mCompiled
              , fnSources = Map.fromList [(JL4, content)]
              , fnDecisionQueryCache = Nothing
              }

          -- Build SerializedBundle from typecheck result for CBOR caching
          let bundle = SerializedBundle
                { sbModule = resolvedModule
                , sbEnvironment = env
                , sbEntityInfo = ei
                }

          logInfo logger "Found exports"
            [ ("file", toJSON filepath)
            , ("exportCount", toJSON (length fns))
            ]
          pure $ Right (fns, [bundle])

-- | Rebuild ValidatedFunctions from a deserialized CBOR bundle,
-- skipping the expensive typecheck step.
--
-- This is the fast restart path: the Module Resolved, Environment, and
-- EntityInfo are loaded from CBOR, and only the evaluator import environment
-- needs to be rebuilt (which requires the sources).
buildFromCborBundle
  :: Logger
  -> SerializedBundle
  -> Map FilePath Text       -- ^ source files (for import resolution and eval)
  -> StoredMetadata          -- ^ persisted metadata
  -> IO (Either Text (Map Text ValidatedFunction, DeploymentMetadata))
buildFromCborBundle logger bundle sources storedMeta = do
  let resolvedModule = bundle.sbModule
      env = bundle.sbEnvironment
      ei = bundle.sbEntityInfo
      moduleContext = sources

  -- Discover exported functions from the deserialized module
  let exports = getExportedFunctions resolvedModule

  if null exports
    then pure $ Left "No exported functions found in CBOR bundle"
    else do
      -- For each export, build a CompiledModule directly (no typechecking)
      fns <- forM exports $ \export -> do
        let fnDecl = exportToFunction resolvedModule [] export
            apiDecl = toDecl fnDecl
            funRawName = NormalName apiDecl.name

        -- Find the function definition in the deserialized AST
        mDecide <- runExceptT $ getFunctionDefinition funRawName resolvedModule
        case mDecide of
          Left _err -> pure Nothing
          Right decide -> do
            -- Find which source file contains this function
            -- Use the first .l4 file (most deployments have one file)
            let l4Files = Map.toList $ Map.filterWithKey (\k _ -> takeExtension k == ".l4") sources
            case l4Files of
              [] -> pure Nothing
              ((filepath, content):_) -> do
                -- Build import environment (needs source + module context)
                importEnv <- buildImportEnvironment filepath content moduleContext ei

                let compiled = CompiledModule
                      { compiledModule = resolvedModule
                      , compiledEnvironment = env
                      , compiledEntityInfo = ei
                      , compiledDecide = decide
                      , compiledModuleContext = moduleContext
                      , compiledImportEnv = importEnv
                      , compiledSource = content
                      }

                -- Create RunFunction using precompiled module
                let runFn = Jl4.createRunFunctionFromCompiled filepath apiDecl compiled

                pure $ Just ValidatedFunction
                  { fnImpl = fnDecl
                  , fnEvaluator = Map.fromList [(JL4, runFn)]
                  , fnCompiled = Just compiled
                  , fnSources = Map.fromList [(JL4, content)]
                  , fnDecisionQueryCache = Nothing
                  }

      let validFns = [fn | Just fn <- fns]

      if null validFns
        then pure $ Left "Failed to rebuild functions from CBOR bundle"
        else do
          -- Build function map
          let fnMap = Map.fromList [(fn.fnImpl.name, fn) | fn <- validFns]

          -- Reconstruct DeploymentMetadata from stored metadata
          now <- getCurrentTime
          let summaries = [FunctionSummary fn.fnImpl.name fn.fnImpl.description fn.fnImpl.parameters | fn <- validFns]
              version = storedMeta.smVersion
              meta = DeploymentMetadata
                { metaFunctions = summaries
                , metaVersion = version
                , metaCreatedAt = now
                }

          logInfo logger "Rebuilt functions from CBOR cache"
            [ ("functionCount", toJSON (length validFns))
            ]
          pure $ Right (fnMap, meta)

-- | Convert an ExportedFunction to our Function type.
exportToFunction :: Module Resolved -> [(Text, Type' Resolved)] -> ExportedFunction -> Function
exportToFunction resolvedModule implicitParams export =
  let baseParams = parametersFromExport resolvedModule export.exportParams
      declares = declaresFromModule resolvedModule
      -- Merge implicit params, avoiding duplicates
      implicitParamMap = Map.fromList
        [ (pname, (typeToParameter declares Set.empty ty) {parameterDescription = "", parameterAlias = Nothing})
        | (pname, ty) <- implicitParams
        , pname `Map.notMember` baseParams.parameterMap
        ]
      mergedParams = baseParams
        { parameterMap = baseParams.parameterMap <> implicitParamMap
        , required = baseParams.required <> Map.keys implicitParamMap
        }
      -- Detect deontic return type and add startTime/events parameters
      isDeontic = case export.exportReturnType of
        Just ty -> isDeonticType ty
        Nothing -> False
      finalParams = if isDeontic
        then mergedParams
          { parameterMap = mergedParams.parameterMap <> Map.fromList
              [ ("startTime", Parameter "number" Nothing Nothing [] "Start time for contract simulation" Nothing Nothing Nothing)
              , ("events", Parameter "array" Nothing Nothing [] "Events for contract simulation (each: {party, action, at})" Nothing Nothing
                  (Just $ Parameter "object" Nothing Nothing [] "A trace event"
                    (Just $ Map.fromList
                      [ ("party", Parameter "object" Nothing Nothing [] "The party performing the action" Nothing Nothing Nothing)
                      , ("action", Parameter "object" Nothing Nothing [] "The action performed" Nothing Nothing Nothing)
                      , ("at", Parameter "number" Nothing Nothing [] "Timestamp" Nothing Nothing Nothing)
                      ])
                    (Just ["party", "action", "at"])
                    Nothing
                  ))
              ]
          , required = mergedParams.required <> ["startTime", "events"]
          }
        else mergedParams
  in Function
    { name = export.exportName
    , description =
        let desc = Text.strip export.exportDescription
        in if Text.null desc then "Exported function" else desc
    , parameters = finalParams
    , supportedEvalBackend = [JL4]
    }

-- | Build Parameters from ExportedParam list.
parametersFromExport :: Module Resolved -> [ExportedParam] -> Parameters
parametersFromExport resolvedModule params =
  let declares = declaresFromModule resolvedModule
  in MkParameters
    { parameterMap = Map.fromList [(param.paramName, paramToParameter declares param) | param <- params]
    , required = [param.paramName | param <- params, param.paramRequired]
    }

-- | Convert an ExportedParam to a Parameter.
paramToParameter :: Map Text (Declare Resolved) -> ExportedParam -> Parameter
paramToParameter declares param =
  let p0 = maybe
              (Parameter "object" Nothing Nothing [] "" Nothing Nothing Nothing)
              (typeToParameter declares Set.empty)
              param.paramType
  in p0
    { parameterAlias = Nothing
    , parameterDescription = Text.strip $ maybe "" id param.paramDescription
    }

-- | Convert a Function to a FunctionDeclaration for the Backend.
toDecl :: Function -> FunctionDeclaration
toDecl fn =
  FunctionDeclaration
    { Backend.Api.name = fn.name
    , Backend.Api.description = fn.description
    , Backend.Api.longNames = Map.keysSet fn.parameters.parameterMap
    , Backend.Api.nameMapping = shortToLongNameMapping
    }
 where
  shortToLongNameMapping :: Map Text Text
  shortToLongNameMapping =
    Map.fromList
      [ (alias, long)
      | (long, param) <- Map.toList fn.parameters.parameterMap
      , Just alias <- [param.parameterAlias]
      ]

-- | Compute SHA-256 version from sorted source contents.
computeVersion :: Map FilePath Text -> Text
computeVersion sources =
  let sorted = sortBy (comparing fst) (Map.toList sources)
      payload = LBS.toStrict $ Builder.toLazyByteString $ mconcat
        [ Builder.byteString (Text.Encoding.encodeUtf8 $ Text.pack path)
          <> Builder.word8 0
          <> Builder.byteString (Text.Encoding.encodeUtf8 content)
          <> Builder.word8 0
        | (path, content) <- sorted
        ]
      digest = SHA256.hash payload
  in Text.pack $ concatMap (\b -> let h = showHex b "" in if length h == 1 then '0':h else h) (BS.unpack digest)

showHex :: Integral a => a -> ShowS
showHex n
  | n < 16 = showChar (intToDigit (fromIntegral n))
  | otherwise = showHex (n `div` 16) . showChar (intToDigit (fromIntegral (n `mod` 16)))
 where
  intToDigit :: Int -> Char
  intToDigit i
    | i < 10 = toEnum (fromEnum '0' + i)
    | otherwise = toEnum (fromEnum 'a' + i - 10)
