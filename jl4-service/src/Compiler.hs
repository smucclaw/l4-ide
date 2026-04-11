module Compiler (
  compileBundle,
  buildFromCborBundle,
  computeVersion,
  toDecl,
) where

import qualified Backend.Jl4 as Jl4
import Backend.Jl4 (ModuleContext, CompiledModule(..), getFunctionDefinition, buildCompiledFromShared, typecheckAndEvalBundle)
import qualified L4.Evaluate.ValueLazy as EvalEnv (Environment)
import Backend.Api (EvalBackend (..), FunctionDeclaration (..))
import Shared (validateNoSanitizationCollisions)
import Backend.CodeGen (isDeonticType)
import L4.FunctionSchema (Parameters (..), Parameter (..), typeToParameter, declaresFromModule)
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
import L4.Export (ExportedFunction (..), ExportedParam (..), getExportedFunctions, enrichReturnTypes, extractImplicitAssumeParams)
import L4.Syntax (Resolved, Declare(..), Type'(..), RawName(..), getActual, rawName, rawNameToText)
import Logging (Logger, logInfo, logWarn)
import qualified LSP.L4.Rules as Rules
import System.FilePath (takeExtension)

-- | Compile an in-memory source map into a deployment-ready function registry.
--
-- Uses a SINGLE Shake session for the entire bundle: all files are typechecked
-- and evaluated together, so Shake automatically shares parse/typecheck/eval
-- results across the import graph. This avoids creating N independent IDE
-- sessions that redundantly re-process overlapping imports.
--
-- Returns a list of 'SerializedBundle's (one per compiled file that had exports)
-- so callers can persist CBOR caches for fast restart.
compileBundle
  :: Logger
  -> Text  -- ^ deployment ID for logging
  -> Map FilePath Text
  -> IO (Either Text (Map Text ValidatedFunction, DeploymentMetadata, [SerializedBundle]))
compileBundle logger deployId sources = do
  let l4Files = Map.toList $ Map.filterWithKey (\k _ -> takeExtension k == ".l4") sources
  if null l4Files
    then pure $ Left "No .l4 files found in bundle"
    else do
      let moduleContext = sources
          allPaths = map fst l4Files

      -- Phase 1: Typecheck ALL files in one Shake session.
      -- Pass all paths as needing eval environments — we'll discover which have
      -- exports from the typecheck results, then only the eval envs for those
      -- files will actually be used.
      (_errs, tcMap, evalMap) <- typecheckAndEvalBundle moduleContext allPaths

      -- Phase 2: Process typecheck results per file
      results <- forM l4Files $ \(filepath, content) ->
        case Map.lookup filepath tcMap of
          Just (Just tcResult) ->
            processTypecheckedFile logger deployId filepath content moduleContext tcResult evalMap
          _ -> do
            logWarn logger "Typecheck failed"
              [ ("deploymentId", toJSON deployId)
              , ("file", toJSON filepath)
              ]
            pure (filepath, Left "Typecheck failed")

      let allFunctionsWithFile = [ (fp, fn) | (fp, Right (fns, _)) <- results, fn <- fns ]
          allFunctions = map snd allFunctionsWithFile
          allBundles = concatMap (either (const []) snd . snd) results
          errors = [err | (_, Left err) <- results]

      if null allFunctions && not (null errors)
        then pure $ Left $ Text.intercalate "\n" errors
        else do
          let fnMap = Map.fromList [(fn.fnImpl.name, fn) | fn <- allFunctions]

          now <- getCurrentTime
          let summaries = [FunctionSummary { fsName = fn.fnImpl.name, fsDescription = fn.fnImpl.description, fsParameters = fn.fnImpl.parameters, fsReturnType = fn.fnImpl.returnType, fsSection = Nothing, fsIsDeontic = fn.fnImpl.isDeontic, fsSourceFile = Just (Text.pack fp) } | (fp, fn) <- allFunctionsWithFile]
              version = computeVersion sources
              fileEntries = buildFileEntries deployId sources summaries
              meta = DeploymentMetadata
                { metaFunctions = summaries
                , metaFiles = fileEntries
                , metaVersion = version
                , metaCreatedAt = now
                }

          unless (null allFunctions) $
            logInfo logger "Compilation complete"
              [ ("deploymentId", toJSON deployId)
              , ("functionCount", toJSON (length allFunctions))
              ]

          pure $ Right (fnMap, meta, allBundles)

-- | Process a single typechecked file: discover exports, build CompiledModules.
-- The typecheck result and evaluator import environment come from the shared
-- Shake session (not from per-file oneshot sessions).
processTypecheckedFile
  :: Logger
  -> Text  -- ^ deployment ID
  -> FilePath
  -> Text  -- ^ source content
  -> ModuleContext
  -> Rules.TypeCheckResult
  -> Map FilePath EvalEnv.Environment  -- ^ Import environments from shared session
  -> IO (FilePath, Either Text ([ValidatedFunction], [SerializedBundle]))
processTypecheckedFile logger deployId filepath content moduleContext
  tcResult@Rules.TypeCheckResult{module' = resolvedModule, environment = env, entityInfo = ei, errors = tcErrors}
  evalMap = do
    let exports = enrichReturnTypes ei $ getExportedFunctions resolvedModule
    if null exports
      then pure (filepath, Right ([], []))
      else do
        let implicitParams = extractImplicitAssumeParams tcErrors
            allDeclares = collectAllDeclares tcResult
            -- Look up the import environment from the shared session
            importEnv = Map.findWithDefault Jl4.emptyEvalEnvironment filepath evalMap

        -- Build shared context for all functions from this file
        let shared = Jl4.SharedModuleContext
              { Jl4.sharedModule = resolvedModule
              , Jl4.sharedEnvironment = env
              , Jl4.sharedEntityInfo = ei
              , Jl4.sharedModuleContext = moduleContext
              , Jl4.sharedImportEnv = importEnv
              , Jl4.sharedSource = content
              }

        fns <- forM exports $ \export -> do
          let fnDecl = exportToFunction allDeclares implicitParams export
              apiDecl = toDecl fnDecl

          let collisions = validateNoSanitizationCollisions fnDecl.name fnDecl.parameters
          unless (null collisions) $ do
            let collisionMsg = Text.intercalate "\n  " collisions
            logWarn logger "Property name sanitization collision"
              [ ("deploymentId", toJSON deployId)
              , ("function", toJSON fnDecl.name)
              , ("collisions", toJSON collisions)
              ]
            error $ Text.unpack $ "Compilation failed for function '" <> fnDecl.name
              <> "': field name collision after sanitization (spaces and hyphens "
              <> "become indistinguishable in API/MCP schemas):\n  " <> collisionMsg

          let funRawName = NormalName apiDecl.name
          mCompiled <- buildCompiledFromShared shared funRawName
          case mCompiled of
            Right compiled -> do
              let runFn = Jl4.createRunFunctionFromCompiled filepath apiDecl compiled content moduleContext
              pure ValidatedFunction
                { fnImpl = fnDecl
                , fnEvaluator = Map.fromList [(JL4, runFn)]
                , fnCompiled = Just compiled
                , fnSourceText = content
                , fnModuleContext = moduleContext
                , fnDecisionQueryCache = Nothing
                }
            Left _err -> do
              -- Fallback: use original createFunction path (per-file session)
              (runFn, mComp) <- Jl4.createFunction filepath apiDecl content moduleContext
              pure ValidatedFunction
                { fnImpl = fnDecl
                , fnEvaluator = Map.fromList [(JL4, runFn)]
                , fnCompiled = mComp
                , fnSourceText = content
                , fnModuleContext = moduleContext
                , fnDecisionQueryCache = Nothing
                }

        -- Build SerializedBundle from typecheck result for CBOR caching
        let bundle = SerializedBundle
              { sbModule = resolvedModule
              , sbEnvironment = env
              , sbEntityInfo = ei
              , sbExports = exports
              , sbDeclares = allDeclares
              , sbFilePath = filepath
              }

        logInfo logger "Found exports"
          [ ("deploymentId", toJSON deployId)
          , ("file", toJSON filepath)
          , ("exportCount", toJSON (length fns))
          ]
        pure (filepath, Right (fns, [bundle]))

-- | Rebuild ValidatedFunctions from deserialized CBOR bundles,
-- skipping the expensive typecheck step.
--
-- This is the fast restart path: the Module Resolved, Environment, and
-- EntityInfo are loaded from CBOR, and only the evaluator import environment
-- needs to be rebuilt (which requires the sources).
--
-- Uses a single Shake session for all import environment evaluations,
-- sharing work across the import graph.
buildFromCborBundle
  :: Logger
  -> Text  -- ^ deployment ID for logging
  -> [SerializedBundle]
  -> Map FilePath Text       -- ^ source files (for import resolution and eval)
  -> StoredMetadata          -- ^ persisted metadata
  -> IO (Either Text (Map Text ValidatedFunction, DeploymentMetadata))
buildFromCborBundle logger deployId bundles sources storedMeta = do
  let moduleContext = sources
      -- Collect filepaths that need import environments (bundle files with sources)
      bundleFiles = [bundle.sbFilePath | bundle <- bundles, Map.member bundle.sbFilePath sources]

  -- Build all import environments in a single Shake session
  (_errs, _tcMap, evalMap) <- typecheckAndEvalBundle moduleContext bundleFiles

  -- Process each bundle using the shared eval environments
  allFnsWithFile <- fmap concat $ forM bundles $ \bundle -> do
    let resolvedModule = bundle.sbModule
        env = bundle.sbEnvironment
        ei = bundle.sbEntityInfo
        exports = bundle.sbExports
        filepath = bundle.sbFilePath

    case Map.lookup filepath sources of
      Nothing -> pure []
      Just content -> do
        let importEnv = Map.findWithDefault Jl4.emptyEvalEnvironment filepath evalMap

        fns <- forM exports $ \export -> do
          let fnDecl = exportToFunction bundle.sbDeclares [] export
              apiDecl = toDecl fnDecl
              funRawName = NormalName apiDecl.name

          -- Validate that sanitized property names don't collide
          let collisions = validateNoSanitizationCollisions fnDecl.name fnDecl.parameters
          unless (null collisions) $ do
            let collisionMsg = Text.intercalate "\n  " collisions
            logWarn logger "Property name sanitization collision"
              [ ("deploymentId", toJSON deployId)
              , ("function", toJSON fnDecl.name)
              , ("collisions", toJSON collisions)
              ]
            error $ Text.unpack $ "Compilation failed for function '" <> fnDecl.name
              <> "': field name collision after sanitization (spaces and hyphens "
              <> "become indistinguishable in API/MCP schemas):\n  " <> collisionMsg

          -- Find the function definition in the deserialized AST
          mDecide <- runExceptT $ getFunctionDefinition funRawName resolvedModule
          case mDecide of
            Left _err -> pure Nothing
            Right decide -> do
              let compiled = CompiledModule
                    { compiledModule = resolvedModule
                    , compiledEnvironment = env
                    , compiledEntityInfo = ei
                    , compiledDecide = decide
                    , compiledImportEnv = importEnv
                    }

              let runFn = Jl4.createRunFunctionFromCompiled filepath apiDecl compiled content moduleContext

              pure $ Just ValidatedFunction
                { fnImpl = fnDecl
                , fnEvaluator = Map.fromList [(JL4, runFn)]
                , fnCompiled = Just compiled
                , fnSourceText = content
                , fnModuleContext = moduleContext
                , fnDecisionQueryCache = Nothing
                }

        pure [(filepath, fn) | Just fn <- fns]

  let allFns = map snd allFnsWithFile
  if null allFns
    then pure $ Left "No functions rebuilt from CBOR bundles"
    else do
      let fnMap = Map.fromList [(fn.fnImpl.name, fn) | fn <- allFns]

      now <- getCurrentTime
      let summaries = [FunctionSummary { fsName = fn.fnImpl.name, fsDescription = fn.fnImpl.description, fsParameters = fn.fnImpl.parameters, fsReturnType = fn.fnImpl.returnType, fsSection = Nothing, fsIsDeontic = fn.fnImpl.isDeontic, fsSourceFile = Just (Text.pack fp) } | (fp, fn) <- allFnsWithFile]
          version = storedMeta.smVersion
          fileEntries = buildFileEntries deployId sources summaries
          meta = DeploymentMetadata
            { metaFunctions = summaries
            , metaFiles = fileEntries
            , metaVersion = version
            , metaCreatedAt = now
            }

      logInfo logger "Rebuilt functions from CBOR cache"
        [ ("deploymentId", toJSON deployId)
        , ("functionCount", toJSON (length allFns))
        ]
      pure $ Right (fnMap, meta)

-- | Convert an ExportedFunction to our Function type.
exportToFunction :: Map Text (Declare Resolved) -> [(Text, Type' Resolved)] -> ExportedFunction -> Function
exportToFunction declares implicitParams export =
  let baseParams = parametersFromExport declares export.exportParams
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
      -- Detect deontic return type and extract party/action type names
      isDeontic = case export.exportReturnType of
        Just ty -> isDeonticType ty
        Nothing -> False
      (mPartyType, mActionType) = case export.exportReturnType of
        Just ty -> case extractDeonticTypeNames ty of
          Just (p, a) -> (Just p, Just a)
          Nothing -> (Nothing, Nothing)
        Nothing -> (Nothing, Nothing)
      -- Build typed event schema using actual party/action types
      (partyParam, actionParam) = case export.exportReturnType of
        Just (TyApp _ _ [partyTy, actionTy]) ->
          ( (typeToParameter declares Set.empty partyTy) { parameterDescription = "The party performing the action" }
          , (typeToParameter declares Set.empty actionTy) { parameterDescription = "The action performed" }
          )
        _ -> ( Parameter "object" Nothing Nothing [] "The party performing the action" Nothing Nothing Nothing Nothing
             , Parameter "object" Nothing Nothing [] "The action performed" Nothing Nothing Nothing Nothing
             )
      finalParams = if isDeontic
        then mergedParams
          { parameterMap = mergedParams.parameterMap <> Map.fromList
              [ ("startTime", Parameter "number" Nothing Nothing [] "Start time for contract simulation" Nothing Nothing Nothing Nothing)
              , ("events", Parameter
                  { parameterType = "array"
                  , parameterAlias = Nothing
                  , parameterFormat = Nothing
                  , parameterEnum = []
                  , parameterDescription = "Events for contract simulation (each: {party, action, at})"
                  , parameterProperties = Nothing
                  , parameterPropertyOrder = Nothing
                  , parameterItems = Just $ Parameter
                      { parameterType = "object"
                      , parameterAlias = Nothing
                      , parameterFormat = Nothing
                      , parameterEnum = []
                      , parameterDescription = "A trace event"
                      , parameterProperties = Just $ Map.fromList
                          [ ("party", partyParam)
                          , ("action", actionParam)
                          , ("at", Parameter "number" Nothing Nothing [] "Timestamp" Nothing Nothing Nothing Nothing)
                          ]
                      , parameterPropertyOrder = Just ["party", "action", "at"]
                      , parameterItems = Nothing
                      , parameterRequired = Just ["party", "action", "at"]
                      }
                  , parameterRequired = Nothing
                  })
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
    , deonticPartyType = mPartyType
    , deonticActionType = mActionType
    , returnType = returnTypeDisplay export.exportReturnType
    , isDeontic = isDeontic
    }

-- | Build Parameters from ExportedParam list.
parametersFromExport :: Map Text (Declare Resolved) -> [ExportedParam] -> Parameters
parametersFromExport declares params =
  MkParameters
    { parameterMap = Map.fromList [(param.paramName, paramToParameter declares param) | param <- params]
    , required = [param.paramName | param <- params, param.paramRequired]
    }

-- | Convert an ExportedParam to a Parameter.
paramToParameter :: Map Text (Declare Resolved) -> ExportedParam -> Parameter
paramToParameter declares param =
  let p0 = maybe
              (Parameter "object" Nothing Nothing [] "" Nothing Nothing Nothing Nothing)
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

-- | Extract party and action type names from a DEONTIC return type.
-- Given: DEONTIC Party `Contract Action` → Just ("Party", "Contract Action")
extractDeonticTypeNames :: Type' Resolved -> Maybe (Text, Text)
extractDeonticTypeNames ty@(TyApp _ _ [partyTy, actionTy])
  | isDeonticType ty = Just (typeName partyTy, typeName actionTy)
  where
    typeName :: Type' Resolved -> Text
    typeName (TyApp _ n _) = rawNameToText (rawName (getActual n))
    typeName _ = "Unknown"
extractDeonticTypeNames _ = Nothing

-- | Collect all DECLARE entries from a TypeCheckResult and its transitive imports.
collectAllDeclares :: Rules.TypeCheckResult -> Map Text (Declare Resolved)
collectAllDeclares tc =
  declaresFromModule tc.module'
    <> foldMap collectAllDeclares tc.dependencies

-- | Display the return type of an exported function as a user-facing string.
returnTypeDisplay :: Maybe (Type' Resolved) -> Text
returnTypeDisplay Nothing = "unknown"
returnTypeDisplay (Just ty)
  | isDeonticType ty = "DEONTIC"
  | otherwise = case ty of
      TyApp _ n args ->
        let nameText = Text.toUpper (rawNameToText (rawName (getActual n)))
         in if null args
              then nameText
              else nameText <> " " <> Text.intercalate " " (map (returnTypeDisplay . Just) args)
      Type _ -> "TYPE"
      Fun{} -> "FUNCTION"
      Forall _ _ inner -> returnTypeDisplay (Just inner)
      InfVar{} -> "unknown"

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

-- | Build file entries from the source map and function summaries.
-- Every .l4 file gets an entry; exports are populated from summaries that
-- have a matching 'fsSourceFile'.
buildFileEntries :: Text -> Map FilePath Text -> [FunctionSummary] -> [FileEntry]
buildFileEntries deployId sources summaries =
  let l4Paths = [fp | fp <- Map.keys sources, takeExtension fp == ".l4"]
      exportsByFile = Map.fromListWith (++)
        [ (sf, [fs.fsName])
        | fs <- summaries
        , Just sf <- [fs.fsSourceFile]
        ]
  in [ FileEntry
         { fePath = "/deployments/" <> deployId <> "/files/" <> Text.pack fp
         , feExports = maybe [] id (Map.lookup (Text.pack fp) exportsByFile)
         }
     | fp <- sortBy (comparing id) l4Paths
     ]
