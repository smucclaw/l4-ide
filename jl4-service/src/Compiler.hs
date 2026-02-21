module Compiler (
  compileBundle,
  computeVersion,
  toDecl,
) where

import qualified Backend.Jl4 as Jl4
import Backend.Jl4 (ModuleContext, typecheckModule)
import Backend.Api (EvalBackend (..), FunctionDeclaration (..))
import Backend.FunctionSchema (Parameters (..), Parameter (..), typeToParameter, declaresFromModule)
import Types
import Control.Monad (forM, unless)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
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
import L4.Syntax (Module, Resolved, Declare(..), Type'(..))
import qualified LSP.L4.Rules as Rules
import System.FilePath (takeExtension)

-- | Compile an in-memory source map into a deployment-ready function registry.
--
-- Steps:
-- 1. Identify all .l4 files that may contain exported functions
-- 2. Build ModuleContext from all sources
-- 3. For each file, typecheck and discover @export annotations
-- 4. For each export, create a RunFunction + CompiledModule
-- 5. Build metadata with SHA-256 version
compileBundle
  :: Map FilePath Text
  -> IO (Either Text (Map Text ValidatedFunction, DeploymentMetadata))
compileBundle sources = do
  let l4Files = Map.toList $ Map.filterWithKey (\k _ -> takeExtension k == ".l4") sources
  if null l4Files
    then pure $ Left "No .l4 files found in bundle"
    else do
      -- Build module context from all sources (for import resolution)
      let moduleContext = sources

      -- Try each file for exported functions
      results <- forM l4Files $ \(filepath, content) ->
        compileSingleFile filepath content moduleContext

      let allFunctions = concatMap (either (const []) id) results
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
            putStrLn $ "  Compiled " <> show (length allFunctions) <> " function(s)"

          pure $ Right (fnMap, meta)

-- | Compile a single .l4 file's exported functions.
compileSingleFile
  :: FilePath
  -> Text
  -> ModuleContext
  -> IO (Either Text [ValidatedFunction])
compileSingleFile filepath content moduleContext = do
  -- Typecheck the module
  (errs, mTcRes) <- typecheckModule filepath content moduleContext
  case mTcRes of
    Nothing -> do
      unless (null errs) $
        putStrLn $ "  Typecheck failed for " <> filepath <> ": " <> show errs
      pure $ Left (Text.intercalate "\n" errs)

    Just Rules.TypeCheckResult{module' = resolvedModule, errors = tcErrors} -> do
      -- Discover exported functions
      let exports = getExportedFunctions resolvedModule
      if null exports
        then pure $ Right []
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

          putStrLn $ "  Found " <> show (length fns) <> " export(s) in " <> filepath
          pure $ Right fns

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
  in Function
    { name = export.exportName
    , description =
        let desc = Text.strip export.exportDescription
        in if Text.null desc then "Exported function" else desc
    , parameters = mergedParams
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
              (Parameter "object" Nothing [] "" Nothing Nothing Nothing)
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
