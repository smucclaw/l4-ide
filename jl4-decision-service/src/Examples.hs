{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples (functionSpecs, loadL4File, loadL4Functions, ModuleContext) where

import qualified Backend.Jl4 as Jl4
import Backend.Jl4 (ModuleContext, typecheckModule)
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import L4.Export (ExportedFunction (..), ExportedParam (..), getExportedFunctions)
import L4.Syntax
import qualified LSP.L4.Rules as Rules
import Server
import System.Directory (doesFileExist)
import System.FilePath (replaceExtension, takeBaseName, takeDirectory, (</>))
import qualified Data.Set as Set
import qualified Optics

-- ----------------------------------------------------------------------------
-- load example L4 files and descriptions from disk.
-- ----------------------------------------------------------------------------

-- | Extract IMPORT statements from L4 file content
extractImports :: Text -> [Text]
extractImports content =
  [ T.strip $ T.drop 6 line  -- Remove "IMPORT" prefix and trim
  | line <- T.lines content
  , T.strip line /= ""
  , "IMPORT" `T.isPrefixOf` T.strip line
  ]

-- | Recursively discover all files needed (following IMPORT statements)
discoverAllFiles :: Set.Set FilePath -> [FilePath] -> IO (Set.Set FilePath)
discoverAllFiles discovered [] = return discovered
discoverAllFiles discovered (path:paths)
  | path `Set.member` discovered = discoverAllFiles discovered paths
  | otherwise = do
      putStrLn $ "* Discovering dependencies for: " <> path
      fileExists <- doesFileExist path
      if not fileExists
        then do
          putStrLn $ "  WARNING: File not found: " <> path
          discoverAllFiles discovered paths
        else do
          content <- TIO.readFile path
          let imports = extractImports content
              dir = takeDirectory path
              importPaths = [dir </> T.unpack imp <> ".l4" | imp <- imports]
          putStrLn $ "  Found imports: " <> show imports
          let newDiscovered = Set.insert path discovered
          discoverAllFiles newDiscovered (importPaths ++ paths)

loadL4File :: Map.Map FilePath Text -> FilePath -> IO [(FilePath, Text, Text, Function)]
loadL4File moduleContents path = do
  case Map.lookup path moduleContents of
    Nothing -> do
      putStrLn $ "- skipping " <> path <> " (missing source content)"
      pure []
    Just content -> do
      annotationResult <- tryLoadFromAnnotations moduleContents path content
      case annotationResult of
        [] -> loadFromYaml content
        xs -> do
          putStrLn $ "- loaded " <> show (length xs) <> " exported function(s) from annotations in " <> path
          pure [ (path, fnName, content, fnDecl) | (fnName, fnDecl) <- xs ]
 where
  loadFromYaml content = do
    let yamlPath = replaceExtension path ".yaml"
    yamlExists <- doesFileExist yamlPath
    if not yamlExists
      then pure []
      else do
        yamlContent <- TIO.readFile yamlPath
        putStrLn $ "- for " <> path <> " found yaml file " <> yamlPath
        case Yaml.decodeEither' (encodeUtf8 yamlContent) of
          Left err -> do
            putStrLn "YAML decoding error: "
            print err
            pure []
          Right (fnDecl :: Function) -> do
            let fnDeclWithName =
                  if T.null fnDecl.name
                    then fnDecl{name = T.pack $ takeBaseName path}
                    else fnDecl
            print fnDeclWithName
            pure [(path, fnDeclWithName.name, content, fnDeclWithName)]

tryLoadFromAnnotations :: Map.Map FilePath Text -> FilePath -> Text -> IO [(Text, Function)]
tryLoadFromAnnotations moduleContext path content = do
  (errs, mTcRes) <- typecheckModule path content moduleContext
  case mTcRes of
    Nothing -> do
      unless (null errs) $ do
        putStrLn $ "- annotation load failed for " <> path <> " due to typecheck errors:"
        mapM_ (putStrLn . ("    " <>) . T.unpack) errs
      pure []
    Just Rules.TypeCheckResult{module' = resolvedModule} -> do
      let exports = getExportedFunctions resolvedModule
      case exports of
        [] -> pure []
        xs -> do
          let ordered = orderExports xs
          pure [ (export.exportName, exportToFunction resolvedModule export) | export <- ordered ]
  where
    orderExports xs =
      let (defaults, rest) = List.partition (.exportIsDefault) xs
      in defaults <> rest

exportToFunction :: Module Resolved -> ExportedFunction -> Function
exportToFunction resolvedModule export =
  Function
    { name = export.exportName
    , description =
        let desc = T.strip export.exportDescription
        in if T.null desc then "Exported function" else desc
    , parameters = parametersFromExport resolvedModule export.exportParams
    , supportedEvalBackend = [JL4]
    }

parametersFromExport :: Module Resolved -> [ExportedParam] -> Parameters
parametersFromExport resolvedModule params =
  let declares = declaresFromModule resolvedModule
   in
  MkParameters
    { parameterMap = Map.fromList [(param.paramName, paramToParameter declares param) | param <- params]
    , required = [param.paramName | param <- params, param.paramRequired]
    }

paramToParameter :: Map.Map Text (Declare Resolved) -> ExportedParam -> Parameter
paramToParameter declares param =
  let p0 =
        fromMaybe
          (Parameter "object" Nothing [] "" Nothing Nothing Nothing)
          (typeToParameter declares Set.empty <$> param.paramType)
   in
    p0
      { parameterAlias = Nothing
      , parameterDescription = T.strip $ fromMaybe "" param.paramDescription
      }

declaresFromModule :: Module Resolved -> Map.Map Text (Declare Resolved)
declaresFromModule (MkModule _ _ section) =
  Map.fromList (collectSection section)
 where
  collectSection (MkSection _ _ _ decls) =
    decls >>= collectDecl

  collectDecl = \case
    Declare _ decl@(MkDeclare _ _ (MkAppForm _ name _ _) _) ->
      [(resolvedNameText name, decl)]
    Section _ sub ->
      collectSection sub
    _ ->
      []

typeToParameter ::
  Map.Map Text (Declare Resolved) ->
  Set.Set Text ->
  Type' Resolved ->
  Parameter
typeToParameter declares visited ty =
  case ty of
    Type _ -> emptyParam "object"
    TyApp _ name [] ->
      typeNameToParameter name
    TyApp _ name [inner] ->
      let lowered = T.toLower (resolvedNameText name)
       in
        if lowered `elem` ["list", "listof"]
          then
            (emptyParam "array")
              { parameterItems = Just (typeToParameter declares visited inner)
              }
          else
            if lowered `elem` ["maybe", "optional"]
              then typeToParameter declares visited inner
              else typeNameToParameter name
    TyApp _ name _ ->
      typeNameToParameter name
    Fun{} -> emptyParam "object"
    Forall _ _ inner -> typeToParameter declares visited inner
    InfVar{} -> emptyParam "object"
 where
  emptyParam :: Text -> Parameter
  emptyParam t =
    Parameter
      { parameterType = t
      , parameterAlias = Nothing
      , parameterEnum = []
      , parameterDescription = ""
      , parameterProperties = Nothing
      , parameterPropertyOrder = Nothing
      , parameterItems = Nothing
      }

  typeNameToParameter :: Resolved -> Parameter
  typeNameToParameter name =
    case primitiveJsonType name of
      Just t -> emptyParam t
      Nothing ->
        case Map.lookup (resolvedNameText name) declares of
          Nothing -> emptyParam "object"
          Just decl -> declareToParameter (resolvedNameText name) decl

  primitiveJsonType :: Resolved -> Maybe Text
  primitiveJsonType name =
    case T.toLower (resolvedNameText name) of
      "number" -> Just "number"
      "int" -> Just "number"
      "integer" -> Just "number"
      "float" -> Just "number"
      "double" -> Just "number"
      "boolean" -> Just "boolean"
      "bool" -> Just "boolean"
      "string" -> Just "string"
      "text" -> Just "string"
      "date" -> Just "string"
      "datetime" -> Just "string"
      _ -> Nothing

  declareToParameter :: Text -> Declare Resolved -> Parameter
  declareToParameter typeName (MkDeclare declAnn _ _ typeDecl)
    | typeName `Set.member` visited =
        emptyParam "object"
    | otherwise =
        case typeDecl of
          RecordDecl _ _ fields ->
            let
              visited' = Set.insert typeName visited
              fieldOrder = [resolvedNameText fieldName | MkTypedName _ fieldName _ <- fields]
              props =
                Map.fromList
                  [ (resolvedNameText fieldName, addDesc fieldDesc (typeToParameter declares visited' fieldTy))
                  | MkTypedName fieldAnn fieldName fieldTy <- fields
                  , let fieldDesc = fmap getDesc (fieldAnn Optics.^. annDesc)
                  ]
             in
              (emptyParam "object")
                { parameterDescription = fromMaybe "" (fmap getDesc (declAnn Optics.^. annDesc))
                , parameterProperties = Just props
                , parameterPropertyOrder = Just fieldOrder
                }
          EnumDecl _ constructors ->
            (emptyParam "string")
              { parameterDescription = fromMaybe "" (fmap getDesc (declAnn Optics.^. annDesc))
              , parameterEnum = [resolvedNameText c | MkConDecl _ c _ <- constructors]
              }
          SynonymDecl _ inner ->
            typeToParameter declares (Set.insert typeName visited) inner
   where
    addDesc :: Maybe Text -> Parameter -> Parameter
    addDesc Nothing p = p
    addDesc (Just d) p = p {parameterDescription = d}

resolvedNameText :: Resolved -> Text
resolvedNameText =
  rawNameToText . rawName . getActual

loadL4Functions :: [FilePath] -> IO (Map.Map Text ValidatedFunction, ModuleContext)
loadL4Functions paths = do
  when (null paths) $ do
    putStrLn "* to load L4 functions from disk, run with --sourcePaths"
    putStrLn "  for example, --sourcePaths ../doc/tutorial-code/fruit.l4"
    putStrLn "  each .l4 file needs a matching .yaml definition"

  -- Automatically discover all files including imports
  allFiles <- discoverAllFiles Set.empty paths
  let allFilePaths = Set.toList allFiles
  putStrLn $ "* Auto-discovered " <> show (Set.size allFiles) <> " total files (including imports)"

  moduleContents <- Map.fromList <$> forM allFilePaths \fp -> do
    content <- TIO.readFile fp
    pure (fp, content)

  fileGroups <- mapM (loadL4File moduleContents) allFilePaths
  let validFiles = concat fileGroups
  unless (null validFiles) $ putStrLn $ "* Loaded " <> show (length validFiles) <> " exported function(s)"
  let
    moduleContext = moduleContents
  -- Create validated functions (now IO actions)
  validatedFunctions <- forM validFiles $ \(path, name, content, fn) ->
    (name,) <$> createValidatedFunction path name content fn moduleContext
  let functions = Map.fromList validatedFunctions
  return (functions, moduleContext)

createValidatedFunction :: FilePath -> Text -> Text -> Function -> ModuleContext -> IO ValidatedFunction
createValidatedFunction filepath _filename content fnDecl moduleContext = do
  (runFn, mCompiled) <- Jl4.createFunction filepath (toDecl fnDecl) content moduleContext
  pure ValidatedFunction
    { fnImpl = fnDecl
    , fnEvaluator = Map.fromList [(JL4, runFn)]
    , fnCompiled = mCompiled
    , fnSources = Map.fromList [(JL4, content)]
    , fnDecisionQueryCache = Nothing
    }

-- ----------------------------------------------------------------------------
-- Example data, hardcoded
-- ----------------------------------------------------------------------------

functionSpecs :: IO (Map.Map Text ValidatedFunction)
functionSpecs = do
  funcs <- sequence
    [ runExceptT personQualifiesFunction
    , runExceptT rodentsAndVerminFunction
    , runExceptT constantFunction
    ]
  pure $ Map.fromList
    [ (f.fnImpl.name, f)
    | Right f <- funcs
    ]

-- | Metadata about the function that the user might want to know.
-- Further, an LLM could use this info to ask specific questions to the user.
personQualifiesFunction :: ExceptT EvaluatorError IO ValidatedFunction
personQualifiesFunction = do
  let
    fnDecl =
      Function
        { name = "compute_qualifies"
        , description =
            [__i|Determines if a person qualifies for the purposes of the rule.
                  The input object describes the person's properties in the primary parameters: walks, eats, drinks.
                  Secondary parameters can be given which are sufficient to determine some of the primary parameters.
                  A person drinks whether or not they consume an alcoholic or a non-alcoholic beverage, in part or in whole;
                  those specific details don't really matter.
                  The output of the function can be either a request for required information;
                  a restatement of the user input requesting confirmation prior to function calling;
                  or a Boolean answer with optional explanation summary.
                |]
        , parameters =
            MkParameters
              { parameterMap =
                Map.fromList
                    [ ("walks", Parameter "string" Nothing ["true", "false"] "Did the person walk?" Nothing Nothing Nothing)
                    , ("eats", Parameter "string" Nothing ["true", "false"] "Did the person eat?" Nothing Nothing Nothing)
                    , ("drinks", Parameter "string" Nothing ["true", "false"] "Did the person drink?" Nothing Nothing Nothing)
                    ]
              , required = ["walks", "eats", "drinks"]
              }
        , supportedEvalBackend = [JL4]
        }
  (runFn, mCompiled) <- liftIO $ Jl4.createFunction "compute_qualifies.l4" (toDecl fnDecl) computeQualifiesJL4NoInput Map.empty
  pure $
    ValidatedFunction
      { fnImpl = fnDecl
      , fnEvaluator = Map.fromList [(JL4, runFn)]
      , fnCompiled = mCompiled
      , fnSources = Map.fromList [(JL4, computeQualifiesJL4NoInput)]
      , fnDecisionQueryCache = Nothing
      }

-- | Metadata about the function that the user might want to know.
-- Further, an LLM could use this info to ask specific questions to the user.
rodentsAndVerminFunction :: ExceptT EvaluatorError IO ValidatedFunction
rodentsAndVerminFunction = do
  let
    fnDecl =
      Function
        { name = "vermin_and_rodent"
        , description =
            [__i|We do not cover any loss or damage caused by rodents, insects, vermin, or birds.
                  However, this exclusion does not apply to:
                  a) loss or damage to your contents caused by birds; or
                  b) ensuing covered loss unless any other exclusion applies or where an animal causes water to escape from
                    a household appliance, swimming pool or plumbing, heating or air conditioning system
                  |]
        , parameters =
            let
              params =
                Map.fromList
                  [ ("Loss or Damage.caused by insects", Parameter "string" Nothing ["true", "false"] "Was the damage caused by insects?" Nothing Nothing Nothing)
                  , ("Loss or Damage.caused by birds", Parameter "string" Nothing ["true", "false"] "Was the damage caused by birds?" Nothing Nothing Nothing)
                  , ("Loss or Damage.caused by vermin", Parameter "string" Nothing ["true", "false"] "Was the damage caused by vermin?" Nothing Nothing Nothing)
                  , ("Loss or Damage.caused by rodents", Parameter "string" Nothing ["true", "false"] "Was the damage caused by rodents?" Nothing Nothing Nothing)
                  , ("Loss or Damage.to Contents", Parameter "string" Nothing ["true", "false"] "Is the damage to your contents?" Nothing Nothing Nothing)
                  , ("Loss or Damage.ensuing covered loss", Parameter "string" Nothing ["true", "false"] "Is the damage ensuing covered loss" Nothing Nothing Nothing)
                  , ("any other exclusion applies", Parameter "string" Nothing ["true", "false"] "Are any other exclusions besides mentioned ones?" Nothing Nothing Nothing)
                  , ("a household appliance", Parameter "string" Nothing ["true", "false"] "Did water escape from a household appliance due to an animal?" Nothing Nothing Nothing)
                  , ("a swimming pool", Parameter "string" Nothing ["true", "false"] "Did water escape from a swimming pool due to an animal?" Nothing Nothing Nothing)
                  , ("a plumbing, heating, or air conditioning system", Parameter "string" Nothing ["true", "false"] "Did water escape from a plumbing, heating or conditioning system due to an animal?" Nothing Nothing Nothing)
                  ]
            in
              MkParameters
                { parameterMap = params
                , required = Map.keys params
                }
        , supportedEvalBackend = [JL4]
        }
  (runFn, mCompiled) <- liftIO $ Jl4.createFunction "vermin_and_rodent.l4" (toDecl fnDecl) rodentsAndVerminJL4 Map.empty
  pure $
    ValidatedFunction
      { fnImpl = fnDecl
      , fnEvaluator = Map.fromList [(JL4, runFn)]
      , fnCompiled = mCompiled
      , fnSources = Map.fromList [(JL4, rodentsAndVerminJL4)]
      , fnDecisionQueryCache = Nothing
      }

computeQualifiesJL4NoInput :: Text
computeQualifiesJL4NoInput =
  [i|
GIVEN walks  IS A BOOLEAN
      drinks IS A BOOLEAN
      eats   IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE `compute_qualifies` IF
        walks
 AND    drinks
     OR eats
|]

rodentsAndVerminJL4 :: Text
rodentsAndVerminJL4 =
  [i|
DECLARE Inputs
  HAS
    `Loss or Damage.caused by rodents` IS A BOOLEAN
    `Loss or Damage.caused by insects` IS A BOOLEAN
    `Loss or Damage.caused by vermin` IS A BOOLEAN
    `Loss or Damage.caused by birds` IS A BOOLEAN
    `Loss or Damage.to Contents` IS A BOOLEAN
    `any other exclusion applies` IS A BOOLEAN
    `a household appliance` IS A BOOLEAN
    `a swimming pool` IS A BOOLEAN
    `a plumbing, heating, or air conditioning system` IS A BOOLEAN
    `Loss or Damage.ensuing covered loss` IS A BOOLEAN

GIVEN i IS Inputs
GIVETH A BOOLEAN
DECIDE `vermin_and_rodent` i IF
    `not covered if`
         `loss or damage by animals`
     AND NOT               `damage to contents and caused by birds`
                OR         `ensuing covered loss`
                    AND NOT `exclusion apply`
 WHERE
    GIVEN x IS A BOOLEAN
    GIVETH A BOOLEAN
    `not covered if` x MEANS x

    `loss or damage by animals` MEANS
        i's `Loss or Damage.caused by rodents`
     OR i's `Loss or Damage.caused by insects`
     OR i's `Loss or Damage.caused by vermin`
     OR i's `Loss or Damage.caused by birds`

    `damage to contents and caused by birds` MEANS
         i's `Loss or Damage.to Contents`
     AND i's `Loss or Damage.caused by birds`

    `ensuing covered loss` MEANS
        i's `Loss or Damage.ensuing covered loss`

    `exclusion apply` MEANS
        i's `any other exclusion applies`
     OR i's `a household appliance`
     OR i's `a swimming pool`
     OR i's `a plumbing, heating, or air conditioning system`
|]

-- | A zero-parameter constant function for testing
constantFunction :: ExceptT EvaluatorError IO ValidatedFunction
constantFunction = do
  let
    fnDecl =
      Function
        { name = "the_answer"
        , description = "A constant function with no parameters that returns 42"
        , parameters =
            MkParameters
              { parameterMap = Map.empty
              , required = []
              }
        , supportedEvalBackend = [JL4]
        }
  (runFn, mCompiled) <- liftIO $ Jl4.createFunction "the_answer.l4" (toDecl fnDecl) constantJL4 Map.empty
  pure $
    ValidatedFunction
      { fnImpl = fnDecl
      , fnEvaluator = Map.fromList [(JL4, runFn)]
      , fnCompiled = mCompiled
      , fnSources = Map.fromList [(JL4, constantJL4)]
      , fnDecisionQueryCache = Nothing
      }

constantJL4 :: Text
constantJL4 =
  [i|
GIVETH A NUMBER
DECIDE the_answer IS 42
|]
