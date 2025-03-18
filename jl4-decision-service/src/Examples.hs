{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples (functionSpecs, loadL4File, loadL4Functions) where

import Backend.Jl4 as Jl4
import Control.Monad.Trans.Except
import Control.Monad (when, unless)
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import Server
import System.FilePath (replaceExtension, takeBaseName)
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes)
-- ----------------------------------------------------------------------------
-- load example L4 files and descriptions from disk.
-- ----------------------------------------------------------------------------

loadL4File :: FilePath -> IO (Maybe (Text, Text, Function))
loadL4File path = do
  let yamlPath = replaceExtension path ".yaml"
  yamlExists <- doesFileExist yamlPath
  if not yamlExists
    then return Nothing
    else do
      content <- TIO.readFile path
      yamlContent <- TIO.readFile yamlPath
      putStrLn $ "- for " <> path <> " found yaml file " <> yamlPath
      case Yaml.decodeEither' (encodeUtf8 yamlContent) of
        Left err -> do
          putStrLn "YAML decoding error: "
          print err
          return Nothing
        Right (fnDecl :: Function) -> do
          let fnDeclWithName = if T.null (fnDecl.name) then fnDecl { name = T.pack $ takeBaseName path } else fnDecl
          print fnDeclWithName
          return (Just (fnDecl.name, content, fnDeclWithName))

loadL4Functions :: [FilePath] -> IO (Map.Map Text ValidatedFunction)
loadL4Functions paths = do
  files <- mapM loadL4File paths
  when (null paths) $ do
     putStrLn "* to load L4 functions from disk, run with --sourcePaths"
     putStrLn "  for example, --sourcePaths ../doc/tutorial-code/fruit.l4"
     putStrLn "  each .l4 file needs a matching .yaml definition"
  unless (null files) $ putStrLn $ "* Loaded " <> show (length files) <> " .l4 files"
  let
    validFiles = catMaybes files
    functions = Map.fromList [(name, createValidatedFunction name content fn) | (name, content, fn) <- validFiles]
  return functions



createValidatedFunction :: Text -> Text -> Function -> ValidatedFunction
createValidatedFunction _filename content fnDecl =
  ValidatedFunction
    { fnImpl = fnDecl
    , fnEvaluator =
        Map.fromList
          [ (JL4, builtinProgram $ Jl4.createFunction (toDecl fnDecl) content)
          ]
    }

-- ----------------------------------------------------------------------------
-- Example data, hardcoded
-- ----------------------------------------------------------------------------

functionSpecs :: Map.Map Text ValidatedFunction
functionSpecs =
  Map.fromList
    [ (f.fnImpl.name, f)
    | f <-
        [ builtinProgram personQualifiesFunction
        , builtinProgram rodentsAndVerminFunction
        ]
    ]

-- | Metadata about the function that the user might want to know.
-- Further, an LLM could use this info to ask specific questions to the user.
personQualifiesFunction :: Except EvaluatorError ValidatedFunction
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
            Parameters $
              Map.fromList
                [ ("walks", Parameter "string" Nothing ["true", "false"] "Did the person walk?")
                , ("eats", Parameter "string" Nothing ["true", "false"] "Did the person eat?")
                , ("drinks", Parameter "string" Nothing ["true", "false"] "Did the person drink?")
                ]
        , supportedEvalBackend = []
        }
  pure $
    ValidatedFunction
      { fnImpl = fnDecl
      , fnEvaluator =
          Map.fromList
            [ (JL4, builtinProgram $ Jl4.createFunction (toDecl fnDecl) computeQualifiesJL4NoInput)
            ]
      }

-- | Metadata about the function that the user might want to know.
-- Further, an LLM could use this info to ask specific questions to the user.
rodentsAndVerminFunction :: Except EvaluatorError ValidatedFunction
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
            Parameters $
              Map.fromList
                [ ("Loss or Damage.caused by insects", Parameter "string" Nothing ["true", "false"] "Was the damage caused by insects?")
                , ("Loss or Damage.caused by birds", Parameter "string" Nothing ["true", "false"] "Was the damage caused by birds?")
                , ("Loss or Damage.caused by vermin", Parameter "string" Nothing ["true", "false"] "Was the damage caused by vermin?")
                , ("Loss or Damage.caused by rodents", Parameter "string" Nothing ["true", "false"] "Was the damage caused by rodents?")
                , ("Loss or Damage.to Contents", Parameter "string" Nothing ["true", "false"] "Is the damage to your contents?")
                , ("Loss or Damage.ensuing covered loss", Parameter "string" Nothing ["true", "false"] "Is the damage ensuing covered loss")
                , ("any other exclusion applies", Parameter "string" Nothing ["true", "false"] "Are any other exclusions besides mentioned ones?")
                , ("a household appliance", Parameter "string" Nothing ["true", "false"] "Did water escape from a household appliance due to an animal?")
                , ("a swimming pool", Parameter "string" Nothing ["true", "false"] "Did water escape from a swimming pool due to an animal?")
                , ("a plumbing, heating, or air conditioning system", Parameter "string" Nothing ["true", "false"] "Did water escape from a plumbing, heating or conditioning system due to an animal?")
                ]
        , supportedEvalBackend = []
        }
  pure $
    ValidatedFunction
      { fnImpl = fnDecl
      , fnEvaluator =
          Map.fromList
            [ (JL4, builtinProgram $ Jl4.createFunction (toDecl fnDecl) rodentsAndVerminJL4)
            ]
      }

-- computeQualifiesJL4 :: Text
-- computeQualifiesJL4 =
--   [i|
-- DECLARE Inputs
--   HAS
--     walks IS A BOOLEAN
--     drinks IS A BOOLEAN
--     eats IS A BOOLEAN

-- GIVEN i IS Inputs
-- GIVETH A BOOLEAN
-- DECIDE `compute_qualifies` i IF
--         i's walks
--  AND    i's drinks
--      OR i's eats
-- |]

-- [TODO]: this would be the preferred calling style, but we get L4 ERror: More than ONE #EVAL found
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
    `not covered if` MEANS GIVEN x YIELD x

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

builtinProgram :: Except EvaluatorError a -> a
builtinProgram m = case runExcept m of
  Left err -> error $ "Builtin failed to load " <> show err
  Right e -> e
