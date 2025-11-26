{-# LANGUAGE QuasiQuotes #-}

module TestData (
  rodentAndVerminFunctionSpec,
  rodentAndVerminFunction,
  rodentAndVerminJL4,
) where

import Backend.Jl4 as Jl4
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import Data.Text (Text)
import Server

rodentAndVerminFunctionSpec :: ValidatedFunction
rodentAndVerminFunctionSpec = builtinProgram rodentAndVerminFunction

-- | Metadata about the function that the user might want to know.
-- Further, an LLM could use this info to ask specific questions to the user.
rodentAndVerminFunction :: Except EvaluatorError ValidatedFunction
rodentAndVerminFunction = do
  let
    fnDecl =
      Function
        { name = "vermin_and_rodent"
        , description = "Example description"
        , parameters =
            let
              params =
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
            in
              MkParameters
                { parameterMap = params
                , required = Map.keys params
                }
        , supportedEvalBackend = [JL4]
        }
  pure $
    ValidatedFunction
      { fnImpl = fnDecl
      , fnEvaluator =
          Map.fromList
            [ (JL4, Jl4.createFunction "vermin_and_rodent.l4" (toDecl fnDecl) rodentAndVerminJL4 Map.empty)
            ]
      }

rodentAndVerminJL4 :: Text
rodentAndVerminJL4 =
  [i|
GIVEN
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
GIVETH A BOOLEAN
DECIDE `vermin_and_rodent` IF
    `not covered if`
         `loss or damage by animals`
     AND NOT               `damage to contents and caused by birds`
                OR         `ensuing covered loss`
                    AND NOT `exclusion apply`
 WHERE
    `not covered if` MEANS GIVEN x YIELD x

    `loss or damage by animals` MEANS
        `Loss or Damage.caused by rodents`
     OR `Loss or Damage.caused by insects`
     OR `Loss or Damage.caused by vermin`
     OR `Loss or Damage.caused by birds`

    `damage to contents and caused by birds` MEANS
         `Loss or Damage.to Contents`
     AND `Loss or Damage.caused by birds`

    `ensuing covered loss` MEANS
        `Loss or Damage.ensuing covered loss`

    `exclusion apply` MEANS
        `any other exclusion applies`
     OR `a household appliance`
     OR `a swimming pool`
     OR `a plumbing, heating, or air conditioning system`
|]

builtinProgram :: Except EvaluatorError a -> a
builtinProgram m = case runExcept m of
  Left err -> error $ "Builtin failed to load " <> show err
  Right e -> e
