{-# LANGUAGE QuasiQuotes #-}

module TestData (
  rodentAndVerminFunctionSpec,
  rodentAndVerminFunction,
  rodentAndVerminJL4,
  constantFunctionSpec,
  constantFunction,
  constantJL4,
  qualifiesJL4,
  recordJL4,
  maybeParamJL4,
  saleContractJL4,
  deonticExportJL4,
  deonticRecordPartyJL4,
) where

import Backend.Jl4 as Jl4
import Backend.Api
import Compiler (toDecl)
import Types
import Backend.FunctionSchema (Parameters (..), Parameter (..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import Data.Text (Text)

rodentAndVerminFunctionSpec :: IO ValidatedFunction
rodentAndVerminFunctionSpec = either (error . show) id <$> runExceptT rodentAndVerminFunction

rodentAndVerminFunction :: ExceptT EvaluatorError IO ValidatedFunction
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
                  [ ("Loss or Damage.caused by insects", Parameter "string" Nothing Nothing ["true", "false"] "Was the damage caused by insects?" Nothing Nothing Nothing)
                  , ("Loss or Damage.caused by birds", Parameter "string" Nothing Nothing ["true", "false"] "Was the damage caused by birds?" Nothing Nothing Nothing)
                  , ("Loss or Damage.caused by vermin", Parameter "string" Nothing Nothing ["true", "false"] "Was the damage caused by vermin?" Nothing Nothing Nothing)
                  , ("Loss or Damage.caused by rodents", Parameter "string" Nothing Nothing ["true", "false"] "Was the damage caused by rodents?" Nothing Nothing Nothing)
                  , ("Loss or Damage.to Contents", Parameter "string" Nothing Nothing ["true", "false"] "Is the damage to your contents?" Nothing Nothing Nothing)
                  , ("Loss or Damage.ensuing covered loss", Parameter "string" Nothing Nothing ["true", "false"] "Is the damage ensuing covered loss" Nothing Nothing Nothing)
                  , ("any other exclusion applies", Parameter "string" Nothing Nothing ["true", "false"] "Are any other exclusions besides mentioned ones?" Nothing Nothing Nothing)
                  , ("a household appliance", Parameter "string" Nothing Nothing ["true", "false"] "Did water escape from a household appliance due to an animal?" Nothing Nothing Nothing)
                  , ("a swimming pool", Parameter "string" Nothing Nothing ["true", "false"] "Did water escape from a swimming pool due to an animal?" Nothing Nothing Nothing)
                  , ("a plumbing, heating, or air conditioning system", Parameter "string" Nothing Nothing ["true", "false"] "Did water escape from a plumbing, heating or conditioning system due to an animal?" Nothing Nothing Nothing)
                  ]
            in
              MkParameters
                { parameterMap = params
                , required = Map.keys params
                }
        , supportedEvalBackend = [JL4]
        , deonticPartyType = Nothing
        , deonticActionType = Nothing
        }
  (runFn, mCompiled) <- liftIO $ Jl4.createFunction "vermin_and_rodent.l4" (toDecl fnDecl) rodentAndVerminJL4 Map.empty
  pure $
    ValidatedFunction
      { fnImpl = fnDecl
      , fnEvaluator = Map.fromList [(JL4, runFn)]
      , fnCompiled = mCompiled
      , fnSources = Map.fromList [(JL4, rodentAndVerminJL4)]
      , fnDecisionQueryCache = Nothing
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

constantFunctionSpec :: IO ValidatedFunction
constantFunctionSpec = either (error . show) id <$> runExceptT constantFunction

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
        , deonticPartyType = Nothing
        , deonticActionType = Nothing
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

-- | L4 source for the "compute_qualifies" function used in integration tests.
qualifiesJL4 :: Text
qualifiesJL4 =
  [i|
@export default person qualifies
GIVEN walks IS A BOOLEAN
      eats  IS A BOOLEAN
      drinks IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE compute_qualifies IF walks AND eats AND drinks
|]

-- | L4 source returning a record type, for testing named fields in JSON output.
recordJL4 :: Text
recordJL4 =
  [i|
DECLARE Person HAS
    name IS A STRING
    age IS A NUMBER

GIVEN n IS A STRING
      a IS A NUMBER
GIVETH A Person
DECIDE make_person IS Person WITH name IS n, age IS a
|]

-- | L4 source with a MAYBE parameter, for testing null/NOTHING handling.
maybeParamJL4 :: Text
maybeParamJL4 =
  [i|
IMPORT prelude

DECLARE Result HAS
    label IS A STRING
    extra_provided IS A BOOLEAN

GIVEN label IS A STRING
      extra IS A MAYBE STRING
GIVETH A Result
DECIDE with_maybe IS Result WITH
    label IS label
    extra_provided IS
        CONSIDER extra
            WHEN JUST x THEN TRUE
            WHEN NOTHING THEN FALSE
|]

-- | L4 source with an exported boolean function and a regulative (sale contract) rule.
-- Used for testing state graph extraction and DOT output.
saleContractJL4 :: Text
saleContractJL4 =
  [i|
DECLARE Party IS ONE OF `the seller`, `the buyer`
DECLARE `Contract Action` IS ONE OF `deliver the goods`, `pay the invoice`

@export default test function
GIVEN walks IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE test_fn IF walks

GIVETH A DEONTIC Party `Contract Action`
`the sale contract` MEANS
    PARTY `the seller`
    MUST `deliver the goods`
    WITHIN 14
    HENCE
        PARTY `the buyer`
        MUST `pay the invoice`
        WITHIN 30
        HENCE FULFILLED
        LEST BREACH
    LEST BREACH
|]

-- | L4 source with an exported deontic function for testing contract simulation.
-- The sale contract has no GIVEN parameters — it's a standalone deontic rule.
deonticExportJL4 :: Text
deonticExportJL4 =
  [i|
DECLARE Party IS ONE OF `the seller`, `the buyer`
DECLARE `Contract Action` IS ONE OF `deliver the goods`, `pay the invoice`

@export default sale contract
GIVETH A DEONTIC Party `Contract Action`
`the sale contract` MEANS
    PARTY `the seller`
    MUST `deliver the goods`
    WITHIN 14
    HENCE
        PARTY `the buyer`
        MUST `pay the invoice`
        WITHIN 30
        HENCE FULFILLED
        LEST BREACH
    LEST BREACH
|]

-- | L4 source with a deontic function that uses record-typed parties.
-- Tests that events with JSON object parties (e.g. {"name": "Alice"})
-- are correctly formatted as L4 record construction (Driver WITH name IS "Alice")
-- rather than backtick-quoted identifiers.
deonticRecordPartyJL4 :: Text
deonticRecordPartyJL4 =
  [i|
DECLARE Car HAS
    `number of wheels` IS A NUMBER

DECLARE Driver HAS
    name IS A STRING

DECLARE `Driver Action` IS ONE OF
    `wear seatbelt`
    `drive`

@export default seatbelt requirement
GIVEN car    IS A Car
      driver IS A Driver
GIVETH A PROVISION OF Driver, `Driver Action`
`Seatbelt Requirement` MEANS
    IF      car's `number of wheels` EQUALS 4
    THEN    PARTY driver
            MUST `wear seatbelt`
            WITHIN 1
            HENCE
                PARTY driver
                MAY `drive`
    ELSE    PARTY driver
            MAY `drive`
|]
