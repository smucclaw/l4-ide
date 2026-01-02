{-# LANGUAGE OverloadedStrings #-}

module AskOrderingSpec (spec) where

import Backend.DecisionQueryPlan (QueryAsk (..), orderAsksForElicitation)
import Backend.FunctionSchema (Parameter (..), Parameters (..))
import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

spec :: Spec
spec = do
  describe "orderAsksForElicitation" do
    it "orders array indices numerically (list-of-records)" do
      let
        recordItem =
          Parameter
            { parameterType = "object"
            , parameterAlias = Nothing
            , parameterEnum = []
            , parameterDescription = ""
            , parameterProperties =
                Just $
                  Map.fromList
                    [ ("recordNumber", pNumber)
                    , ("recordLabel", pString)
                    ]
            , parameterPropertyOrder = Just ["recordNumber", "recordLabel"]
            , parameterItems = Nothing
            }
        listOfRecords =
          Parameter
            { parameterType = "array"
            , parameterAlias = Nothing
            , parameterEnum = []
            , parameterDescription = ""
            , parameterProperties = Nothing
            , parameterPropertyOrder = Nothing
            , parameterItems = Just recordItem
            }
        recordOfLists =
          Parameter
            { parameterType = "object"
            , parameterAlias = Nothing
            , parameterEnum = []
            , parameterDescription = ""
            , parameterProperties =
                Just $
                  Map.fromList
                    [ ("people", listOfRecords)
                    , ("wrappers", listOfRecords)
                    ]
            , parameterPropertyOrder = Just ["people", "wrappers"]
            , parameterItems = Nothing
            }
        params =
          MkParameters
            { parameterMap = Map.singleton "rol" recordOfLists
            , required = ["rol"]
            }

        ask score0 path0 =
          QueryAsk
            { container = "rol"
            , key = Just (Text.intercalate "." path0)
            , path = path0
            , label = "rol." <> Text.intercalate "." path0
            , score = score0
            , atoms = []
            , schema = Nothing
            }

        asks0 =
          [ ask 10 ["people", "10", "recordLabel"]
          , ask 10 ["people", "2", "recordLabel"]
          , ask 10 ["people", "2", "recordNumber"]
          , ask 10 ["people", "10", "recordNumber"]
          , ask 10 ["wrappers", "1", "recordLabel"]
          ]

        ordered = orderAsksForElicitation params asks0

      fmap (.path) ordered
        `shouldBe` [ ["people", "2", "recordNumber"]
                  , ["people", "2", "recordLabel"]
                  , ["people", "10", "recordNumber"]
                  , ["people", "10", "recordLabel"]
                  , ["wrappers", "1", "recordLabel"]
                  ]

    it "uses schema-aware longest-match for dotted property keys" do
      let
        root =
          Parameter
            { parameterType = "object"
            , parameterAlias = Nothing
            , parameterEnum = []
            , parameterDescription = ""
            , parameterProperties =
                Just $
                  Map.fromList
                    [ ("a.b", pBool)
                    , ("x", pBool)
                    ]
            , parameterPropertyOrder = Just ["a.b", "x"]
            , parameterItems = Nothing
            }
        params =
          MkParameters
            { parameterMap = Map.singleton "i" root
            , required = ["i"]
            }

        ask path0 =
          QueryAsk
            { container = "i"
            , key = Just (Text.intercalate "." path0)
            , path = path0
            , label = "i." <> Text.intercalate "." path0
            , score = 1
            , atoms = []
            , schema = Nothing
            }

        asks0 =
          [ ask ["x"]
          , ask ["a", "b"]
          ]

      fmap (.path) (orderAsksForElicitation params asks0) `shouldBe` [["a", "b"], ["x"]]

pBool :: Parameter
pBool =
  Parameter
    { parameterType = "boolean"
    , parameterAlias = Nothing
    , parameterEnum = []
    , parameterDescription = ""
    , parameterProperties = Nothing
    , parameterPropertyOrder = Nothing
    , parameterItems = Nothing
    }

pNumber :: Parameter
pNumber = pBool {parameterType = "number"}

pString :: Parameter
pString = pBool {parameterType = "string"}
