{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SchemaSpec (spec) where

import Test.Hspec
import Test.QuickCheck (Arbitrary (..))
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Instances ()

import Backend.Api
import Backend.DecisionQueryPlan (QueryAtom (..), QueryOutcome (..), QueryImpact (..), QueryInput (..), QueryAsk (..), QueryPlanResponse (..))
import Backend.FunctionSchema (Parameters (..), Parameter (..))
import ControlPlane (DeploymentStatusResponse (..))
import qualified Data.Map as Map
import qualified Data.Text as Text
import Schema ()
import Servant.API (FromHttpApiData (..))
import Types
import qualified Test.Hspec.QuickCheck as Hspec
import Test.QuickCheck.Property
import Data.Time (UTCTime (..))
import Data.Time.Calendar (fromGregorian)

spec :: Spec
spec = do
  describe "Schema" do
    describe "Param Schema" do
      describe "FnLiteral" do
        Hspec.prop "Int" $ \(n :: Integer) ->
          parseQueryParam (Text.pack $ show n) === Right (FnLitInt n)
        Hspec.prop "Bool" $ \(n :: Bool) ->
          parseQueryParam (Text.pack $ show n) === Right (FnLitBool n)

-- ----------------------------------------------------------------------------
-- Arbitrary instances
-- ----------------------------------------------------------------------------

instance Arbitrary FnLiteral where
  arbitrary =
    Q.frequency
      [ (1, FnLitBool <$> arbitrary)
      , (4, FnLitDouble <$> arbitrary)
      , (4, FnLitString <$> arbitrary)
      , (4, FnLitInt <$> arbitrary)
      , (1, pure FnUnknown)
      , (1, pure FnUncertain)
      ]

instance Arbitrary Reasoning where
  arbitrary = Reasoning <$> arbitrary

instance Arbitrary ReasonNode where
  arbitrary = ReasonNode <$> arbitrary <*> arbitrary

instance Arbitrary ReasoningTree where
  arbitrary = Q.sized $ \n -> do
    k <- Q.chooseInt (0, n)
    go k
   where
    go n = do
      node <- arbitrary
      pars <- arbPartition (n - 1)
      forest <- mapM go pars
      return $
        ReasoningTree
          { payload = node
          , children = forest
          }

    arbPartition :: Int -> Q.Gen [Int]
    arbPartition k = case compare k 1 of
      LT -> pure []
      EQ -> pure [1]
      GT -> do
        first <- Q.chooseInt (1, k)
        rest <- arbPartition $ k - first
        Q.shuffle (first : rest)

instance Arbitrary ResponseWithReason where
  arbitrary = ResponseWithReason <$> arbitrary <*> pure emptyTree <*> pure Nothing

instance Arbitrary EvaluatorError where
  arbitrary = Q.oneof [InterpreterError <$> arbitrary]

instance Arbitrary SimpleResponse where
  arbitrary =
    Q.oneof
      [ SimpleResponse <$> arbitrary
      , SimpleError <$> arbitrary
      ]

instance Arbitrary Parameters where
  arbitrary = do
    params <- arbitrary
    pure $ MkParameters params (Map.keys params)

instance Arbitrary Parameter where
  arbitrary = Q.sized $ \n ->
    if n <= 0
      then Parameter <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> pure Nothing <*> pure Nothing <*> pure Nothing
      else
        Parameter
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> Q.resize (n `div` 4) arbitrary
          <*> Q.resize (n `div` 4) arbitrary
          <*> Q.resize (n `div` 4) arbitrary

instance Arbitrary Function where
  arbitrary = Types.Function <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary EvalBackend where
  arbitrary = Q.chooseEnum (minBound, maxBound)

instance Arbitrary TraceEvent where
  arbitrary = TraceEvent <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FnArguments where
  arbitrary = FnArguments <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SimpleFunction where
  arbitrary = SimpleFunction <$> arbitrary <*> arbitrary

instance Arbitrary OutcomeStyle where
  arbitrary = Q.chooseEnum (ValueOnly, BaseAttributes)

instance Arbitrary OutcomeObject where
  arbitrary =
    OutcomeObject
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary BatchRequest where
  arbitrary =
    BatchRequest
      <$> arbitrary
      <*> arbitrary

instance Arbitrary BatchResponse where
  arbitrary =
    BatchResponse
      <$> arbitrary
      <*> arbitrary

instance Arbitrary Outcomes where
  arbitrary = Q.oneof [OutcomeAttribute <$> arbitrary, OutcomePropertyObject <$> arbitrary]

instance Arbitrary InputCase where
  arbitrary = InputCase <$> arbitrary <*> arbitrary

instance Arbitrary OutputCase where
  arbitrary =
    OutputCase
      <$> arbitrary
      <*> arbitrary
      <*> pure Nothing  -- Exclude GraphViz from QuickCheck

instance Arbitrary OutputSummary where
  arbitrary =
    OutputSummary
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary QueryAtom where
  arbitrary = QueryAtom <$> arbitrary <*> arbitrary <*> arbitrary <*> pure []

instance Arbitrary QueryOutcome where
  arbitrary = QueryOutcome <$> arbitrary <*> arbitrary

instance Arbitrary QueryImpact where
  arbitrary = QueryImpact <$> arbitrary <*> arbitrary

instance Arbitrary QueryInput where
  arbitrary =
    QueryInput
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary QueryAsk where
  arbitrary =
    QueryAsk
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary QueryPlanResponse where
  arbitrary =
    QueryPlanResponse
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure Nothing

instance Arbitrary StateGraphInfo where
  arbitrary = StateGraphInfo <$> arbitrary <*> arbitrary

instance Arbitrary StateGraphListResponse where
  arbitrary = StateGraphListResponse <$> arbitrary

instance Arbitrary DeploymentMetadata where
  arbitrary = DeploymentMetadata
    <$> arbitrary
    <*> arbitrary
    <*> pure (UTCTime (fromGregorian 2025 1 1) 0)

instance Arbitrary FunctionSummary where
  arbitrary = FunctionSummary <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DeploymentStatusResponse where
  arbitrary = DeploymentStatusResponse
    <$> arbitrary
    <*> Q.elements ["compiling", "ready", "failed"]
    <*> arbitrary
    <*> arbitrary
