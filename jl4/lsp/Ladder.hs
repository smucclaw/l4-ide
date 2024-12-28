{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Ladder where

import L4.Syntax as S

import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics

data AndOrTag = All | Any | Leaf
  deriving stock (Show, Ord, Eq, Generic)
  deriving anyclass (ToJSON)

data ShouldView = Ask | View
  deriving stock (Show, Ord, Eq, Generic)
  deriving anyclass (ToJSON)

data AndOrNode = AndOrNode
  { tag :: AndOrTag
  , children :: Maybe [RuleNode]
  , contents :: Maybe Text
  }
  deriving stock (Show, Ord, Eq, Generic)
  deriving anyclass (ToJSON)

data RuleNode = RuleNode
  { andOr :: AndOrNode
  , prePost :: Map Text Text
  , mark :: Mark
  , shouldView :: ShouldView
  }
  deriving (Show, Ord, Eq, Generic)
  deriving anyclass (ToJSON)

data Mark = Mark
  { value :: Text
  , source :: Text
  }
  deriving (Show, Ord, Eq, Generic)
  deriving anyclass (ToJSON)

undefinedUserMark :: Mark
undefinedUserMark =
  Mark
    { value = "undefined"
    , source = "user"
    }

-- | Generate boolean circuits of the 'Program'.
--
-- Generates one diagram for each 'Decide' statement.
visualise :: Program Name -> [RuleNode]
visualise prog =
  concatMap decideToRuleNode decides
 where
  decides =
    toListOf (gplate @(Decide Name)) prog

decideToRuleNode :: Decide Name -> [RuleNode]
decideToRuleNode (MkDecide _ sig appForm expr) =
  []
  -- Maybe.mapMaybe (clauseToRuleNode sig) clauses

-- | Turn a single clause into a boolean circuit.
-- We support only a tiny subset of possible representations, in particular
-- only statements of the form:
--
-- @
--   GIVEN <variable>
--   DECIDE <variable> (IS|IF) <boolean expression>
-- @
--
-- Moreover, only 'AND', 'OR' and variables are allowed in the '<boolean expression>'.
--
-- These limitations are arbitrary, mostly to make sure we have something to show rather
-- than being complete. So, feel free to lift these limitations at your convenience :)
{-
clauseToRuleNode :: TypeSig Name -> Clause Name -> Maybe RuleNode
clauseToRuleNode (MkTypeSig _ givenSig _) (GuardedClause _ e guard_) =
  case givenSig of
    MkGivenSig _ [MkTypedName _ (Name _ subject) _] ->
      case e of
        Var _ (Name _ what) -> case guard_ of
          PlainGuard _ g -> do
            rNode <- traverseExpr subject g
            pure
              rNode
                { prePost = Map.fromList [("Pre", "Does the " <> subject <> " " <> what <> "?")]
                }
          _ ->
            Nothing
        _ -> Nothing
    _ -> Nothing
-}

traverseExpr :: Text -> Expr Name -> Maybe RuleNode
traverseExpr subject e = case e of
  And{} ->
    Just $
      ruleNode andPrePost $
        node All [rNode | n <- scanAnd e, Just rNode <- [traverseExpr subject n]]
  Or{} ->
    Just $
      ruleNode orPrePost $
        node Any [rNode | n <- scanOr e, Just rNode <- [traverseExpr subject n]]
  Var _ (Name _ verb) ->
    Just $ ruleNode emptyPrePost $ leaf subject verb
  Is{} -> Nothing -- Can't handle 'Is' yet
  Not{} -> Nothing -- Can't handle 'Not' yet
  Proj{} -> Nothing -- Can't handle 'Proj' yet

andPrePost :: Map Text Text
andPrePost = Map.fromList [("Pre", "all of:")]

orPrePost :: Map Text Text
orPrePost = Map.fromList [("Pre", "any of:")]

emptyPrePost :: Map Text Text
emptyPrePost = mempty

leaf :: Text -> Text -> AndOrNode
leaf subject t =
  AndOrNode
    { tag = Leaf
    , children = Nothing
    , contents = Just $ "does the " <> subject <> " " <> t <> "?"
    }

node :: AndOrTag -> [RuleNode] -> AndOrNode
node tag chs =
  AndOrNode
    { tag = tag
    , children = Just chs
    , contents = Nothing
    }

ruleNode :: Map Text Text -> AndOrNode -> RuleNode
ruleNode prePost n =
  RuleNode
    { andOr = n
    , prePost = prePost
    , mark = undefinedUserMark
    , shouldView = View
    }

scanAnd :: Expr Name -> [Expr Name]
scanAnd (And _ e1 e2) =
  scanAnd e1 <> scanAnd e2
scanAnd e = [e]

scanOr :: Expr Name -> [Expr Name]
scanOr (Or _ e1 e2) =
  scanOr e1 <> scanOr e2
scanOr e = [e]
