{-# LANGUAGE DataKinds #-}

module LSP.L4.Ladder where

import L4.Syntax as S
import qualified L4.ExactPrint as L4
import qualified L4.Annotation as L4
import qualified L4.Lexer as L4

import Data.Aeson
import Data.List.NonEmpty (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics

------- Imports for testing -------------
import Text.Pretty.Simple
import L4.Parser
import L4.TypeCheck (rawNameToText)
import qualified Data.Either.Extra as Either
-----------------------------------------

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

----------------------------
  -- Entrypoint: Visualise
----------------------------

-- | Entrypoint: Generate boolean circuits of the 'Program'.
--
-- Generates one diagram for each 'Decide' statement.
visualise :: Program Name -> [RuleNode]
visualise prog =
  concatMap decideToRuleNode decides
 where
  decides =
    toListOf (gplate @(Decide Name)) prog

-- Currently using the simplest possible implementation
decideToRuleNode :: Decide Name -> [RuleNode]
decideToRuleNode (MkDecide _ sig appform body) =
  case appform of
    MkAppForm _ name _ ->
      Maybe.maybeToList $ exprToRuleNode name sig body

------------------------
  -- Traversals
------------------------

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
exprToRuleNode :: Name -> TypeSig Name -> Expr Name -> Maybe RuleNode
exprToRuleNode (MkName _ name) (MkTypeSig _ givenSig _) body =
  case givenSig of
    MkGivenSig _ [MkOptionallyTypedName _ (MkName _ subject) _] -> do
        rNode <- traverseExpr (rawNameToText subject) body
        pure
          RuleNode
            { andOr = AndOrNode
              { children = Just [rNode]
              , contents = Nothing
              , tag = All
              }
            , prePost = Map.fromList [("Pre", "Does the " <> rawNameToText subject <> " " <> rawNameToText name <> "?")]
            , mark = undefinedUserMark
            , shouldView = Ask
            }
    MkGivenSig _ [] -> traverseExpr (rawNameToText name) body
    MkGivenSig _ _xs -> Nothing -- "DECIDEs with more than one GIVEN not currently supported"

traverseExpr :: Text -> Expr Name -> Maybe RuleNode
traverseExpr subject e = case e of
  And{} ->
    Just $
      ruleNode andPrePost $
        node All [rNode | n <- scanAnd e,
                          Just rNode <- [traverseExpr subject n]]
  Or{} ->
    Just $
      ruleNode orPrePost $
        node Any [rNode | n <- scanOr e, Just rNode <- [traverseExpr subject n]]
  Var _ (MkName _ verb) ->
    Just $ ruleNode emptyPrePost $ leaf subject (rawNameToText verb)
  Equals{} -> Nothing -- Can't handle 'Is' yet
  Not{}    -> Nothing -- Can't handle 'Not' yet
  Proj{}   -> Nothing -- Can't handle 'Proj' yet
  App _ fnName argExprs -> do
    args <- traverse realSimplePrint argExprs
    name <- realSimplePrint fnName
    Just $ ruleNode emptyPrePost $ leaf subject (Text.unwords $ name :  args)
  _        -> Nothing
    -- error $ "[fallthru]\n" <> show x (Keeping comment around because useful for printf-style debugging)
  where
    realSimplePrint :: (L4.ToConcreteNodes L4.PosToken a, L4.HasAnno a, L4.AnnoToken a ~ L4.PosToken) => a -> Maybe Text
    realSimplePrint = fmap (Text.strip . Text.unwords . Text.words) . Either.eitherToMaybe . L4.exactprint

scanAnd :: Expr Name -> [Expr Name]
scanAnd (And _ e1 e2) =
  scanAnd e1 <> scanAnd e2
scanAnd e = [e]

scanOr :: Expr Name -> [Expr Name]
scanOr (Or _ e1 e2) =
  scanOr e1 <> scanOr e2
scanOr e = [e]


------------------------
  -- RuleNode makers
------------------------

andPrePost :: Map Text Text
andPrePost = Map.fromList [("Pre", "all of:")]

orPrePost :: Map Text Text
orPrePost = Map.fromList [("Pre", "any of:")]

emptyPrePost :: Map Text Text
emptyPrePost = mempty

leaf :: Text -> Text -> AndOrNode
leaf subject complement =
  AndOrNode
    { tag = Leaf
    , children = Nothing
    , contents = Just complement
    }
    where
      makeMessage subject = ""
      -- "does the " <> subject <> " " <> t <> "?"

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

------------------------
  -- Dev utils
------------------------

programToRuleNodes :: Text -> [RuleNode]
programToRuleNodes prog =
  case execParser program "" prog of
    Left errs -> error $ unlines $ fmap (Text.unpack . (.message)) (toList errs)
    Right x -> visualise x

vizTest :: Text -> IO ()
vizTest = pPrint . programToRuleNodes
