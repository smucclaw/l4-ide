{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Backend.BooleanDecisionQuery (
  BoolExpr (..),
  DecisionQueryResult (..),
  QueryOutcome (..),
  VarImpact (..),
  CompiledDecisionQuery (..),
  compileDecisionQuery,
  queryDecision,
) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data BoolExpr v
  = BTrue
  | BFalse
  | BVar !v
  | BNot !(BoolExpr v)
  | BAnd ![BoolExpr v]
  | BOr ![BoolExpr v]
  deriving stock (Eq, Show)

type NodeId = Int

data Node
  = Terminal !Bool
  | Branch !Int !NodeId !NodeId
  deriving stock (Eq, Show)

data Op = OpAnd | OpOr
  deriving stock (Eq, Ord, Show)

data Bdd = Bdd
  { nodes :: Map NodeId Node
  , uniqueTable :: Map (Int, NodeId, NodeId) NodeId
  , nextId :: !NodeId
  , applyMemo :: Map (Op, NodeId, NodeId) NodeId
  , negMemo :: Map NodeId NodeId
  }
  deriving stock (Eq, Show)

emptyBdd :: Bdd
emptyBdd =
  Bdd
    { nodes = Map.fromList [(0, Terminal False), (1, Terminal True)]
    , uniqueTable = Map.empty
    , nextId = 2
    , applyMemo = Map.empty
    , negMemo = Map.empty
    }

isTerminal :: NodeId -> Bool
isTerminal n = n == 0 || n == 1

mk :: Int -> NodeId -> NodeId -> Bdd -> (NodeId, Bdd)
mk v lo hi bdd
  | lo == hi = (lo, bdd)
  | otherwise =
      case Map.lookup (v, lo, hi) bdd.uniqueTable of
        Just existing -> (existing, bdd)
        Nothing ->
          let nid = bdd.nextId
              nodes' = Map.insert nid (Branch v lo hi) bdd.nodes
              uniq' = Map.insert (v, lo, hi) nid bdd.uniqueTable
           in (nid, bdd {nodes = nodes', uniqueTable = uniq', nextId = nid + 1})

var :: Int -> Bdd -> (NodeId, Bdd)
var v = mk v 0 1

neg :: NodeId -> Bdd -> (NodeId, Bdd)
neg nid bdd =
  case Map.lookup nid bdd.negMemo of
    Just cached -> (cached, bdd)
    Nothing ->
      case Map.lookup nid bdd.nodes of
        Nothing -> error "Internal error: BDD node not found"
        Just (Terminal False) ->
          let bdd' = bdd {negMemo = Map.insert nid 1 bdd.negMemo}
           in (1, bdd')
        Just (Terminal True) ->
          let bdd' = bdd {negMemo = Map.insert nid 0 bdd.negMemo}
           in (0, bdd')
        Just (Branch v lo hi) ->
          let (nlo, bdd1) = neg lo bdd
              (nhi, bdd2) = neg hi bdd1
              (res, bdd3) = mk v nlo nhi bdd2
              bdd4 = bdd3 {negMemo = Map.insert nid res bdd3.negMemo}
           in (res, bdd4)

opEval :: Op -> Bool -> Bool -> Bool
opEval OpAnd a b = a && b
opEval OpOr a b = a || b

apply :: Op -> NodeId -> NodeId -> Bdd -> (NodeId, Bdd)
apply op a b bdd
  | a == b = (a, bdd)
  | isTerminal a && isTerminal b =
      case (bdd.nodes Map.! a, bdd.nodes Map.! b) of
        (Terminal av, Terminal bv) ->
          if opEval op av bv then (1, bdd) else (0, bdd)
        _ -> error "Internal error: terminal node id did not map to Terminal"
  | otherwise =
      case Map.lookup (op, a, b) bdd.applyMemo of
        Just cached -> (cached, bdd)
        Nothing ->
          let an = bdd.nodes Map.! a
              bn = bdd.nodes Map.! b
              aVar = case an of
                Branch v _ _ -> v
                Terminal _ -> maxBound :: Int
              bVar = case bn of
                Branch v _ _ -> v
                Terminal _ -> maxBound :: Int
              top = min aVar bVar
              (aLo, aHi) = case an of
                Branch v lo hi | v == top -> (lo, hi)
                _ -> (a, a)
              (bLo, bHi) = case bn of
                Branch v lo hi | v == top -> (lo, hi)
                _ -> (b, b)
              (loRes, bdd1) = apply op aLo bLo bdd
              (hiRes, bdd2) = apply op aHi bHi bdd1
              (res, bdd3) = mk top loRes hiRes bdd2
              bdd4 = bdd3 {applyMemo = Map.insert (op, a, b) res bdd3.applyMemo}
           in (res, bdd4)

restrictVar :: Int -> Bool -> NodeId -> Bdd -> (NodeId, Bdd)
restrictVar v val nid bdd =
  case bdd.nodes Map.! nid of
    Terminal _ -> (nid, bdd)
    Branch v' lo hi
      | v' == v -> if val then (hi, bdd) else (lo, bdd)
      | otherwise ->
          let (nlo, bdd1) = restrictVar v val lo bdd
              (nhi, bdd2) = restrictVar v val hi bdd1
           in mk v' nlo nhi bdd2

restrictMany :: Map Int Bool -> NodeId -> Bdd -> (NodeId, Bdd)
restrictMany bindings nid bdd =
  Map.foldlWithKey'
    (\(accId, accBdd) v val -> restrictVar v val accId accBdd)
    (nid, bdd)
    bindings

support :: NodeId -> Bdd -> Set Int
support root bdd = go Set.empty Set.empty root
 where
  go :: Set NodeId -> Set Int -> NodeId -> Set Int
  go seen vars nid
    | isTerminal nid = vars
    | nid `Set.member` seen = vars
    | otherwise =
        case bdd.nodes Map.! nid of
          Terminal _ -> vars
          Branch v lo hi ->
            let seen' = Set.insert nid seen
                vars' = Set.insert v vars
                vars'' = go seen' vars' lo
             in go seen' vars'' hi

determinedFromRoot :: NodeId -> Maybe Bool
determinedFromRoot 0 = Just False
determinedFromRoot 1 = Just True
determinedFromRoot _ = Nothing

data QueryOutcome v = QueryOutcome
  { determined :: Maybe Bool
  , support :: Set v
  }
  deriving stock (Eq, Show)

data VarImpact v = VarImpact
  { ifTrue :: QueryOutcome v
  , ifFalse :: QueryOutcome v
  }
  deriving stock (Eq, Show)

data DecisionQueryResult v = DecisionQueryResult
  { determined :: Maybe Bool
  , support :: Set v
  , ranked :: [v]
  , impact :: Map v (VarImpact v)
  }
  deriving stock (Eq, Show)

data CompiledDecisionQuery v = CompiledDecisionQuery
  { varOrder :: [v]
  , vToIdx :: Map v Int
  , idxToV :: Map Int v
  , root :: NodeId
  , bdd :: Bdd
  }

compileDecisionQuery :: (Ord v) => [v] -> BoolExpr v -> CompiledDecisionQuery v
compileDecisionQuery order expr =
  let vToIdx = Map.fromList (zip order [0 ..])
      (root, bdd0) = compile vToIdx expr emptyBdd
   in CompiledDecisionQuery
        { varOrder = order
        , vToIdx
        , idxToV = Map.fromList (zip [0 ..] order)
        , root
        , bdd = bdd0
        }
 where
  compile :: (Ord v) => Map v Int -> BoolExpr v -> Bdd -> (NodeId, Bdd)
  compile vToIdx = \case
    BTrue -> \bdd -> (1, bdd)
    BFalse -> \bdd -> (0, bdd)
    BVar v ->
      case Map.lookup v vToIdx of
        Nothing -> error "Internal error: var not found in order"
        Just idx -> \bdd -> var idx bdd
    BNot e ->
      \bdd ->
        let (nid, bdd1) = compile vToIdx e bdd
         in neg nid bdd1
    BAnd es ->
      \bdd ->
        foldl
          (\(acc, bddAcc) e ->
            let (nid, bddE) = compile vToIdx e bddAcc
             in apply OpAnd acc nid bddE
          )
          (1, bdd)
          es
    BOr es ->
      \bdd ->
        foldl
          (\(acc, bddAcc) e ->
            let (nid, bddE) = compile vToIdx e bddAcc
             in apply OpOr acc nid bddE
          )
          (0, bdd)
          es

queryDecision :: (Ord v) => CompiledDecisionQuery v -> Map v Bool -> DecisionQueryResult v
queryDecision compiled bindings =
  let idxBindings =
        Map.fromList
          [ (idx, b)
          | (v, b) <- Map.toList bindings
          , Just idx <- [Map.lookup v compiled.vToIdx]
          ]
      (restrictedRoot, bdd1) = restrictMany idxBindings compiled.root compiled.bdd
      supportIdx = support restrictedRoot bdd1
      supportVs =
        Set.fromList
          [ v
          | idx <- Set.toList supportIdx
          , Just v <- [Map.lookup idx compiled.idxToV]
          ]
      outcomeOf rid bdd =
        QueryOutcome
          { determined = determinedFromRoot rid
          , support =
              Set.fromList
                [ v
                | idx <- Set.toList (support rid bdd)
                , Just v <- [Map.lookup idx compiled.idxToV]
                ]
          }
      impactMap =
        Map.fromList
          [ ( v
            , case Map.lookup v compiled.vToIdx of
                Nothing -> error "Internal error: var not found in order"
                Just idx ->
                  let (ridT, bddT) = restrictVar idx True restrictedRoot bdd1
                      (ridF, bddF) = restrictVar idx False restrictedRoot bdd1
                   in VarImpact
                        { ifTrue = outcomeOf ridT bddT
                        , ifFalse = outcomeOf ridF bddF
                        }
            )
          | v <- Set.toList supportVs
          ]
      determinableCount v =
        case Map.lookup v impactMap of
          Nothing -> 0 :: Int
          Just vi ->
            fromEnum (vi.ifTrue.determined /= Nothing)
              + fromEnum (vi.ifFalse.determined /= Nothing)
      level v = Map.findWithDefault 1000000 v compiled.vToIdx
      rankedVars =
        sortOn
          (\v -> (-determinableCount v, level v))
          (Set.toList supportVs)
   in DecisionQueryResult
        { determined = determinedFromRoot restrictedRoot
        , support = supportVs
        , ranked = rankedVars
        , impact = impactMap
        }
