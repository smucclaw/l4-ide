{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- | Clean FGL-based GraphViz trace visualization
--
-- Architecture: Pure recursion following inductive structure
--   1. Recursively build FGL graph from trace
--   2. Apply local visual optimizations
--   3. Render to DOT using graphviz library
module L4.EvaluateLazy.GraphViz2 (
  traceToGraphViz,
  GraphVizOptions(..),
  defaultGraphVizOptions
) where

import Base
import qualified Base.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import L4.EvaluateLazy.Trace (EvalTrace(..))
import L4.EvaluateLazy.Machine (EvalException, boolView)
import L4.Evaluate.ValueLazy (NF(..))
import L4.Syntax (Expr(..), Resolved, Branch(..), BranchLhs(..))
import L4.Print (prettyLayout)

-- FGL and GraphViz imports
import Data.Graph.Inductive.Graph (Node, LNode, LEdge)
import qualified Data.Graph.Inductive.Graph as FGL
import qualified Data.Graph.Inductive.PatriciaTree as FGL
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GV

-- | Options for controlling GraphViz output
data GraphVizOptions = GraphVizOptions
  { includeValues :: Bool
  , showUnevaluated :: Bool
  , simplifyTrivial :: Bool
  , maxDepth :: Maybe Int
  } deriving (Eq, Show)

defaultGraphVizOptions :: GraphVizOptions
defaultGraphVizOptions = GraphVizOptions
  { includeValues = True
  , showUnevaluated = True
  , simplifyTrivial = False
  , maxDepth = Nothing
  }

-- | Node attributes for rendering
data NodeAttrs = NodeAttrs
  { nodeLabel :: Text
  , fillColor :: Text
  , nodeStyle :: Text
  } deriving (Eq, Show)

-- | Edge attributes for rendering
data EdgeAttrs = EdgeAttrs
  { edgeLabel :: Maybe Text
  , edgeColor :: Text
  , edgeStyle :: Text
  , edgeDir :: Maybe Text  -- "back" for reversed
  } deriving (Eq, Show)

type EvalGraph = FGL.Gr NodeAttrs EdgeAttrs

-- | Main entry point: convert trace to GraphViz DOT format
traceToGraphViz :: GraphVizOptions -> EvalTrace -> Text
traceToGraphViz opts evalTrace =
  let (nodes, edges, _) = buildGraph opts 0 0 evalTrace
      graph = FGL.mkGraph nodes edges
      -- Identify IF patterns and add invisible ordering edges
      ifPatterns = identifyIFPatterns graph
      dotGraph = graphToDot graph ifPatterns
  in Text.Lazy.toStrict $ GV.printDotGraph dotGraph

-- ============================================================================
-- Pure recursive graph building - follows inductive structure
-- ============================================================================

-- | Build FGL graph from trace
-- Returns (nodes, edges, next available node ID)
buildGraph :: GraphVizOptions -> Int -> Node -> EvalTrace
           -> ([LNode NodeAttrs], [LEdge EdgeAttrs], Node)
buildGraph opts depth nodeId (Trace mlabel steps result) =
  let -- Create node for this trace
      nodeAttrs = NodeAttrs
        { nodeLabel = formatTraceLabel opts mlabel steps result
        , fillColor = if isRight result then "#d0e8f2" else "#ffcccc"
        , nodeStyle = "filled"
        }
      thisNode = [(nodeId, nodeAttrs)]

      -- Recursively build nodes/edges for each step
      (childNodes, childEdges, nextId) =
        buildSteps opts (depth + 1) (nodeId + 1) nodeId steps

      -- Combine with simple list concatenation
      allNodes = thisNode ++ childNodes
      allEdges = childEdges

  in (allNodes, allEdges, nextId)

-- | Build nodes/edges for all steps
buildSteps :: GraphVizOptions -> Int -> Node -> Node
           -> [(Expr Resolved, [EvalTrace])]
           -> ([LNode NodeAttrs], [LEdge EdgeAttrs], Node)
buildSteps _opts _depth nextId _parentId [] = ([], [], nextId)
buildSteps opts depth nodeId parentId ((expr, subtraces):rest) =
  let -- Get edge configurations for this expression type
      edgeConfigs = edgeConfigsFor expr subtraces

      -- Build nodes/edges for each subtrace
      (subNodes, subEdges, afterSubId) =
        buildSubtraces opts depth nodeId parentId subtraces edgeConfigs

      -- Add stub nodes for IF unevaluated branches if needed
      (stubNodes, stubEdges, afterStubId) =
        if opts.showUnevaluated
          then buildStubs opts afterSubId parentId expr subtraces
          else ([], [], afterSubId)

      -- Build remaining sibling steps
      (restNodes, restEdges, finalId) =
        buildSteps opts depth afterStubId parentId rest

      -- Combine everything with list concatenation
      allNodes = subNodes ++ stubNodes ++ restNodes
      allEdges = subEdges ++ stubEdges ++ restEdges

  in (allNodes, allEdges, finalId)

-- | Build nodes/edges for subtraces with edge labels
buildSubtraces :: GraphVizOptions -> Int -> Node -> Node
               -> [EvalTrace] -> [EdgeConfig]
               -> ([LNode NodeAttrs], [LEdge EdgeAttrs], Node)
buildSubtraces _opts _depth nodeId _parentId [] _configs = ([], [], nodeId)
buildSubtraces opts depth nodeId parentId (tr:trs) (cfg:cfgs) =
  let -- Build nodes/edges for this subtrace
      (subNodes, subEdges, nextId) = buildGraph opts depth nodeId tr

      -- Create edge from parent to this subtrace's root
      edgeAttrs = EdgeAttrs
        { edgeLabel = cfg.edgeLabel
        , edgeColor = "#2ca02c"
        , edgeStyle = "solid"
        , edgeDir = if cfg.edgeReversed then Just "back" else Nothing
        }
      edge = (parentId, nodeId, edgeAttrs)

      -- Build remaining subtraces
      (restNodes, restEdges, finalId) =
        buildSubtraces opts depth nextId parentId trs cfgs

      -- Combine with list concatenation
      allNodes = subNodes ++ restNodes
      allEdges = edge : subEdges ++ restEdges

  in (allNodes, allEdges, finalId)
buildSubtraces opts depth nodeId parentId (tr:trs) [] =
  -- No more configs, use default
  buildSubtraces opts depth nodeId parentId (tr:trs) [defaultEdgeConfig]

-- | Build stub nodes for unevaluated branches
buildStubs :: GraphVizOptions -> Node -> Node -> Expr Resolved -> [EvalTrace]
           -> ([LNode NodeAttrs], [LEdge EdgeAttrs], Node)
buildStubs _opts nodeId parentId (IfThenElse _ _ thenE elseE) subtraces =
  case (traceBoolValue <$> listToMaybe subtraces) of
    Just (Just True) ->
      -- THEN taken, stub ELSE
      let elseNode = (nodeId, NodeAttrs
            { nodeLabel = Text.take 50 (prettyLayout elseE)
            , fillColor = "#e0e0e0"
            , nodeStyle = "filled,dashed"
            })
          elseEdge = (parentId, nodeId, EdgeAttrs
            { edgeLabel = Just "ELSE"
            , edgeColor = "#999999"
            , edgeStyle = "dashed"
            , edgeDir = Nothing
            })
      in ([elseNode], [elseEdge], nodeId + 1)

    Just (Just False) ->
      -- ELSE taken, stub THEN
      let thenNode = (nodeId, NodeAttrs
            { nodeLabel = Text.take 50 (prettyLayout thenE)
            , fillColor = "#e0e0e0"
            , nodeStyle = "filled,dashed"
            })
          thenEdge = (parentId, nodeId, EdgeAttrs
            { edgeLabel = Just "THEN"
            , edgeColor = "#999999"
            , edgeStyle = "dashed"
            , edgeDir = Nothing
            })
      in ([thenNode], [thenEdge], nodeId + 1)

    _ -> ([], [], nodeId)

buildStubs _opts nodeId _parentId _expr _subtraces = ([], [], nodeId)

-- ============================================================================
-- Phase 2: Visual Optimization - IF/THEN/ELSE grouping
-- ============================================================================

-- | Pattern representing an IF expression's immediate children
data IFPattern = IFPattern
  { ifParentNode :: Node
  , ifConditionNode :: Node
  , ifThenNode :: Maybe Node
  , ifElseNode :: Maybe Node
  } deriving (Eq, Show)

-- | Identify IF expression patterns in the graph
identifyIFPatterns :: EvalGraph -> [IFPattern]
identifyIFPatterns graph =
  let allEdges = FGL.labEdges graph
      -- Group edges by source node
      edgesByParent = groupBy (\(s1,_,_) (s2,_,_) -> s1 == s2)
                    $ sortBy (\(s1,_,_) (s2,_,_) -> compare s1 s2) allEdges
  in mapMaybe extractIFPattern edgesByParent

extractIFPattern :: [LEdge EdgeAttrs] -> Maybe IFPattern
extractIFPattern [] = Nothing
extractIFPattern edges@((parentNode,_,_):_) =
  let -- Find edges by label
      findEdge lbl = find (\(_,_,attrs) -> attrs.edgeLabel == Just lbl) edges
      condEdge = findEdge "IF"
      thenEdge = findEdge "THEN"
      elseEdge = findEdge "ELSE"
  in case condEdge of
       Just (_,condNode,_) ->
         -- We have an IF pattern
         let thenNode = (\(_,n,_) -> n) <$> thenEdge
             elseNode = (\(_,n,_) -> n) <$> elseEdge
         in Just $ IFPattern parentNode condNode thenNode elseNode
       Nothing -> Nothing

-- ============================================================================
-- Edge configuration helpers
-- ============================================================================

data EdgeConfig = EdgeConfig
  { edgeLabel :: Maybe Text
  , edgeReversed :: Bool
  } deriving stock (Eq, Show)

defaultEdgeConfig :: EdgeConfig
defaultEdgeConfig = EdgeConfig Nothing False

edgeConfigsFor :: Expr Resolved -> [EvalTrace] -> [EdgeConfig]
edgeConfigsFor (IfThenElse _ _ _ _) subtraces = labelIf subtraces
  where
    labelIf [] = []
    labelIf [_] = [EdgeConfig (Just "IF") True]
    labelIf (condTrace:branchTraces) =
      let branchLabel = case traceBoolValue condTrace of
            Just True -> "THEN"
            Just False -> "ELSE"
            _ -> "branch"
      in EdgeConfig (Just "IF") True
         : replicate (length branchTraces) (EdgeConfig (Just branchLabel) False)

edgeConfigsFor (Consider _ _ branches) subtraces =
  let branchLabels = map branchLabel branches
      labelFor idx = listToMaybe $ drop idx branchLabels
  in defaultEdgeConfig : map (\lbl -> EdgeConfig (Just lbl) False) (catMaybes [labelFor i | i <- [0..length subtraces - 2]])
  where
    branchLabel (MkBranch _ lhs _) = case lhs of
      When _ pat -> "when " <> prettyLayout pat
      Otherwise _ -> "otherwise"

edgeConfigsFor _ subtraces =
  replicate (length subtraces) defaultEdgeConfig

-- ============================================================================
-- Formatting helpers
-- ============================================================================

formatTraceLabel :: GraphVizOptions -> Maybe Resolved -> [(Expr Resolved, [EvalTrace])] -> Either EvalException NF -> Text
formatTraceLabel opts mlabel steps result =
  let labelText = maybe "" (\lbl -> prettyLayout lbl <> "\n") mlabel
      exprText = case steps of
        [] -> ""
        (expr, _):_ -> firstLine (prettyLayout expr)
      resultText = if opts.includeValues
        then case result of
          Right nf -> "\n────────────────\n" <> firstLine (prettyLayout nf)
          Left _ -> "\n────────────────\n<error>"
        else ""
  in labelText <> exprText <> resultText

firstLine :: Text -> Text
firstLine = Text.takeWhile (/= '\n')

traceBoolValue :: EvalTrace -> Maybe Bool
traceBoolValue (Trace _ _ (Right nf)) = nfToBool nf
traceBoolValue _ = Nothing

nfToBool :: NF -> Maybe Bool
nfToBool (MkNF val) = boolView val
nfToBool Omitted = Nothing

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

-- ============================================================================
-- GraphViz rendering
-- ============================================================================

-- | Add invisible ordering edges for IF/THEN/ELSE layout
addIFOrderingEdges :: [IFPattern] -> [LEdge EdgeAttrs]
addIFOrderingEdges patterns =
  concatMap makeOrderingEdges patterns
  where
    makeOrderingEdges (IFPattern _ cond mThen mElse) =
      let -- Create invisible edges to force left-to-right layout: IF -> THEN -> ELSE
          invisEdgeAttrs = EdgeAttrs
            { edgeLabel = Nothing
            , edgeColor = "#000000"
            , edgeStyle = "invis"
            , edgeDir = Nothing
            }
          -- Chain: condition -> then -> else (only for nodes that exist)
          edges1 = case mThen of
            Just thenNode -> [(cond, thenNode, invisEdgeAttrs)]
            Nothing -> []
          edges2 = case (mThen, mElse) of
            (Just thenNode, Just elseNode) -> [(thenNode, elseNode, invisEdgeAttrs)]
            (Nothing, Just elseNode) -> [(cond, elseNode, invisEdgeAttrs)]
            _ -> []
      in edges1 ++ edges2

graphToDot :: EvalGraph -> [IFPattern] -> GV.DotGraph Node
graphToDot graph ifPatterns =
  let -- Add invisible ordering edges for IF patterns
      orderingEdges = addIFOrderingEdges ifPatterns
      graphWithOrdering = FGL.insEdges orderingEdges graph
  in
  let params = GV.nonClusteredParams
        { GV.globalAttributes =
            [ GV.GraphAttrs [GV.RankDir GV.FromTop]
            , GV.NodeAttrs
                [ GV.Shape GV.BoxShape
                , GV.Style [GV.SItem GV.Rounded []]
                ]
            ]
        , GV.fmtNode = \(_, attrs) ->
            let -- Parse hex color to RGB (simplified - just use fixed colors for now)
                color = if attrs.fillColor == "#d0e8f2"
                  then GV.RGB 208 232 242
                  else if attrs.fillColor == "#e0e0e0"
                    then GV.RGB 224 224 224
                    else GV.RGB 255 204 204
            in [ GV.Label (GV.StrLabel (Text.Lazy.fromStrict attrs.nodeLabel))
               , GV.FillColor [GV.toWC color]
               , GV.Style [GV.SItem GV.Filled []]
               ]
        , GV.fmtEdge = \(_, _, attrs) ->
            let color = if attrs.edgeColor == "#2ca02c"
                  then GV.RGB 44 160 44
                  else GV.RGB 153 153 153  -- #999999
                baseAttrs =
                  [ GV.Color [GV.toWC color]
                  , GV.PenWidth 2
                  , GV.FontSize 10
                  ]
                labelAttr = maybe [] (\lbl -> [GV.Label (GV.StrLabel (Text.Lazy.fromStrict lbl))]) attrs.edgeLabel
                dirAttr = maybe [] (\d -> [GV.Dir (if d == "back" then GV.Back else GV.Forward)]) attrs.edgeDir
                styleAttr = if attrs.edgeStyle == "dashed" || attrs.edgeStyle == "invis"
                  then [GV.Style [GV.SItem (if attrs.edgeStyle == "invis" then GV.Invisible else GV.Dashed) []]]
                  else []
            in baseAttrs ++ labelAttr ++ dirAttr ++ styleAttr
        }
  in GV.graphToDot params graphWithOrdering
