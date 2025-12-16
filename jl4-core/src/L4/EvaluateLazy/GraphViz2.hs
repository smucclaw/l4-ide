{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- | New FGL-based GraphViz trace visualization
--
-- This module provides a cleaner architecture for rendering evaluation traces
-- using the FGL (Functional Graph Library) and graphviz packages.
--
-- Architecture:
--   1. Analyze: Consume flat trace steps and build intermediate tree structure
--   2. Build: Convert tree to FGL graph
--   3. Render: Use graphviz library to generate DOT format
module L4.EvaluateLazy.GraphViz2 (
  traceToGraphViz,
  GraphVizOptions(..),
  defaultGraphVizOptions
) where

import Base
import qualified Base.Map as Map
import qualified Base.Set as Set
import qualified Base.Text as Text
import L4.EvaluateLazy.Trace (EvalTrace(..))
import L4.EvaluateLazy.Machine (EvalException, boolView)
import L4.Evaluate.ValueLazy (NF(..), Value(..))
import L4.Syntax (Expr(..), Resolved, Branch(..), BranchLhs(..), Unique(..), getOriginal, getUnique, nameToText, pattern Var)
import L4.Print (prettyLayout)

-- FGL and GraphViz imports
import Data.Graph.Inductive.Graph (Graph, Node, LNode, LEdge)
import qualified Data.Graph.Inductive.Graph as FGL
import qualified Data.Graph.Inductive.PatriciaTree as FGL
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Attributes.Complete as GV
import qualified Data.GraphViz.Printing as GV

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

-- | Node in the intermediate render tree
data RenderNode = RenderNode
  { nodeId :: Int
  , nodeLabel :: Text
  , nodeResult :: Maybe Text
  , nodeColor :: Text
  , nodeStyle :: Text
  , children :: [(EdgeInfo, RenderNode)]
  }

data EdgeInfo = EdgeInfo
  { edgeLabel :: Maybe Text
  , edgeReversed :: Bool
  , edgeStyle :: Text
  , edgeColor :: Text
  }

-- | Rendering context carried through tree building
data RenderContext = RenderContext
  { nextNodeId :: Int
  , options :: GraphVizOptions
  , depth :: Int
  , reverseEdges :: Bool  -- For IF conditions
  , parentResult :: Either EvalException NF  -- Parent trace result
  }

defaultEdge :: EdgeInfo
defaultEdge = EdgeInfo
  { edgeLabel = Nothing
  , edgeReversed = False
  , edgeStyle = "solid"
  , edgeColor = "#2ca02c"
  }

-- | Main entry point: convert trace to GraphViz DOT format
traceToGraphViz :: GraphVizOptions -> EvalTrace -> Text
traceToGraphViz opts trace =
  let ctx = RenderContext 0 opts 0 False (Left undefined)
      (tree, _) = buildRenderTree ctx trace
      graph = treeToFGL tree
      dotGraph = graphToDot graph
  in GV.renderDot $ GV.toDot dotGraph

-- ============================================================================
-- Pass 1: Build intermediate tree from flat trace
-- ============================================================================

-- | Build render tree from evaluation trace
buildRenderTree :: RenderContext -> EvalTrace -> (RenderNode, RenderContext)
buildRenderTree ctx (Trace mlabel steps result) =
  let nodeLabel = formatNodeLabel mlabel steps
      nodeResult = if ctx.options.includeValues
                     then Just (formatResult result)
                     else Nothing
      (nodeStyle, nodeColor) = getNodeStyle steps result

      -- Build children from steps
      childCtx = ctx { nextNodeId = ctx.nextNodeId + 1
                     , depth = ctx.depth + 1
                     , parentResult = result
                     }
      (children, finalCtx) = buildChildren childCtx steps

      node = RenderNode
        { nodeId = ctx.nextNodeId
        , nodeLabel = nodeLabel
        , nodeResult = nodeResult
        , nodeColor = nodeColor
        , nodeStyle = nodeStyle
        , children = children
        }
  in (node, finalCtx)

-- | Build children, handling special cases like IF with compound branches
buildChildren :: RenderContext -> [(Expr Resolved, [EvalTrace])] -> ([(EdgeInfo, RenderNode)], RenderContext)
buildChildren ctx [] = ([], ctx)
buildChildren ctx steps@((expr, subtraces):rest) =
  case (expr, rest) of
    -- IF expression with compound branch (next step is the branch evaluation)
    (IfThenElse _ _ thenE elseE, (branchExpr, branchSubtraces):restAfterBranch)
      | length subtraces == 1 ->
        buildIfWithCompoundBranch ctx expr subtraces thenE elseE branchExpr branchSubtraces restAfterBranch

    -- Normal expression
    _ -> buildNormalChildren ctx expr subtraces rest

-- | Handle IF expression where branch is compound (appears as next step)
buildIfWithCompoundBranch :: RenderContext
                          -> Expr Resolved
                          -> [EvalTrace]
                          -> Expr Resolved  -- THEN
                          -> Expr Resolved  -- ELSE
                          -> Expr Resolved  -- Branch expr
                          -> [EvalTrace]     -- Branch subtraces
                          -> [(Expr Resolved, [EvalTrace])]  -- Remaining steps
                          -> ([(EdgeInfo, RenderNode)], RenderContext)
buildIfWithCompoundBranch ctx _ifExpr condTraces thenE elseE branchExpr branchSubtraces restSteps =
  let -- Get condition result
      condResult = case condTraces of
        (condTrace:_) -> traceBoolValue condTrace
        _ -> Nothing

      -- Build condition node
      condCtx = ctx { reverseEdges = True }
      (condNodes, afterCondCtx) = buildSubtraces condCtx condTraces
      condEdges = [(defaultEdge { edgeLabel = Just "IF", edgeReversed = True }, node) | node <- condNodes]

      -- Build stub for unevaluated branch
      (stubNode, afterStubCtx) = case condResult of
        Just True  -> buildStubNode afterCondCtx elseE "ELSE"
        Just False -> buildStubNode afterCondCtx thenE "THEN"
        Nothing    -> (Nothing, afterCondCtx)

      -- Build evaluated branch node with result from parent
      branchLabel = Text.take 50 (prettyLayout branchExpr)
      branchResultText = case ctx.parentResult of
        Right nf -> Just (Text.take 50 (prettyLayout nf))
        Left _ -> Nothing

      -- Build branch subtraces as children of branch node
      branchChildCtx = afterStubCtx { reverseEdges = False }
      (branchChildren, afterBranchCtx) = buildSubtraces branchChildCtx branchSubtraces

      branchNode = RenderNode
        { nodeId = afterStubCtx.nextNodeId
        , nodeLabel = branchLabel
        , nodeResult = branchResultText
        , nodeColor = "#d0e8f2"
        , nodeStyle = "filled"
        , children = [(defaultEdge, child) | child <- branchChildren]
        }

      branchEdgeLabel = case condResult of
        Just True -> "THEN"
        Just False -> "ELSE"
        Nothing -> "branch"

      branchEdge = (defaultEdge { edgeLabel = Just branchEdgeLabel }, branchNode)

      -- Process remaining steps
      restCtx = afterBranchCtx { nextNodeId = afterBranchCtx.nextNodeId + 1 }
      (restChildren, finalCtx) = buildChildren restCtx restSteps

      allChildren = condEdges ++ maybeToList (fmap (\n -> (defaultEdge { edgeLabel = Just (if condResult == Just True then "ELSE" else "THEN"), edgeStyle = "dashed", edgeColor = "#999999" }, n)) stubNode) ++ [branchEdge] ++ restChildren
  in (allChildren, finalCtx)

-- | Build normal children (non-IF special case)
buildNormalChildren :: RenderContext -> Expr Resolved -> [EvalTrace] -> [(Expr Resolved, [EvalTrace])] -> ([(EdgeInfo, RenderNode)], RenderContext)
buildNormalChildren ctx expr subtraces rest =
  let -- Get edge configs for this expression
      edgeConfigs = edgeConfigsFor expr subtraces

      -- Build subtrace nodes
      (subNodes, afterSubCtx) = buildSubtracesWithConfigs ctx subtraces edgeConfigs

      -- Process remaining siblings
      (restChildren, finalCtx) = buildChildren afterSubCtx rest

  in (subNodes ++ restChildren, finalCtx)

-- | Build nodes from subtraces
buildSubtraces :: RenderContext -> [EvalTrace] -> ([RenderNode], RenderContext)
buildSubtraces ctx traces = go ctx traces []
  where
    go curCtx [] acc = (reverse acc, curCtx)
    go curCtx (tr:trs) acc =
      let (node, nextCtx) = buildRenderTree curCtx tr
      in go nextCtx trs (node:acc)

-- | Build nodes from subtraces with edge configs
buildSubtracesWithConfigs :: RenderContext -> [EvalTrace] -> [EdgeConfig] -> ([(EdgeInfo, RenderNode)], RenderContext)
buildSubtracesWithConfigs ctx traces configs = go ctx (zip traces configs) []
  where
    go curCtx [] acc = (reverse acc, curCtx)
    go curCtx ((tr, cfg):rest) acc =
      let (node, nextCtx) = buildRenderTree curCtx tr
          edge = EdgeInfo
            { edgeLabel = getEdgeLabel cfg
            , edgeReversed = getEdgeReversed cfg
            , edgeStyle = "solid"
            , edgeColor = "#2ca02c"
            }
      in go nextCtx rest ((edge, node):acc)

-- | Build stub node for unevaluated branch
buildStubNode :: RenderContext -> Expr Resolved -> Text -> (Maybe RenderNode, RenderContext)
buildStubNode ctx expr label =
  if not ctx.options.showUnevaluated
    then (Nothing, ctx)
    else
      let nodeLabel = Text.take 50 (prettyLayout expr)
          node = RenderNode
            { nodeId = ctx.nextNodeId
            , nodeLabel = nodeLabel
            , nodeResult = Nothing
            , nodeColor = "#e0e0e0"
            , nodeStyle = "filled,dashed"
            , children = []
            }
      in (Just node, ctx { nextNodeId = ctx.nextNodeId + 1 })

-- ============================================================================
-- Pass 2: Convert tree to FGL graph
-- ============================================================================

type EvalGraph = FGL.Gr Text Text

-- | Convert render tree to FGL graph
treeToFGL :: RenderNode -> EvalGraph
treeToFGL root =
  let (nodes, edges) = collectNodesAndEdges root
  in FGL.mkGraph nodes edges

-- | Collect all nodes and edges from tree
collectNodesAndEdges :: RenderNode -> ([LNode Text], [LEdge Text])
collectNodesAndEdges node =
  let thisNode = (node.nodeId, formatNodeForFGL node)
      (childNodes, childEdges) = unzip [collectNodesAndEdges child | (_, child) <- node.children]
      allChildNodes = concat childNodes
      allChildEdges = concat childEdges
      edgesToChildren = [(node.nodeId, child.nodeId, fromMaybe "" edge.edgeLabel) | (edge, child) <- node.children]
  in (thisNode : allChildNodes, edgesToChildren ++ allChildEdges)

formatNodeForFGL :: RenderNode -> Text
formatNodeForFGL node =
  case node.nodeResult of
    Just result -> node.nodeLabel <> "\n────────────────\n" <> result
    Nothing -> node.nodeLabel

-- ============================================================================
-- Pass 3: Convert FGL graph to GraphViz DOT
-- ============================================================================

graphToDot :: EvalGraph -> GV.DotGraph Node
graphToDot graph = GV.graphToDot params graph
  where
    params = GV.nonClusteredParams
      { GV.globalAttributes =
          [ GV.GraphAttrs [GV.RankDir GV.FromTop]
          , GV.NodeAttrs [GV.Shape GV.BoxShape, GV.Style [GV.SItem GV.Rounded []]]
          ]
      , GV.fmtNode = \(_, l) -> [GV.Label (GV.StrLabel l)]
      , GV.fmtEdge = \(_, _, l) -> [GV.Label (GV.StrLabel l), GV.Color [GV.toWColor GV.Green]]
      }

-- ============================================================================
-- Helper functions (from original module)
-- ============================================================================

data EdgeConfig = EdgeConfig
  { edgeLabel :: Maybe Text
  , edgeReversed :: Bool
  }
  deriving stock (Eq, Show)

getEdgeLabel :: EdgeConfig -> Maybe Text
getEdgeLabel = (.edgeLabel)

getEdgeReversed :: EdgeConfig -> Bool
getEdgeReversed = (.edgeReversed)

edgeConfigsFor :: Expr Resolved -> [EvalTrace] -> [EdgeConfig]
edgeConfigsFor (IfThenElse _ _ _ _) subtraces = labelIf subtraces
  where
    labelIf [] = []
    labelIf [_] = [EdgeConfig { edgeLabel = Just "IF", edgeReversed = True }]
    labelIf (condTrace:branchTraces) =
      let branchLabel = case traceBoolValue condTrace of
                          Just True -> "THEN"
                          Just False -> "ELSE"
                          _ -> "branch"
      in EdgeConfig { edgeLabel = Just "IF", edgeReversed = True }
         : replicate (length branchTraces) (EdgeConfig { edgeLabel = Just branchLabel, edgeReversed = False })
edgeConfigsFor _ subtraces = replicate (length subtraces) (EdgeConfig Nothing False)

traceBoolValue :: EvalTrace -> Maybe Bool
traceBoolValue (Trace _ _ (Right nf)) =
  case nfToBool nf of
    Just b -> Just b
    Nothing -> nfToBoolFromText nf
traceBoolValue _ = Nothing

nfToBool :: NF -> Maybe Bool
nfToBool (MkNF val) = boolView val
nfToBool Omitted = Nothing

nfToBoolFromText :: NF -> Maybe Bool
nfToBoolFromText nf =
  let txt = Text.toUpper (prettyLayout nf)
  in if "TRUE" `Text.isInfixOf` txt then Just True
     else if "FALSE" `Text.isInfixOf` txt then Just False
     else Nothing

formatNodeLabel :: Maybe Resolved -> [(Expr Resolved, [EvalTrace])] -> Text
formatNodeLabel mlabel steps =
  let exprText = case steps of
        [] -> "<no steps>"
        (firstExpr, _) : _ -> Text.take 50 (prettyLayout firstExpr)
      labelText = maybe "" (\lbl -> Text.take 50 (prettyLayout lbl) <> "\n") mlabel
  in labelText <> exprText

formatResult :: Either EvalException NF -> Text
formatResult (Right nf) = Text.take 50 (prettyLayout nf)
formatResult (Left _) = "<error>"

getNodeStyle :: [(Expr Resolved, [EvalTrace])] -> Either EvalException NF -> (Text, Text)
getNodeStyle _ (Right _) = ("filled", "#d0e8f2")
getNodeStyle _ (Left _) = ("filled", "#ffcccc")
