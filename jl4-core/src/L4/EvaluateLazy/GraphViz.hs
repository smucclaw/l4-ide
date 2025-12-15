{-# LANGUAGE ViewPatterns #-}
module L4.EvaluateLazy.GraphViz (
  traceToGraphViz,
  GraphVizOptions(..),
  defaultGraphVizOptions
) where

import Base
import qualified Base.Text as Text
import L4.EvaluateLazy.Trace (EvalTrace(..))
import L4.EvaluateLazy.Machine (EvalException, boolView)
import L4.Evaluate.ValueLazy (NF(..))
import L4.Syntax (Expr(..), Resolved, Branch(..), BranchLhs(..))
import L4.Print (prettyLayout)

-- | Options for controlling GraphViz output
data GraphVizOptions = GraphVizOptions
  { includeValues :: Bool
    -- ^ Include result values in node labels (default True)
  , simplifyTrivial :: Bool
    -- ^ Omit trivial nodes like literal values (default True)
  , showUnevaluated :: Bool
    -- ^ Show stub nodes for unevaluated branches (default True)
  , maxDepth :: Maybe Int
    -- ^ Maximum depth to render (Nothing = unlimited, default Nothing)
  }
  deriving stock (Eq, Show)

defaultGraphVizOptions :: GraphVizOptions
defaultGraphVizOptions = GraphVizOptions
  { includeValues = True
  , simplifyTrivial = True
  , showUnevaluated = True
  , maxDepth = Nothing
  }

-- | Convert an evaluation trace to GraphViz DOT format
traceToGraphViz :: GraphVizOptions -> EvalTrace -> Text
traceToGraphViz opts evalTrace =
  let header = "digraph evaluation_trace {\n"
      config = "  rankdir=TB;\n  node [shape=box, style=rounded];\n"
      (body, _) = renderTrace opts 0 0 evalTrace
      footer = "}\n"
  in header <> config <> body <> footer

-- | Render a single trace node and its children
-- Returns (DOT text, next available node ID)
renderTrace :: GraphVizOptions -> Int -> Int -> EvalTrace -> (Text, Int)
renderTrace opts@(GraphVizOptions {maxDepth, simplifyTrivial}) depth nodeId (Trace steps result) =
  case maxDepth of
    Just maxD | depth >= maxD -> 
      -- Hit depth limit, render stub
      let nodeLabel = escapeLabel "... (max depth reached)"
          nodeDef = "  node" <> Text.pack (show nodeId) <> " [label=\"" <> nodeLabel <> "\", style=dashed, color=gray];\n"
      in (nodeDef, nodeId + 1)
    _ ->
      let currentNode = nodeId
          currentNodeId = "node" <> Text.pack (show currentNode)
          
          -- Determine node label and styling
          (_nodeLabel, nodeStyle) = formatNode opts steps result
          
          -- Skip this node if it's trivial and simplification is enabled
          shouldSkip = simplifyTrivial && isTrivialNode steps
          
          -- Generate node definition
          nodeDef = if shouldSkip
                      then ""
                      else "  " <> currentNodeId <> " " <> nodeStyle <> ";\n"
          
          -- Render child nodes
          (childDefs, finalNodeId) = renderChildren opts depth currentNode (currentNode + 1) steps
          
      in (nodeDef <> childDefs, finalNodeId)

-- | Render all child steps
renderChildren :: GraphVizOptions -> Int -> Int -> Int -> [(Expr Resolved, [EvalTrace])] -> (Text, Int)
renderChildren _opts _depth _parentNode nodeId [] = ("", nodeId)
renderChildren opts depth parentNode nodeId ((expr, subtraces):rest) =
  let parentNodeId = "node" <> Text.pack (show parentNode)

      -- Render each subtrace
      (childTexts, childRootIds, nextId) = renderSubtraces opts (depth + 1) nodeId subtraces

      edgeLabels = edgeLabelsFor expr subtraces

      mkEdge childIdx mLabel =
        let attrs =
              "[color=\"#2ca02c\", penwidth=2"
                <> maybe "" (\lbl -> ", label=\"" <> escapeLabel lbl <> "\"") mLabel
                <> "]"
        in "  " <> parentNodeId <> " -> node" <> Text.pack (show childIdx) <> " " <> attrs <> ";\n"

      edges =
        mconcat $
          zipWith mkEdge childRootIds (edgeLabels ++ repeat Nothing)

      -- Render remaining siblings
      (restTexts, finalId) = renderChildren opts depth parentNode nextId rest
      
  in (childTexts <> edges <> restTexts, finalId)

-- | Render a list of subtraces
renderSubtraces :: GraphVizOptions -> Int -> Int -> [EvalTrace] -> (Text, [Int], Int)
renderSubtraces _opts _depth nodeId [] = ("", [], nodeId)
renderSubtraces opts depth nodeId (tr:traces) =
  let (thisText, nextId) = renderTrace opts depth nodeId tr
      (restText, restIds, finalId) = renderSubtraces opts depth nextId traces
  in (thisText <> restText, nodeId : restIds, finalId)

-- | Format a node's label and styling based on its content
formatNode :: GraphVizOptions -> [(Expr Resolved, [EvalTrace])] -> Either EvalException NF -> (Text, Text)
formatNode (GraphVizOptions {includeValues}) steps result =
  let exprText = case steps of
                   [] -> "<no steps>"
                   _  -> Text.intercalate "\\n" (map (prettyLayout . fst) steps)
      
      resultText = if includeValues
                     then case result of
                            Left _err -> "Error"
                            Right nf -> prettyLayout nf
                     else ""
      
      separator = if Text.null resultText then "" else "\n────────────────\n"
      label = escapeLabel (exprText <> separator <> resultText)
      
      -- Determine node shape and color based on expression type
      style = case result of
                Left _ -> "[label=\"" <> label <> "\", shape=box, fillcolor=\"#ffcccc\", style=filled]"
                Right _ -> "[label=\"" <> label <> "\", fillcolor=\"#d0e8f2\", style=filled]"
                
  in (label, style)

-- | Check if a node is trivial (just a literal value)
isTrivialNode :: [(Expr Resolved, [EvalTrace])] -> Bool
isTrivialNode [] = True
isTrivialNode [(_, [])] = True  -- Leaf node with no children
isTrivialNode _ = False

-- | Escape special characters in DOT labels
escapeLabel :: Text -> Text
escapeLabel = Text.replace "\"" "\\\"" 
            . Text.replace "\n" "\\n"
            . Text.replace "\\" "\\\\"

-- | Determine edge labels for specific expression forms.
edgeLabelsFor :: Expr Resolved -> [EvalTrace] -> [Maybe Text]
edgeLabelsFor (IfThenElse _ _ _ _) subtraces = labelIf subtraces
  where
    labelIf [] = []
    labelIf [_] = [Nothing]
    labelIf (condTrace:_:rest) =
      let branchLabel = case traceBoolValue condTrace of
                          Just True -> Just "yes"
                          Just False -> Just "no"
                          _ -> Nothing
      in Nothing : branchLabel : replicate (length rest) Nothing
edgeLabelsFor (Consider _ _ branches) subtraces = labelConsider subtraces
  where
    branchLookup =
      [ (expr, branchLabelText branch)
      | branch@(MkBranch _ _ expr) <- branches
      ]

    labelConsider [] = []
    labelConsider (_:rest) =
      Nothing : map labelBranch rest

    labelBranch subTrace =
      case traceHeadExpr subTrace >>= (\expr -> lookup expr branchLookup) of
        Just lbl -> Just lbl
        Nothing -> Nothing
edgeLabelsFor _ subtraces = replicate (length subtraces) Nothing

traceBoolValue :: EvalTrace -> Maybe Bool
traceBoolValue (Trace _ (Right nf)) = nfToBool nf
traceBoolValue _ = Nothing

nfToBool :: NF -> Maybe Bool
nfToBool (MkNF val) = boolView val
nfToBool Omitted = Nothing

traceHeadExpr :: EvalTrace -> Maybe (Expr Resolved)
traceHeadExpr (Trace ((expr, _):_) _) = Just expr
traceHeadExpr _ = Nothing

branchLabelText :: Branch Resolved -> Text
branchLabelText (MkBranch _ lhs _) =
  case lhs of
    When _ pat -> "when " <> prettyLayout pat
    Otherwise _ -> "otherwise"
