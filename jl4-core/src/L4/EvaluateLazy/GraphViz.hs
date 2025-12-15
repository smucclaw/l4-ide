{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module L4.EvaluateLazy.GraphViz (
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

-- | Edge configuration for customizing edge appearance and direction
data EdgeConfig = EdgeConfig
  { edgeLabel :: Maybe Text
    -- ^ Optional label for the edge
  , edgeReversed :: Bool
    -- ^ If True, edge goes from child to parent (for IF conditions)
  }
  deriving stock (Eq, Show)

defaultEdgeConfig :: EdgeConfig
defaultEdgeConfig = EdgeConfig { edgeLabel = Nothing, edgeReversed = False }

getEdgeLabel :: EdgeConfig -> Maybe Text
getEdgeLabel (EdgeConfig lbl _) = lbl

getEdgeReversed :: EdgeConfig -> Bool
getEdgeReversed (EdgeConfig _ rev) = rev

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
  let normalizedTrace = reattachBindings evalTrace
      header = "digraph evaluation_trace {\n"
      config = "  rankdir=TB;\n  node [shape=box, style=rounded];\n"
      (body, _, _) = renderTrace opts 0 0 False normalizedTrace
      footer = "}\n"
  in header <> config <> body <> footer

reattachBindings :: EvalTrace -> EvalTrace
reattachBindings evalTrace =
  let (cleanTrace, bindings) = collectBindings evalTrace
      pruned = pruneBinderEchoes (Map.keysSet bindings) cleanTrace
  in attachBindings bindings Set.empty pruned

pruneBinderEchoes :: Set.Set Unique -> EvalTrace -> EvalTrace
pruneBinderEchoes bindingKeys (Trace mlabel steps result) =
  Trace mlabel (catMaybes (map pruneStep steps)) result
  where
    pruneStep (expr, subtraces) =
      let subtraces' = map (pruneBinderEchoes bindingKeys) subtraces
      in if null subtraces'
            && maybe False (`Set.member` bindingKeys) (exprVarUnique expr)
         then Nothing
         else Just (expr, subtraces')

exprVarUnique :: Expr Resolved -> Maybe Unique
exprVarUnique (App _ name []) = Just (getUnique name)
exprVarUnique _ = Nothing

collectBindings :: EvalTrace -> (EvalTrace, Map.Map Unique EvalTrace)
collectBindings (Trace mlabel steps result) =
  let (steps', bindingMaps) = unzip (map collectStep steps)
  in (Trace mlabel steps' result, mconcat bindingMaps)
  where
    collectStep (expr, subtraces) =
      let (subtraces', maps) = unzip (map collectSubtrace subtraces)
          keptChildren = catMaybes subtraces'
      in ((expr, keptChildren), mconcat maps)

    collectSubtrace child =
      let (child', bindings) = collectBindings child
      in case traceLabel child' of
           Just name ->
             let key = getUnique name
             in (Nothing, Map.insertWith (\_ old -> old) key child' bindings)
           Nothing ->
             (Just child', bindings)

attachBindings :: Map.Map Unique EvalTrace -> Set.Set Unique -> EvalTrace -> EvalTrace
attachBindings bindingMap ancestorLabels (Trace mlabel steps result) =
  let ancestors' =
        case mlabel of
          Nothing -> ancestorLabels
          Just lbl -> Set.insert (getUnique lbl) ancestorLabels
      steps' = map (attachStep ancestors') steps
  in Trace mlabel steps' result
  where
    attachStep ancestors (expr, subtraces) =
      let subtraces' = map (attachBindings bindingMap ancestors) subtraces
          namesInExpr = exprReferenceNames expr
          refKeys = map getUnique namesInExpr
          referenced =
            Set.fromList
              [ key
              | key <- refKeys
              , Map.member key bindingMap
              , not (Set.member key ancestors)
              , all (not . hasLabelUnique key) subtraces'
              ]
          attached =
            [ attachBindings bindingMap (Set.insert key ancestors) childTrace
            | key <- Set.toList referenced
            , Just childTrace <- [Map.lookup key bindingMap]
            ]
      in (expr, subtraces' ++ attached)

    hasLabelUnique key (Trace lbl _ _) =
      maybe False ((== key) . getUnique) lbl

traceLabel :: EvalTrace -> Maybe Resolved
traceLabel (Trace mlabel _ _) = mlabel

exprReferenceNames :: Expr Resolved -> [Resolved]
exprReferenceNames (Where _ body _) = exprReferenceNames body
exprReferenceNames expr = toList expr

-- Returns (DOT text, node emitted?, next available node ID)
renderTrace :: GraphVizOptions -> Int -> Int -> Bool -> EvalTrace -> (Text, Bool, Int)
renderTrace opts@(GraphVizOptions {maxDepth, simplifyTrivial}) depth nodeId forceRender (Trace mlabel steps result) =
  case maxDepth of
    Just maxD | depth >= maxD ->
      -- Hit depth limit, render stub
      let nodeLabel = escapeLabel "... (max depth reached)"
          nodeDef = "  node" <> Text.pack (show nodeId) <> " [label=\"" <> nodeLabel <> "\", style=dashed, color=gray];\n"
      in (nodeDef, True, nodeId + 1)
    _ ->
      let currentNode = nodeId
          currentNodeId = "node" <> Text.pack (show currentNode)
          
          -- Determine node label and styling
          (_nodeLabel, nodeStyle) = formatNode opts depth mlabel steps result
          
          -- Skip this node if it's trivial and simplification is enabled
          shouldSkip = not forceRender && simplifyTrivial && isTrivialNode steps result

          -- Generate node definition
          nodeDef = if shouldSkip
                      then ""
                      else "  " <> currentNodeId <> " " <> nodeStyle <> ";\n"

          -- Render child nodes
          (childDefs, finalNodeId) = renderChildren opts depth currentNode (currentNode + 1) steps

      in (nodeDef <> childDefs, not shouldSkip, finalNodeId)

-- | Render all child steps
renderChildren :: GraphVizOptions -> Int -> Int -> Int -> [(Expr Resolved, [EvalTrace])] -> (Text, Int)
renderChildren _opts _depth _parentNode nodeId [] = ("", nodeId)
renderChildren opts depth parentNode nodeId ((expr, subtraces):rest) =
  let parentNodeId = "node" <> Text.pack (show parentNode)

      -- Get edge configs for this expression type
      edgeConfigs = edgeConfigsFor expr subtraces
      zippedSubtraces = zip subtraces (edgeConfigs ++ repeat defaultEdgeConfig)
      limitedSubtraces = limitListLiteralSubtracesWithConfig expr zippedSubtraces
      filteredSubtraces =
        [ (subtrace, cfg)
        | (subtrace, cfg) <- limitedSubtraces
        , not (shouldHideStructuralSubtrace expr subtrace)
        , not (shouldHideRecursiveFoldSubtrace expr subtrace)
        ]
      keptConfigs = map snd filteredSubtraces
      keptSubtraces = map fst filteredSubtraces
      forceIndices =
        Set.fromList
          [ idx
          | (idx, cfg) <- zip [0..] keptConfigs
          , isJust (getEdgeLabel cfg)
          ]

      -- Render evaluated subtraces
      (childTexts, childRootIds, nextId) =
        renderSubtracesWithConfig opts (depth + 1) nodeId parentNode keptSubtraces keptConfigs forceIndices

      -- Handle IF/THEN/ELSE and CONSIDER: add stub nodes for unevaluated branches
      (stubDefs, stubEdges, finalStubId) = 
        if opts.showUnevaluated
        then case expr of
          IfThenElse _ _cond thenE elseE ->
            let condResult = case subtraces of
                  (condTrace:_) -> traceBoolValue condTrace
                  _ -> Nothing
            in mkIfStubs opts parentNodeId nextId condResult thenE elseE
          Consider _ _scrutinee branches ->
            mkConsiderStubs opts parentNodeId nextId branches subtraces
          _ -> ("", "", nextId)
        else ("", "", nextId)

      -- Generate edges for evaluated subtraces
      mkEdge childIdx cfg =
        let dirAttr = if getEdgeReversed cfg then ", dir=back" else ""
            attrs =
              "[color=\"#2ca02c\", penwidth=2, fontsize=10"
                <> maybe "" (\lbl -> ", label=\"" <> escapeLabel lbl <> "\"") (getEdgeLabel cfg)
                <> dirAttr
                <> "]"
        in "  " <> parentNodeId <> " -> node" <> Text.pack (show childIdx) <> " " <> attrs <> ";\n"

      edges =
        mconcat
          [ mkEdge childIdx cfg
          | (Just childIdx, cfg) <- zip childRootIds keptConfigs
          ]

      -- Render remaining siblings
      (restTexts, finalId) = renderChildren opts depth parentNode finalStubId rest
      
  in (childTexts <> stubDefs <> edges <> stubEdges <> restTexts, finalId)

-- | Create stub nodes for unevaluated IF branches
mkIfStubs :: GraphVizOptions -> Text -> Int -> Maybe Bool -> Expr Resolved -> Expr Resolved -> (Text, Text, Int)
mkIfStubs opts parentNodeId nodeId condResult thenE elseE =
  case condResult of
    Just True ->
      -- THEN was taken, ELSE is unevaluated - create ELSE stub
      let stubLabel = escapeLabel (firstLine (prettyLayout elseE))
          stubDef = "  node" <> Text.pack (show nodeId) <> " [label=\"" <> stubLabel <> "\", fillcolor=\"#e0e0e0\", style=\"filled,dashed\"];\n"
          stubEdge = "  " <> parentNodeId <> " -> node" <> Text.pack (show nodeId) <> " [color=\"#999999\", style=dashed, penwidth=2, fontsize=10, label=\"ELSE\"];\n"
      in if opts.showUnevaluated
           then (stubDef, stubEdge, nodeId + 1)
           else ("", "", nodeId)
    Just False ->
      -- ELSE was taken, THEN is unevaluated - create THEN stub
      let stubLabel = escapeLabel (firstLine (prettyLayout thenE))
          stubDef = "  node" <> Text.pack (show nodeId) <> " [label=\"" <> stubLabel <> "\", fillcolor=\"#e0e0e0\", style=\"filled,dashed\"];\n"
          stubEdge = "  " <> parentNodeId <> " -> node" <> Text.pack (show nodeId) <> " [color=\"#999999\", style=dashed, penwidth=2, fontsize=10, label=\"THEN\"];\n"
      in if opts.showUnevaluated
           then (stubDef, stubEdge, nodeId + 1)
           else ("", "", nodeId)
    Nothing ->
      -- Unknown condition result, no stubs
      ("", "", nodeId)

-- | Create stub nodes for unmatched CONSIDER branches
mkConsiderStubs :: GraphVizOptions -> Text -> Int -> [Branch Resolved] -> [EvalTrace] -> (Text, Text, Int)
mkConsiderStubs opts parentNodeId nodeId branches subtraces
  | not opts.showUnevaluated = ("", "", nodeId)
  | otherwise =
      let -- Find which branch was taken by matching the evaluated body expression by text
          matchedBranchText = case subtraces of
            (_scrutinee : bodyTrace : _) -> 
              fmap (prettyLayout :: Expr Resolved -> Text) (traceHeadExpr bodyTrace)
            _ -> Nothing
          
          -- All branch body expressions by text for lookup
          branchBodiesByText = 
            [ (prettyLayout branchBodyExpr, branch) 
            | branch@(MkBranch _ _ branchBodyExpr) <- branches
            ]
          
          -- Find the matched branch by comparing pretty-printed text
          matchedBranch = matchedBranchText >>= \txt -> lookup txt branchBodiesByText
          
          -- Get unmatched branches (those not taken)
          unmatchedBranches = case matchedBranch of
            Nothing -> []
            Just matched -> filter (/= matched) branches
          
          -- Generate stub node for each unmatched branch
          mkStub curId (MkBranch _ lhs bodyExpr) =
            let stubLabel = escapeLabel (firstLine (prettyLayout bodyExpr))
                branchLabel = case lhs of
                  When _ pat -> "when " <> escapeLabel (prettyLayout pat)
                  Otherwise _ -> "otherwise"
                stubDef = "  node" <> Text.pack (show curId) <> " [label=\"" <> stubLabel <> "\", fillcolor=\"#e0e0e0\", style=\"filled,dashed\"];\n"
                stubEdge = "  " <> parentNodeId <> " -> node" <> Text.pack (show curId) <> " [color=\"#999999\", style=dashed, penwidth=2, fontsize=10, label=\"" <> branchLabel <> "\"];\n"
            in (stubDef, stubEdge, curId + 1)
          
          -- Generate all stub nodes
          (stubDefs, stubEdges, finalId) = foldl' go ("", "", nodeId) unmatchedBranches
            where
              go (accDefs, accEdges, curId) branch =
                let (def, edge, nextId) = mkStub curId branch
                in (accDefs <> def, accEdges <> edge, nextId)
      in (stubDefs, stubEdges, finalId)

-- | Render a list of subtraces with edge configuration
renderSubtracesWithConfig :: GraphVizOptions -> Int -> Int -> Int -> [EvalTrace] -> [EdgeConfig] -> Set.Set Int -> (Text, [Maybe Int], Int)
renderSubtracesWithConfig opts depth nodeId parentNode traces configs forceIdxs = go 0 nodeId traces configs
  where
    go _ curId [] _ = ("", [], curId)
    go idx curId (tr:trs) (cfg:cfgs) =
      let forceChild = Set.member idx forceIdxs
          -- For IF condition subtraces (reversed edges), render children with reversed edges too
          (thisText, emitted, nextId) = renderTraceWithConfig opts depth curId forceChild (getEdgeReversed cfg) parentNode tr
          (restText, restIds, finalId) = go (idx + 1) nextId trs cfgs
          thisIds = if emitted then [Just curId] else [Nothing]
      in (thisText <> restText, thisIds <> restIds, finalId)
    go idx curId (tr:trs) [] =
      let forceChild = Set.member idx forceIdxs
          (thisText, emitted, nextId) = renderTraceWithConfig opts depth curId forceChild False parentNode tr
          (restText, restIds, finalId) = go (idx + 1) nextId trs []
          thisIds = if emitted then [Just curId] else [Nothing]
      in (thisText <> restText, thisIds <> restIds, finalId)

-- | Render a trace, optionally with reversed edge direction for its children
renderTraceWithConfig :: GraphVizOptions -> Int -> Int -> Bool -> Bool -> Int -> EvalTrace -> (Text, Bool, Int)
renderTraceWithConfig opts@(GraphVizOptions {maxDepth, simplifyTrivial}) depth nodeId forceRender reverseChildEdges parentNode (Trace mlabel steps result) =
  case maxDepth of
    Just maxD | depth >= maxD ->
      -- Hit depth limit, render stub
      let nodeLabel = escapeLabel "... (max depth reached)"
          nodeDef = "  node" <> Text.pack (show nodeId) <> " [label=\"" <> nodeLabel <> "\", style=dashed, color=gray];\n"
      in (nodeDef, True, nodeId + 1)
    _ ->
      let currentNode = nodeId
          currentNodeId = "node" <> Text.pack (show currentNode)
          
          -- Determine node label and styling
          (_nodeLabel, nodeStyle) = formatNode opts depth mlabel steps result
          
          -- Skip this node if it's trivial and simplification is enabled
          shouldSkip = not forceRender && simplifyTrivial && isTrivialNode steps result

          -- Generate node definition
          nodeDef = if shouldSkip
                      then ""
                      else "  " <> currentNodeId <> " " <> nodeStyle <> ";\n"

          -- Render child nodes with potentially reversed edges
          (childDefs, finalNodeId) = renderChildrenWithReverse opts depth currentNode (currentNode + 1) reverseChildEdges parentNode steps

      in (nodeDef <> childDefs, not shouldSkip, finalNodeId)

-- | Render children with option to reverse all edges (for IF condition subtrees)
renderChildrenWithReverse :: GraphVizOptions -> Int -> Int -> Int -> Bool -> Int -> [(Expr Resolved, [EvalTrace])] -> (Text, Int)
renderChildrenWithReverse _opts _depth _parentNode nodeId _reverse _grandParent [] = ("", nodeId)
renderChildrenWithReverse opts depth parentNode nodeId reverseAll grandParent ((expr, subtraces):rest) =
  let parentNodeId = "node" <> Text.pack (show parentNode)

      -- Get edge configs, but if we're in a reversed subtree, reverse all edges
      baseConfigs = edgeConfigsFor expr subtraces
      edgeConfigs = if reverseAll
                      then map (\cfg -> cfg { edgeReversed = True }) baseConfigs
                      else baseConfigs
      zippedSubtraces = zip subtraces (edgeConfigs ++ repeat (defaultEdgeConfig { edgeReversed = reverseAll }))
      limitedSubtraces = limitListLiteralSubtracesWithConfig expr zippedSubtraces
      filteredSubtraces =
        [ (subtrace, cfg)
        | (subtrace, cfg) <- limitedSubtraces
        , not (shouldHideStructuralSubtrace expr subtrace)
        , not (shouldHideRecursiveFoldSubtrace expr subtrace)
        ]
      keptConfigs = map snd filteredSubtraces
      keptSubtraces = map fst filteredSubtraces
      forceIndices =
        Set.fromList
          [ idx
          | (idx, cfg) <- zip [0..] keptConfigs
          , isJust (getEdgeLabel cfg)
          ]

      -- Render evaluated subtraces
      (childTexts, childRootIds, nextId) =
        renderSubtracesWithConfig opts (depth + 1) nodeId parentNode keptSubtraces keptConfigs forceIndices

      -- Handle IF/THEN/ELSE and CONSIDER: add stub nodes for unevaluated branches
      (stubDefs, stubEdges, finalStubId) = 
        if opts.showUnevaluated
        then case expr of
          IfThenElse _ _cond thenE elseE ->
            let condResult = case subtraces of
                  (condTrace:_) -> traceBoolValue condTrace
                  _ -> Nothing
            in mkIfStubs opts parentNodeId nextId condResult thenE elseE
          Consider _ _scrutinee branches ->
            mkConsiderStubs opts parentNodeId nextId branches subtraces
          _ -> ("", "", nextId)
        else ("", "", nextId)

      -- Generate edges
      mkEdge childIdx (EdgeConfig mLabel isReversed) =
        let dirAttr = if isReversed then ", dir=back" else ""
            attrs =
              "[color=\"#2ca02c\", penwidth=2, fontsize=10"
                <> maybe "" (\lbl -> ", label=\"" <> escapeLabel lbl <> "\"") mLabel
                <> dirAttr
                <> "]"
        in "  " <> parentNodeId <> " -> node" <> Text.pack (show childIdx) <> " " <> attrs <> ";\n"

      edges =
        mconcat
          [ mkEdge childIdx cfg
          | (Just childIdx, cfg) <- zip childRootIds keptConfigs
          ]

      -- Render remaining siblings
      (restTexts, finalId) = renderChildrenWithReverse opts depth parentNode finalStubId reverseAll grandParent rest
      
  in (childTexts <> stubDefs <> edges <> stubEdges <> restTexts, finalId)

formatNode :: GraphVizOptions -> Int -> Maybe Resolved -> [(Expr Resolved, [EvalTrace])] -> Either EvalException NF -> (Text, Text)
formatNode (GraphVizOptions {includeValues}) depth mlabel steps result =
  let exprText = case steps of
                   [] -> "<no steps>"
                   (firstExpr, _) : _ ->
                     firstLine (summarizeExpr firstExpr)
      callout =
        case (mlabel, depth, firstChildBinder steps) of
          (Nothing, 0, Just childLbl) -> "\n" <> labelToText childLbl
          _ -> ""
      headerText =
        case fmap labelToText mlabel of
          Nothing -> exprText <> callout
          Just lbl ->
            let exprPortion =
                  if Text.null exprText || exprText == "<no steps>"
                    then ""
                    else "\n" <> exprText
            in lbl <> exprPortion <> callout
      
      resultText = if includeValues
                     then case result of
                            Left _err -> "Error"
                            Right nf -> prettyLayout nf
                    else ""
      
      separator = if Text.null resultText then "" else "\n────────────────\n"
      label = escapeLabel (headerText <> separator <> resultText)
      
      -- Determine node shape and color based on expression type
      style = case result of
                Left _ -> "[label=\"" <> label <> "\", shape=box, fillcolor=\"#ffcccc\", style=filled]"
                Right _ -> "[label=\"" <> label <> "\", fillcolor=\"#d0e8f2\", style=filled]"
                
  in (label, style)

-- | Check if a node is trivial (e.g. a literal) and can be elided.
isTrivialNode :: [(Expr Resolved, [EvalTrace])] -> Either EvalException NF -> Bool
isTrivialNode [] _ = True
isTrivialNode [(expr, [])] result =
  isLiteralExpr expr || resultIsFunction result
isTrivialNode _ _ = False

-- | Escape special characters in DOT labels
escapeLabel :: Text -> Text
escapeLabel = Text.replace "\"" "\\\"" 
            . Text.replace "\n" "\\n"
            . Text.replace "\\" "\\\\"

summarizeExpr :: Expr Resolved -> Text
summarizeExpr (Where _ body _) = summarizeExpr body
summarizeExpr expr = prettyLayout expr

firstLine :: Text -> Text
firstLine = Text.takeWhile (/= '\n')

labelToText :: Resolved -> Text
labelToText = nameToText . getOriginal

firstChildBinder :: [(Expr Resolved, [EvalTrace])] -> Maybe Resolved
firstChildBinder steps =
  listToMaybe
    [ lbl
    | (_, subtraces) <- steps
    , tr <- subtraces
    , Just lbl <- [traceLabel tr]
    ]

isLiteralExpr :: Expr Resolved -> Bool
isLiteralExpr (Lit _ _) = True
isLiteralExpr _ = False

resultIsFunction :: Either EvalException NF -> Bool
resultIsFunction (Right nf) = nfIsFunction nf
resultIsFunction _ = False

nfIsFunction :: NF -> Bool
nfIsFunction (MkNF val) = case val of
  ValClosure {} -> True
  ValNullaryBuiltinFun {} -> True
  ValUnaryBuiltinFun {} -> True
  ValBinaryBuiltinFun {} -> True
  ValTernaryBuiltinFun {} -> True
  ValPartialTernary {} -> True
  ValPartialTernary2 {} -> True
  ValUnappliedConstructor {} -> True
  _ -> False
nfIsFunction Omitted = False

-- | Determine edge labels for specific expression forms.
edgeConfigsFor :: Expr Resolved -> [EvalTrace] -> [EdgeConfig]
edgeConfigsFor (IfThenElse _ _ _ _) subtraces = labelIf subtraces
  where
    labelIf [] = []
    labelIf [_] = [EdgeConfig { edgeLabel = Just "IF", edgeReversed = True }]
    labelIf (condTrace:_:rest) =
      let branchLabel = case traceBoolValue condTrace of
                          Just True -> "THEN"
                          Just False -> "ELSE"
                          _ -> "branch"
      in EdgeConfig { edgeLabel = Just "IF", edgeReversed = True }
         : EdgeConfig { edgeLabel = Just branchLabel, edgeReversed = False }
         : replicate (length rest) defaultEdgeConfig
edgeConfigsFor (MultiWayIf _ _ _) subtraces = labelMulti subtraces
  where
    labelMulti [] = []
    labelMulti [_] = [EdgeConfig { edgeLabel = Just "if", edgeReversed = False }]
    labelMulti (_:_:rest) =
      EdgeConfig { edgeLabel = Just "if", edgeReversed = False }
      : EdgeConfig { edgeLabel = Just "then", edgeReversed = False }
      : replicate (length rest) defaultEdgeConfig
edgeConfigsFor (Consider _ _ branches) subtraces = labelConsider subtraces
  where
    branchLookup =
      [ (expr, branchLabelText branch)
      | branch@(MkBranch _ _ expr) <- branches
      ]

    labelConsider [] = []
    labelConsider (_:rest) =
      defaultEdgeConfig : map labelBranch rest

    labelBranch subTrace =
      case traceHeadExpr subTrace >>= (\expr -> lookup expr branchLookup) of
        Just lbl -> EdgeConfig { edgeLabel = Just lbl, edgeReversed = False }
        Nothing -> defaultEdgeConfig
edgeConfigsFor _ subtraces = replicate (length subtraces) defaultEdgeConfig

traceBoolValue :: EvalTrace -> Maybe Bool
traceBoolValue (Trace _ _ (Right nf)) = 
  case nfToBool nf of
    Just b -> Just b
    Nothing -> nfToBoolFromText nf
traceBoolValue _ = Nothing

nfToBool :: NF -> Maybe Bool
nfToBool (MkNF val) = boolView val
nfToBool Omitted = Nothing

-- | Fallback: check the text representation of the normal form for TRUE/FALSE
nfToBoolFromText :: NF -> Maybe Bool
nfToBoolFromText nf =
  let txt = Text.toUpper (prettyLayout nf)
  in if "TRUE" `Text.isInfixOf` txt then Just True
     else if "FALSE" `Text.isInfixOf` txt then Just False
     else Nothing

traceHeadExpr :: EvalTrace -> Maybe (Expr Resolved)
traceHeadExpr (Trace _ ((expr, _):_) _) = Just expr
traceHeadExpr _ = Nothing

shouldHideStructuralSubtrace :: Expr Resolved -> EvalTrace -> Bool
shouldHideStructuralSubtrace (Cons _ _ tailExpr) subtrace =
  isListLiteralExpr tailExpr && traceMatchesExpr tailExpr subtrace
shouldHideStructuralSubtrace _ _ = False

-- | Hide recursive fold subtraces like "sumList OF zs" where the argument
-- is a pattern variable from list deconstruction. These are implementation
-- details that clutter the visualization for laypeople.
-- We detect this by checking if the subtrace's head expression is a function
-- application where the sole argument is a pattern variable (short name).
shouldHideRecursiveFoldSubtrace :: Expr Resolved -> EvalTrace -> Bool
shouldHideRecursiveFoldSubtrace _parentExpr subtrace =
  case traceHeadExpr subtrace of
    Just childExpr -> isRecursiveFoldCall childExpr
    Nothing -> False

-- | Check if an expression looks like a recursive fold call: funcName OF patternVar
-- where patternVar is a short name like "zs", "xs", "list", "rest", etc.
isRecursiveFoldCall :: Expr Resolved -> Bool
isRecursiveFoldCall (App _ _ [Var _ argName]) =
  let argText = nameToText (getOriginal argName)
  in isPatternVarName argText
isRecursiveFoldCall _ = False

-- | Check if a name looks like a pattern variable from list deconstruction
isPatternVarName :: Text -> Bool
isPatternVarName name =
  Text.length name <= 3  -- Short names like "zs", "xs", "n"
  || name `elem` ["list", "rest", "tail", "xs", "ys", "zs"]

data SubtraceKey
  = KeyLabel Resolved
  | KeyExpr (Expr Resolved)
  deriving stock (Eq)

_limitListLiteralSubtraces :: Expr Resolved -> [(EvalTrace, Maybe Text)] -> [(EvalTrace, Maybe Text)]
_limitListLiteralSubtraces (List _ items) subtraces =
  -- NOTE: We intentionally flatten synthetic CONS chains so every list element
  -- becomes a direct child of the LIST node. This improves readability but it
  -- also means DOT no longer preserves the original left-to-right ordering.
  -- If ordering ever matters for a visualization, revisit this resugaring step
  -- and consider emitting a DOT subgraph that forces LR rank just for those
  -- children while leaving the rest of the diagram in TB mode.
  _matchKeys (map exprKeyForItem items) subtraces
_limitListLiteralSubtraces _ subtraces = subtraces

-- | Version of limitListLiteralSubtraces that works with EdgeConfig
limitListLiteralSubtracesWithConfig :: Expr Resolved -> [(EvalTrace, EdgeConfig)] -> [(EvalTrace, EdgeConfig)]
limitListLiteralSubtracesWithConfig (List _ items) subtraces =
  matchKeysWithConfig (map exprKeyForItem items) subtraces
limitListLiteralSubtracesWithConfig _ subtraces = subtraces

_matchKeys :: [SubtraceKey] -> [(EvalTrace, Maybe Text)] -> [(EvalTrace, Maybe Text)]
_matchKeys [] _ = []
_matchKeys _ [] = []
_matchKeys (k:ks) subtraces =
  let (match, remaining) = _pickMatchingSubtrace k subtraces
      rest = _matchKeys ks remaining
  in case match of
       Just pair -> pair : rest
       Nothing -> rest

_pickMatchingSubtrace :: SubtraceKey -> [(EvalTrace, Maybe Text)] -> (Maybe (EvalTrace, Maybe Text), [(EvalTrace, Maybe Text)])
_pickMatchingSubtrace _ [] = (Nothing, [])
_pickMatchingSubtrace key (pair@(tr, _):rest) =
  case subtraceKey tr of
    Just k | k == key -> (Just pair, rest)
    _ ->
      let (found, rest') = _pickMatchingSubtrace key rest
      in (found, pair : rest')

matchKeysWithConfig :: [SubtraceKey] -> [(EvalTrace, EdgeConfig)] -> [(EvalTrace, EdgeConfig)]
matchKeysWithConfig [] _ = []
matchKeysWithConfig _ [] = []
matchKeysWithConfig (k:ks) subtraces =
  let (match, remaining) = pickMatchingSubtraceWithConfig k subtraces
      rest = matchKeysWithConfig ks remaining
  in case match of
       Just pair -> pair : rest
       Nothing -> rest

pickMatchingSubtraceWithConfig :: SubtraceKey -> [(EvalTrace, EdgeConfig)] -> (Maybe (EvalTrace, EdgeConfig), [(EvalTrace, EdgeConfig)])
pickMatchingSubtraceWithConfig _ [] = (Nothing, [])
pickMatchingSubtraceWithConfig key (pair@(tr, _):rest) =
  case subtraceKey tr of
    Just k | k == key -> (Just pair, rest)
    _ ->
      let (found, rest') = pickMatchingSubtraceWithConfig key rest
      in (found, pair : rest')

exprKeyForItem :: Expr Resolved -> SubtraceKey
exprKeyForItem expr =
  case stripWhere expr of
    Var _ name -> KeyLabel name
    other -> KeyExpr other

subtraceKey :: EvalTrace -> Maybe SubtraceKey
subtraceKey tr =
  case traceLabel tr of
    Just lbl -> Just (KeyLabel lbl)
    Nothing ->
      KeyExpr . stripWhere <$> traceHeadExpr tr

traceMatchesExpr :: Expr Resolved -> EvalTrace -> Bool
traceMatchesExpr expected subtrace =
  case traceHeadExpr subtrace of
    Just expr -> stripWhere expr == stripWhere expected
    Nothing -> False

stripWhere :: Expr Resolved -> Expr Resolved
stripWhere (Where _ body _) = stripWhere body
stripWhere expr = expr

isListLiteralExpr :: Expr Resolved -> Bool
isListLiteralExpr expr =
  case stripWhere expr of
    List _ _ -> True
    _ -> False

branchLabelText :: Branch Resolved -> Text
branchLabelText (MkBranch _ lhs _) =
  case lhs of
    When _ pat -> "when " <> prettyLayout pat
    Otherwise _ -> "otherwise"
