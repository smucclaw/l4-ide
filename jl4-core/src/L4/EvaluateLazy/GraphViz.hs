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

      -- Render each subtrace
      edgeLabels = edgeLabelsFor expr subtraces
      zippedSubtraces = zip subtraces (edgeLabels ++ repeat Nothing)
      limitedSubtraces = limitListLiteralSubtraces expr zippedSubtraces
      filteredSubtraces =
        [ (subtrace, lbl)
        | (subtrace, lbl) <- limitedSubtraces
        , not (shouldHideStructuralSubtrace expr subtrace)
        ]
      keptLabels = map snd filteredSubtraces
      keptSubtraces = map fst filteredSubtraces
      forceIndices =
        Set.fromList
          [ idx
          | (idx, Just _) <- zip [0..] keptLabels
          ]
      (childTexts, childRootIds, nextId) =
        renderSubtraces opts (depth + 1) nodeId keptSubtraces forceIndices

      mkEdge childIdx mLabel =
        let attrs =
              "[color=\"#2ca02c\", penwidth=2"
                <> maybe "" (\lbl -> ", label=\"" <> escapeLabel lbl <> "\"") mLabel
                <> "]"
        in "  " <> parentNodeId <> " -> node" <> Text.pack (show childIdx) <> " " <> attrs <> ";\n"

      edges =
        mconcat
          [ mkEdge childIdx lbl
          | (Just childIdx, lbl) <- zip childRootIds keptLabels
          ]

      -- Render remaining siblings
      (restTexts, finalId) = renderChildren opts depth parentNode nextId rest
      
  in (childTexts <> edges <> restTexts, finalId)

-- | Render a list of subtraces
renderSubtraces :: GraphVizOptions -> Int -> Int -> [EvalTrace] -> Set.Set Int -> (Text, [Maybe Int], Int)
renderSubtraces opts depth nodeId traces forceIdxs = go 0 nodeId traces
  where
    go _ curId [] = ("", [], curId)
    go idx curId (tr:trs) =
      let forceChild = Set.member idx forceIdxs
          (thisText, emitted, nextId) = renderTrace opts depth curId forceChild tr
          (restText, restIds, finalId) = go (idx + 1) nextId trs
          thisIds = if emitted then [Just curId] else [Nothing]
      in (thisText <> restText, thisIds <> restIds, finalId)

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
edgeLabelsFor (MultiWayIf _ _ _) subtraces = labelMulti subtraces
  where
    labelMulti [] = []
    labelMulti [_] = [Just "if"]
    labelMulti (_:_:rest) =
      Just "if" : Just "then" : replicate (length rest) Nothing
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
traceBoolValue (Trace _ _ (Right nf)) = nfToBool nf
traceBoolValue _ = Nothing

nfToBool :: NF -> Maybe Bool
nfToBool (MkNF val) = boolView val
nfToBool Omitted = Nothing

traceHeadExpr :: EvalTrace -> Maybe (Expr Resolved)
traceHeadExpr (Trace _ ((expr, _):_) _) = Just expr
traceHeadExpr _ = Nothing

shouldHideStructuralSubtrace :: Expr Resolved -> EvalTrace -> Bool
shouldHideStructuralSubtrace (Cons _ _ tailExpr) subtrace =
  isListLiteralExpr tailExpr && traceMatchesExpr tailExpr subtrace
shouldHideStructuralSubtrace _ _ = False

data SubtraceKey
  = KeyLabel Resolved
  | KeyExpr (Expr Resolved)
  deriving stock (Eq)

limitListLiteralSubtraces :: Expr Resolved -> [(EvalTrace, Maybe Text)] -> [(EvalTrace, Maybe Text)]
limitListLiteralSubtraces (List _ items) subtraces =
  -- NOTE: We intentionally flatten synthetic CONS chains so every list element
  -- becomes a direct child of the LIST node. This improves readability but it
  -- also means DOT no longer preserves the original left-to-right ordering.
  -- If ordering ever matters for a visualization, revisit this resugaring step
  -- and consider emitting a DOT subgraph that forces LR rank just for those
  -- children while leaving the rest of the diagram in TB mode.
  matchKeys (map exprKeyForItem items) subtraces
limitListLiteralSubtraces _ subtraces = subtraces

matchKeys :: [SubtraceKey] -> [(EvalTrace, Maybe Text)] -> [(EvalTrace, Maybe Text)]
matchKeys [] _ = []
matchKeys _ [] = []
matchKeys (k:ks) subtraces =
  let (match, remaining) = pickMatchingSubtrace k subtraces
      rest = matchKeys ks remaining
  in case match of
       Just pair -> pair : rest
       Nothing -> rest

pickMatchingSubtrace :: SubtraceKey -> [(EvalTrace, Maybe Text)] -> (Maybe (EvalTrace, Maybe Text), [(EvalTrace, Maybe Text)])
pickMatchingSubtrace _ [] = (Nothing, [])
pickMatchingSubtrace key (pair@(tr, _):rest) =
  case subtraceKey tr of
    Just k | k == key -> (Just pair, rest)
    _ ->
      let (found, rest') = pickMatchingSubtrace key rest
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
