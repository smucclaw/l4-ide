{-# LANGUAGE PatternSynonyms #-}
module L4.EvaluateLazy.Trace where

import Base
import qualified Base.DList as DList
import qualified Base.Map as Map
import qualified Base.Text as Text
import L4.Syntax
import L4.Evaluate.ValueLazy
import L4.EvaluateLazy.Machine (EvalException(..), InternalEvalException(..), UserEvalException(..), prettyEvalException)
import L4.Print
import L4.Utils.RevList

import Prettyprinter

-- Note [Lazy evaluation tracing]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The *goal* of lazy evaluation tracing is to produce an evaluation trace
-- that looks roughly like a trace would look in an eager language. In particular,
-- the evaluation of bindings should be in the trace where those bindings are
-- *allocated* (this means, e.g. function args should be evaluated at the call
-- site of the function, and let-bindings should be evaluated where those
-- bindings happen syntactically).
--
-- Of course, lazy evaluation means that some of these might not get evaluated
-- at all. We don't want lazy evaluation traces to cause additional evaluation.
-- Instead, we will mark unevaluated bindings as such.
--
-- In order to get the order correct, we perform reordering of the trace. We
-- remember allocations. Once we force thunks, we also remember that we are doing
-- so. We later go through the trace actions produced during the evaluations
-- and try to figure out where an allocated value has been evaluated and insert
-- that part of the trace at the point of allocation.
--
-- The process proceeds in multiple phases:
--
-- 1. During evaluation, we generate a list of 'EvalTraceAction's. This is
-- the only part that happens during evaluation. It's a design goal that this
-- can be turned off easily, and also that even if it's on, it on its own
-- should not cause a lot of overhead. (It should e.g. be possible to write
-- the trace actions into a file and then have negligible memory and time cost.)
--
-- Current info:
-- @
--   [EvalTraceAction]
-- @
--
-- All the rest of the process happens after evaluation.
--
-- 2. We split the list of 'EvalTraceAction's into several sublist, each of
-- which is associated with an *address*. The split happens at points where we
-- start evaluating a possible thunk. This is done by 'splitEvalTraceActions'.
-- The result of this is a finite map that associates the addresses with the
-- respective sub action lists. The main expression is associated with 'Nothing'.
--
-- Current info:
-- @
--   Map (Maybe Address) (Either WHNF [EvalTraceAction])
-- @
--
-- 3. Every list of actions is turned into a trace with placeholders.
-- For every alloc action, we introduce a placeholder for that address.
-- For every setref action, we introduce a placeholder for the value
-- belonging to that address. We keep WHNFs where eventually NFs are
-- expected.
--
-- @
--   [EvalTraceAction] -> EvalPreTrace
-- @
--
-- 4. We "zonk" the whole structure, by replacing placeholders correspondingly,
-- and turning WHNFs into NFs.
--
-- @
--   Map (Maybe Address) (Either WHNF EvalPreTrace) -> EvalPreTrace -> EvalTrace
-- @
--
-- 5. We simplify the trace by removing some trivial nodes.

-- | 'EvalTraceAction's are optionally generated during evaluation. The idea is that
-- they are themselves not extremely large (i.e., they don't contain unbounded data;
-- we omit any stacks or environments), yet they still should contain sufficient
-- information so that we can recover a meaningful evaluation trace via posprocessing.
--
data EvalTraceAction =
    Enter (Expr Resolved)            -- ^ corresponds to forward, is not always pushing a frame (e.g. tail calls)
  | Exit (Either EvalException WHNF) -- ^ corresponds to backward or exception, is always popping a frame
  | SetRef Reference                 -- ^ can result in evaluation (always pushes a frame) or direct return (immediately exits / pops)
  | Alloc (Expr Resolved) Reference  -- ^ allocation, used for lambdas
  | AllocPre Resolved Reference      -- ^ allocation, used for mutually recursive let/where
  | Push                             -- ^ explicit push
  | Pop                              -- ^ explicit pop (would not be needed / could be combined with Exit)
  deriving stock Show

-- | This instance is primarily used for debugging. It shows individual actions
-- without any postprocessing.
--
instance LayoutPrinter EvalTraceAction where
  printWithLayout :: EvalTraceAction -> Doc ann
  printWithLayout = \ case
    Enter e           -> ">>> " <+> printWithLayout e
    Exit (Right v)    -> "<<< " <+> printWithLayout v
    Exit (Left exc)   -> "*** " <+> vcat (map pretty (prettyEvalException exc))
    SetRef r          -> "!!! " <+> printWithLayout r
    Alloc e r         -> "??? " <+> printWithLayout e <+> printWithLayout r
    AllocPre x r      -> "??p " <+> printWithLayout x <+> "=" <+> printWithLayout r
    Push              -> "+++ "
    Pop               -> "--- "

-- | A pre-trace has the same hierarchical structure as the final trace, but it contains
-- only WHNFs as results, and it contains placeholders with the idea that other parts
-- of the trace should be inserted there.
--
data EvalPreTrace =
    PreTrace [(Expr Resolved, [EvalPreTrace])] (Either EvalException WHNF)
  | PrePlaceholder Address

data EvalTrace =
    Trace (Maybe Resolved) [(Expr Resolved, [EvalTrace])] (Either EvalException NF)
  deriving stock (Generic, Show)
  deriving anyclass NFData

pattern PreValue :: WHNF -> EvalPreTrace
pattern PreValue v = PreTrace [] (Right v)

pattern TraceValue :: NF -> EvalTrace
pattern TraceValue v = Trace Nothing [] (Right v)

-- | Implements step 2 of Note [Lazy evaluation tracing]
splitEvalTraceActions :: [EvalTraceAction] -> Map (Maybe Address) (Either WHNF [EvalTraceAction])
splitEvalTraceActions = go 0 [(0, Nothing, mempty)] Map.empty
  where
    -- In order to split the trace actions into sublists, we need to keep track of
    -- a stack of addresses.
    --
    -- Effectively, every time we encounter a 'SetRef' action, we switch to a new
    -- address, so we push onto our address stack. Every time we notice we're done
    -- with an address, we pop from our address stack and continue with the previous
    -- item on that stack.
    --
    -- The difficulties arise because we have to notice when exactly we're "done"
    -- with a 'SetRef', as unlike the 'SetRef', this isn't *explicitly* marked in
    -- our actions list.
    --
    -- We do this by tracking the depth/size of the original evaluation stack. If
    -- we go past the stack depth at the point where the 'SetRef' occurred, we know
    -- we finished the evaluation of that address.
    --
    go ::    Int                                                 -- ^ track depth of original eval stack
          -> [(Int, Maybe Address, DList EvalTraceAction)]       -- ^ current address stack
          -> Map (Maybe Address) (Either WHNF [EvalTraceAction]) -- ^ accumulated result so far
          -> [EvalTraceAction]                                   -- ^ remaining list of actions to process
          -> Map (Maybe Address) (Either WHNF [EvalTraceAction])
    -- go d1 stack _m actions
    --   | trace (show d1 <> " " <> show ((\ (x, _, _) -> x) <$> stack) <> " " <> show (prettyLayout' <$> take 2 actions)) False = undefined
    go d1 ((d2, ma, casf) : stack) m (a@(SetRef r) : as@(Exit (Right v) : _)) =
      -- If a 'SetRef' is immediately followed by an 'Exit', then we are looking up an
      -- already evaluated address. In this case, we don't have to push or pop from
      -- our address stack at all.
      let
        m' = insertIfMissing (Just r.address) (Left v) m
      in
        go d1 ((d2, ma, casf `DList.snoc` a) : stack) m' as
    go d1 ((d2, ma, casf) : stack) m (a@(SetRef r) : as) =
      -- The normal situation if we encounter a 'SetRef' is to create a new frame on
      -- our address stack with an empty action list.
      go d1 ((d1 + 1, Just r.address, mempty) : (d2, ma, casf `DList.snoc` a) : stack) m as
    go d1 ((d2, ma, casf) : stack) m (a@Push : as) =
      -- A 'Push' increases the tracked depth and is otherwise stored normally.
      go (d1 + 1) ((d2, ma, casf `DList.snoc` a) : stack) m as
    go d1 ((d2, ma, casf) : stack) m (a@Pop : as)
      -- If we encounter a 'Pop', we have to check whether it is the 'Pop' that
      -- closes off our top entry on the address stack. If so, we store it as the
      -- final instruction in that eval action list, pop the address stack, and
      -- continue. If not, we only decrease the tracked depth and continue with
      -- the same address stack.
      | d1 == d2 =
        let
          m' = insertIfMissing ma (Right (toList casf')) m
        in
          go (d1 - 1) stack m' as
      | otherwise =
        go (d1 - 1) ((d2, ma, casf') : stack) m as
      where
        casf' = casf `DList.snoc` a
    go d1 ((d2, ma, casf) : stack) m (a : as) =
      -- The normal case is that we just push encountered actions onto the end of
      -- the action list on the top element of our address stack.
      go d1 ((d2, ma, casf `DList.snoc` a) : stack) m as
    go (-1) [] m [] =
      -- We end at level -1, because we generate a 'Pop' for the final *attempt*
      -- to pop the stack when it is empty.
      m
    go (-1) [] m as@(SetRef _ : _) =
      -- This case occurs if after we're done with the main expression, we
      -- have other subexpressions left to evaluate (because we're computing
      -- the *normal form*, not the weak head normal form). Then we're at
      -- level -1, having tried to pop the empty stack and returned everything,
      -- but we're continuing with a `SetRef` from the next encountered
      -- subexpression we still want to compute. In such a case, we simply
      -- re-initialise to level 0 and a one-element address stack. Reusing
      -- 'Nothing' is fine; we will never overwrite already existing results.
      -- The actual result of this computation will be determined by the
      -- 'SetRef' in the next step.
      --
      go 0 [(0, Nothing, mempty)] m as
    go d stack m as =
      -- In any other situations, we produce an error message.
      error msg
      where
        msg          = header <> "\n" <> depth <> "\n" <> addressStack <> "\n" <> recorded <> "\n" <> actions
        header       = "splitEvalTraceActions: internal error -- encountered an unexpected sequence of lazy eval trace actions"
        depth        = "current stack depth:\n" <> show d <> "\n"
        addressStack = "current address stack:\n" <> debugAddressStack stack <> "\n"
        actions      = "remaining actions:\n" <> debugEvalTraceActionsFromLevel d as <> "\n"
        recorded     = "already recorded actions:\n" <> debugSplitMap m <> "\n"

    insertIfMissing :: Ord k => k -> a -> Map k a -> Map k a
    insertIfMissing = Map.insertWith (\ _new old -> old)

-- | Just used in error messages to produce a reasonably readable version
-- of the split address action map maintained during 'splitEvalTraceActions'.
debugSplitMap :: Map (Maybe Address) (Either WHNF [EvalTraceAction]) -> String
debugSplitMap = intercalate "\n" . map go . Map.toList
  where
    go :: (Maybe Address, Either WHNF [EvalTraceAction]) -> [Char]
    go (ma, r) =
      "for " <> show ma <> "\n" <>
      either prettyLayout' (debugEvalTraceActions . toList) r

-- | Just used in error messages to display the address stack maintained
-- during 'splitEvalTraceActions'.
debugAddressStack :: [(Int, Maybe Address, DList EvalTraceAction)] -> String
debugAddressStack = intercalate "\n" . map go
  where
    go :: (Int, Maybe Address, DList EvalTraceAction) -> String
    go (d, ma, as) =
      "threshold " <> show d <> ", address " <> show ma <> "\n" <>
      debugEvalTraceActionsFromLevel d (toList as)

-- | This shows a full lazy evaluation trace.
instance LayoutPrinter EvalTrace where
  printWithLayout = printEvalTrace 0

-- | Shows a lazy evaluation trace. Keeps track of the level.
printEvalTrace :: forall ann. Int -> EvalTrace -> Doc ann
printEvalTrace lvl = \ case
  Trace _ [] v ->
    pre lvl <> "•" <> " " <> printExceptionOrNF v
  Trace mlabel (esubs : otheresubs) v ->
    let
      proc :: Doc ann -> Doc ann -> Doc ann -> [Doc ann]
      proc firstd followd x =
        case docLines x of
          [] ->
            [pre lvl <> firstd]
          l : ls ->
              (pre lvl <> firstd <> " " <> l)
            : (fmap (\ d -> pre lvl <> followd <> " " <> d) ls)

      proc' firstd followd (e, subs) =
        proc firstd followd (printWithLayout e) <> (printEvalTrace (lvl + 1) <$> subs)

      esubsd      = proc' "┌" "│" esubs
      otheresubsd = proc' "├" "│" <$> otheresubs
      vd          = proc "└" " " (printExceptionOrNF v)
    in
      vcat
        (   esubsd
         <> concat otheresubsd
         <> vd
        )
      <> printEvalTrace lvl (Trace mlabel otheresubs v)
  where
    pre :: Int -> Doc ann
    pre i = pretty (replicate i '│')

-- | Helper function to display an exception or final value in a trace.
printExceptionOrNF :: Either EvalException NF -> Doc ann
printExceptionOrNF (Left e)  = "↯ " <> printEvalExceptionShort e
printExceptionOrNF (Right v) = printWithLayout v

-- | Shows just the kind of the exception, for display in a trace.
printEvalExceptionShort :: EvalException -> Doc ann
printEvalExceptionShort (InternalEvalException e) = printInternalEvalExceptionShort e
printEvalExceptionShort (UserEvalException e)     = printUserEvalExceptionShort e

printInternalEvalExceptionShort :: InternalEvalException -> Doc ann
printInternalEvalExceptionShort (RuntimeScopeError _) = "run-time scope error"
printInternalEvalExceptionShort (RuntimeTypeError _)  = "run-time type error"
printInternalEvalExceptionShort PrematureGC           = "accessed garbage-collected value"
printInternalEvalExceptionShort DanglingPointer       = "dangling pointer"
printInternalEvalExceptionShort UnhandledPatternMatch = "unhandled pattern match"

printUserEvalExceptionShort :: UserEvalException -> Doc ann
printUserEvalExceptionShort (BlackholeForced _)             = "loop detected"
printUserEvalExceptionShort (EqualityOnUnsupportedType _ _) = "called equality on unsupported type"
printUserEvalExceptionShort (NonExhaustivePatterns _)       = "non-exhaustive patterns"
printUserEvalExceptionShort StackOverflow                   = "stack overflow"
printUserEvalExceptionShort (DivisionByZero _)              = "division by zero"
printUserEvalExceptionShort (NotAnInteger _ _)              = "not an integer"
printUserEvalExceptionShort (Stuck _)                       = "stuck"

-- | This is a stack data structure that is maintained while building an 'EvalPreTrace'.
type PreTraceStack = [PreTraceFrame]

-- | These a stack frames maintained while building an 'EvalPreTrace'.
--
-- Each frame contains a partial 'EvalPreTrace' node. It contains a list of
-- expressions and subtraces, and it possibly contains a result or exception
-- already.
--
data PreTraceFrame =
    PreTraceFrame (RevList (Expr Resolved, RevList EvalPreTrace)) (Maybe (Either EvalException WHNF))
  | HiddenFrame

-- | Implements step 3 of Note [Lazy evaluation tracing]
buildEvalPreTrace :: [EvalTraceAction] -> EvalPreTrace
buildEvalPreTrace as = case as of
  Enter _ : _ -> go [PreTraceFrame emptyRevList Nothing] as -- outer expression starts with Enter
  Push : _ -> go [] as -- everything else starts with an update frame
  _ -> error "buildEvalPreTrace: unexpected start of actions"
  where
    go :: PreTraceStack -> [EvalTraceAction] -> EvalPreTrace
    go stack (Push : actions) =
      go (PreTraceFrame emptyRevList Nothing : stack) actions
    go (frame : stack) (Enter e : actions) =
      go (addExprToFrame e frame : stack) actions
    go (frame : stack) (Exit v : actions) =
      go (addResultToFrame v frame : stack) actions
    go (frame : stack) (Pop : actions) =
      case closeFrame frame of
        Nothing -> go stack actions -- hidden frame, just drop
        Just subtrace ->
          case stack of
            [] -> subtrace
            (nextFrame : stack') ->
              go (addSubTraceToFrame subtrace nextFrame : stack') actions
    go (frame : stack) (SetRef _r : actions) =
      go (frame : stack) actions
    go (frame : stack) (Alloc _e r : actions) =
      go (addSubTraceToFrame (PrePlaceholder r.address) frame : stack) actions
    go (frame : stack) (AllocPre _e r : actions) =
      go (addSubTraceToFrame (PrePlaceholder r.address) frame : stack) actions
    go [frame] [] =
      case closeFrame frame of
        Nothing -> error $ "buildEvalPreTrace: top-level trace frame is hidden"
        Just t  -> t
    go frames actions =
      error $ "buildEvalPreTrace: unexpected action sequence: " <> show (length frames, prettyLayout' <$> actions)

    -- Tries to add an expression to the current frame. If the current frame already
    -- has an expression, then it will get another expression. This happens during
    -- tail-calls, where one expression is directly rewritten into another.
    --
    -- We don't expect expressions to be added after we already have a result. In such
    -- a case, we fail.
    --
    addExprToFrame :: Expr Resolved -> PreTraceFrame -> PreTraceFrame
    addExprToFrame e (PreTraceFrame esubs Nothing) = PreTraceFrame (pushRevList (e, emptyRevList) esubs) Nothing
    addExprToFrame _ (PreTraceFrame _ (Just _))    = error "addExprToFrame: unexpected expression after value"
    addExprToFrame _ HiddenFrame                   = HiddenFrame

    -- Add a value or exception to the current frame. We expect this to occur exactly
    -- once, and we expect to close the frame after that.
    addResultToFrame :: (Either EvalException WHNF) -> PreTraceFrame -> PreTraceFrame
    addResultToFrame r (PreTraceFrame esubs Nothing) = PreTraceFrame esubs (Just r)
    addResultToFrame _ (PreTraceFrame _ (Just _))    = error "addValToFrame: double value"
    addResultToFrame _ HiddenFrame                   = HiddenFrame

    -- Close the current frame by turning it into an actual eval pre trace node.
    closeFrame :: PreTraceFrame -> Maybe EvalPreTrace
    closeFrame (PreTraceFrame esubs (Just v)) = Just (PreTrace (second unRevList <$> unRevList esubs) v)
    closeFrame (PreTraceFrame _ Nothing)      = Nothing -- error "closeFrame: trying to close frame without value"
    closeFrame HiddenFrame                    = Nothing

    -- Adds a new sub-trace (subcomputation) to the current frame. This fails if the current
    -- frame already has a result.
    addSubTraceToFrame :: EvalPreTrace -> PreTraceFrame -> PreTraceFrame
    addSubTraceToFrame _ (PreTraceFrame (MkRevList []) Nothing)                   = HiddenFrame
    addSubTraceToFrame t (PreTraceFrame (MkRevList ((e, subs) : esubs')) Nothing) = PreTraceFrame (MkRevList ((e, pushRevList t subs) : esubs')) Nothing
    addSubTraceToFrame _ (PreTraceFrame _ (Just _))                               = error "addSubTraceToFrame: subtrace after value"
    addSubTraceToFrame _ HiddenFrame                                              = HiddenFrame

buildEvalPreTraces :: Map (Maybe Address) (Either WHNF [EvalTraceAction]) -> Map (Maybe Address) (Either WHNF EvalPreTrace)
buildEvalPreTraces = Map.map (bimap id buildEvalPreTrace)

collectTraceLabels :: [EvalTraceAction] -> Map Address Resolved
collectTraceLabels = foldl' go Map.empty
  where
    go acc (AllocPre name ref) = Map.insert ref.address name acc
    go acc _ = acc

-- | Implements step 4 of Note [Lazy evaluation tracing]
buildEvalTrace :: Map Address Resolved -> Map (Maybe Address) (Either WHNF EvalPreTrace) -> Maybe Resolved -> EvalPreTrace -> EvalTrace
buildEvalTrace labels m label (PreTrace esubs w) =
  Trace label (second (fmap (buildEvalTrace labels m Nothing)) <$> esubs) (second (nfFromTrace m) w)
buildEvalTrace labels m label (PrePlaceholder a) =
  let label' =
        case label of
          Just existing -> Just existing
          Nothing -> Map.lookup a labels
  in case Map.lookup (Just a) m of
       Nothing -> Trace label' [] (Right Omitted)
       Just (Left whnf) -> Trace label' [] (Right (nfFromTrace m whnf))
       Just (Right preTrace) -> buildEvalTrace labels m label' preTrace

-- | Implements step 5 of Note [Lazy evaluation tracing]
simplifyEvalTrace :: EvalTrace -> EvalTrace
simplifyEvalTrace (Trace lbl children v) = Trace lbl (second (concatMap go) <$> children) v
  where
    go :: EvalTrace -> [EvalTrace]
    go (Trace _ [] (Right _))              = []   -- eliminate trivial successful trace nodes
    go (Trace _ [(Lit _ _, [])] (Right _)) = []   -- eliminate trivial literal evaluations
    go (Trace _ [(Var _ n, [])] (Right (MkNF (ValConstructor n' []))))
      | getUnique n == getUnique n'      = []   -- eliminate trivial constructor evaluations
    go (Trace _ [(Var _ n, [])] (Right (MkNF (ValUnappliedConstructor n'))))
      | getUnique n == getUnique n'      = []   -- eliminate trivial constructor evaluations
    go (Trace _ [(App _ n [], [])] (Right (MkNF (ValConstructor n' []))))
      | getUnique n == getUnique n'      = []   -- eliminate trivial constructor evaluations
    go (Trace _ [(App _ n [], [])] (Right (MkNF (ValUnappliedConstructor n'))))
      | getUnique n == getUnique n'      = []   -- eliminate trivial constructor evaluations
    go (Trace _ [(Var _ n, [])] _)
      | isInternalName n                 = []   -- eliminate internal function applications
    go (Trace _ [(App _ n _xs, [])] _)
      | isInternalName n                 = []   -- eliminate internal function applications
    go t                                 = [simplifyEvalTrace t]

    -- Heuristically declare functions to be internal that start with double underscores.
    -- TODO: we could be more systematic about this.
    isInternalName :: Resolved -> Bool
    isInternalName r =
      case rawName (getOriginal r) of
        NormalName n -> Text.take 2 n == "__"
        _            -> False

-- | Helper function that turns a WHNF into an NF, but not by actively evaluating,
-- but by looking up the values in the heap of traces.
--
-- This function is similar to 'nf', and both could probably be unified by abstracting
-- over the recursive calls.
--
nfFromTrace :: Map (Maybe Address) (Either WHNF EvalPreTrace) -> WHNF -> NF
nfFromTrace m = \ case
  ValNumber i   -> MkNF (ValNumber i)
  ValString s   -> MkNF (ValString s)
  ValDate day   -> MkNF (ValDate day)
  ValTime tod   -> MkNF (ValTime tod)
  ValDateTime utc tz -> MkNF (ValDateTime utc tz)
  ValNil        -> MkNF ValNil
  ValCons r1 r2 ->
    MkNF (ValCons (rec r1) (rec r2))
  ValClosure givens e env ->
    MkNF (ValClosure givens e env)
  ValObligation env party act due followup lest ->
    MkNF (ValObligation env (fmap (fmap rec) party) act (fmap (fmap rec) due) followup lest)
  ValNullaryBuiltinFun b ->
    MkNF (ValNullaryBuiltinFun b)
  ValUnaryBuiltinFun b ->
    MkNF (ValUnaryBuiltinFun b)
  ValBinaryBuiltinFun b ->
    MkNF (ValBinaryBuiltinFun b)
  ValTernaryBuiltinFun b ->
    MkNF (ValTernaryBuiltinFun b)
  ValPartialTernary b r1 ->
    MkNF (ValPartialTernary b (rec r1))
  ValPartialTernary2 b r1 r2 ->
    MkNF (ValPartialTernary2 b (rec r1) (rec r2))
  ValUnappliedConstructor n ->
    MkNF (ValUnappliedConstructor n)
  ValConstructor n rs ->
    MkNF (ValConstructor n (fmap rec rs))
  ValAssumed n ->
    MkNF (ValAssumed n)
  ValEnvironment env ->
    MkNF (ValEnvironment env)
  ValBreached (DeadlineMissed ev'party ev'act ev'timestamp party act deadline) ->
    MkNF (ValBreached (DeadlineMissed (rec ev'party) (rec ev'act) ev'timestamp (rec party) act deadline))
  ValBreached (ExplicitBreach mParty mReason) ->
    MkNF (ValBreached (ExplicitBreach (fmap rec mParty) (fmap rec mReason)))
  ValROp env op l r ->
    MkNF (ValROp env op (fmap (fmap rec) l) (fmap (fmap rec) r))
  where
    rec :: Reference -> NF
    rec r = rec' r.address

    rec' :: Address -> NF
    rec' a =
      maybe Omitted (either (nfFromTrace m) extractVal) (Map.lookup (Just a) m)

    extractVal :: EvalPreTrace -> NF
    extractVal (PreTrace _ (Left _))  = Omitted
    extractVal (PreTrace _ (Right v)) = nfFromTrace m v
    extractVal (PrePlaceholder a)     = rec' a

-- | This function exists purely for debugging / internal error messages.
--
-- It displays a somewhat readable string from a list of eval trace actions.
-- It additionally tracks the evaluation stack depth.
--
debugEvalTraceActions :: [EvalTraceAction] -> String
debugEvalTraceActions = debugEvalTraceActionsFromLevel 0

-- | This function exists purely for debugging / internal error messages.
--
-- It displays a somewhat readable string from a list of eval trace actions.
-- It additionally tracks the evaluation stack depth. It allows to specify
-- the initial stack depth.
--
debugEvalTraceActionsFromLevel :: Int -> [EvalTraceAction] -> String
debugEvalTraceActionsFromLevel = go
  where
    go d (a@Push : as) = showAt d a <> go (d + 1) as
    go d (a@Pop  : as) = showAt (d - 1) a <> go (d - 1) as
    go d (a      : as) = showAt d a <> go d as
    go _ []            = ""

    showAt d a = show d <> " " <> prettyLayout' a <> "\n"
