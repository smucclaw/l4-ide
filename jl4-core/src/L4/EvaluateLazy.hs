{-# LANGUAGE GADTs #-}
module L4.EvaluateLazy
( EvalDirectiveResult (..)
, execEvalModuleWithEnv
, execEvalExprInContextOfModule
, prettyEvalException
, prettyEvalDirectiveResult
)
where

import Base
import qualified Base.Map as Map
import qualified Base.Set as Set
import qualified Base.Text as Text
import L4.EvaluateLazy.Machine
import L4.Evaluate.ValueLazy
import L4.Parser.SrcSpan (SrcRange)
import L4.Annotation
import L4.Print
import L4.Syntax
import L4.Utils.RevList

import Control.Concurrent
import Data.Bifunctor
import Prettyprinter

-----------------------------------------------------------------------------
-- The Eval monad and the required types for the monad
-----------------------------------------------------------------------------
data EvalState =
  MkEvalState
    { moduleUri :: !NormalizedUri
    , stack     :: !(IORef Stack)
    , supply    :: !(IORef Int)   -- used for uniques and addresses
    , evalTrace :: !(Maybe (IORef ([EvalTraceAction] -> [EvalTraceAction])))
    }

newtype Eval a = MkEval (EvalState -> IO (Either EvalException a))
  deriving (Functor, Applicative, Monad, MonadError EvalException, MonadReader EvalState, MonadIO)
    via ExceptT EvalException (ReaderT EvalState IO)

-----------------------------------------------------------------------------
-- Helper functions for the Eval Monad
-----------------------------------------------------------------------------

step :: Eval Int
step = do
  i <- readRef (.supply)
  writeRef (.supply) $! i + 1
  pure i

newUnique :: Eval Unique
newUnique = do
  i <- step
  u <- asks (.moduleUri)
  pure (MkUnique 'e' i u)

readRef :: (EvalState -> IORef a) -> Eval a
readRef r = asks r >>= liftIO . readIORef

writeRef :: (EvalState -> IORef a) -> a -> Eval ()
writeRef r !x = asks r >>= liftIO . flip writeIORef x

pushFrame :: Frame -> Eval ()
pushFrame frame = do
  s <- readRef (.stack)
  if s.size == maximumStackSize
    then exception $ UserEvalException StackOverflow
    else writeRef (.stack) (over #frames (frame :) s)

-- | Pops a stack frame (if any are left) and calls the continuation on it.
withPoppedFrame :: (Maybe Frame -> Eval a) -> Eval a
withPoppedFrame k = do
  stack <- readRef (.stack)
  case stack.frames of
    []       -> k Nothing
    (f : fs) -> do
      writeRef (.stack) (MkStack { size = stack.size - 1, frames = fs })
      k (Just f)

-- | For the time being, exceptions are always fatal. But we could
-- in principle have exception we can recover from ...
exception :: EvalException -> Eval a
exception exc =
  withPoppedFrame $ \ case
    Nothing -> throwError exc
    Just _f -> exception exc

tryEval :: Eval a -> Eval (Either EvalException a)
tryEval = tryError

lookupAndUpdateRef :: Reference -> (Thunk -> (Thunk, a)) -> Eval a
lookupAndUpdateRef rf f =
  liftIO $
    atomicModifyIORef' rf.pointer f

updateRef :: Reference -> Thunk -> Eval ()
updateRef rf a = lookupAndUpdateRef rf $ const (a, ())

newReference :: Eval Reference
newReference = do
  address <- newAddress
  reference <- liftIO (myThreadId >>= newIORef . blackhole)
  pure (MkReference address reference)

blackhole :: ThreadId -> Thunk
blackhole tid =
  Unevaluated (Set.singleton tid) (error "blackhole") Map.empty



-----------------------------------------------------------------------------
-- The Stack of the machine
-----------------------------------------------------------------------------

data Stack =
  MkStack
    { size   :: !Int
    , frames :: [Frame]
    }
  deriving stock (Generic, Show)

emptyStack :: Stack
emptyStack = MkStack 0 []

newAddress :: Eval Address
newAddress = do
  i <- step
  u <- asks (.moduleUri)
  pure (MkAddress u i)

interpMachine :: Machine a -> Eval a
interpMachine = \ case
  Config a -> pure a
  Exception e -> exception e
  Allocate' alloc -> case alloc of
    Recursive expr env -> do
      rf <- newReference
      let env' = env rf
      updateRef rf $ Unevaluated Set.empty expr env'
      traceEval (Alloc expr rf)
      pure (rf, env')
    Value whnf -> do
      rf <- newReference
      updateRef rf $ WHNF whnf
      -- we don't trace this because it is used for initial environment stuff which is misleading in the trace
      -- traceEval (AllocVal whnf rf)
      pure rf
    PreAllocation r -> do
      rf <- newReference
      traceEval (AllocPre r rf)
      pure (getUnique r, rf)
  WithPoppedFrame k -> do
    traceEval Pop
    withPoppedFrame (interpMachine . k)
  PokeThunk rf k -> do
    tid <- liftIO myThreadId
    conf <- lookupAndUpdateRef rf (k tid)
    interpMachine $ pure conf
  Bind act k -> interpMachine act >>= interpMachine . k
  PushFrame f -> do
    traceEval Push
    pushFrame f
  NewUnique -> newUnique

data EvalTraceAction =
    Enter (Expr Resolved) -- ^ corresponds to forward, is not always pushing a frame (e.g. tail calls)
  | Exit WHNF -- ^ corresponds to backward, is always popping a frame
  | SetRef Reference -- ^ can result in evaluation (always pushes a frame) or direct return (immediately exits / pops)
  | Alloc (Expr Resolved) Reference -- used for lambda
  -- | AllocVal WHNF Reference
  | AllocPre Resolved Reference -- used for mutually recursive let/where
  | Push -- ^ explicit push
  | Pop -- ^ explicit pop (would not be needed / could be combined with Exit)
  deriving stock Show

-- | This instance is primarily used for debugging. It shows individual actions
-- without any postprocessing.
--
instance LayoutPrinter EvalTraceAction where
  printWithLayout = \ case
    Enter e      -> ">>> " <+> printWithLayout e
    Exit v       -> "<<< " <+> printWithLayout v
    SetRef r     -> "!!! " <+> printWithLayout r
    Alloc e r    -> "??? " <+> printWithLayout e <+> printWithLayout r
    -- AllocVal v r -> "??< " <+> printWithLayout v <+> printWithLayout r
    AllocPre x r -> "??p " <+> printWithLayout x <+> "=" <+> printWithLayout r
    Push         -> "+++ "
    Pop          -> "--- "

-- | This shows a full trace.
instance LayoutPrinter EvalTrace where
  printWithLayout = printEvalTrace 0

printEvalTrace :: forall ann. Int -> EvalTrace -> Doc ann
printEvalTrace lvl = \ case
  Trace [] v ->
    printEvalTrace lvl (TraceValue v)
  Trace (esubs : otheresubs) v -> -- TODO: otheresubs ignored
    let
      proc :: forall a. LayoutPrinter a => Doc ann -> Doc ann -> a -> [Doc ann]
      proc firstd followd x =
        case docLines x of
          [] ->
            [pre lvl <> firstd]
          l : ls ->
              (pre lvl <> firstd <> " " <> l)
            : (fmap (\ d -> pre lvl <> followd <> " " <> d) ls)

      proc' firstd followd (e, subs) =
        proc firstd followd e <> (printEvalTrace (lvl + 1) <$> subs)

      esubsd      = proc' "┌" "│" esubs
      otheresubsd = proc' "├" "│" <$> otheresubs
      vd          = proc "└" " " v

    in
      vcat $
           esubsd
        <> concat otheresubsd
        <> vd
  TraceValue v ->
    pre lvl <> "•" <> " " <> printWithLayout v
  where
    pre :: Int -> Doc ann
    pre i = pretty (replicate i '│')


traceEval :: EvalTraceAction -> Eval ()
traceEval ta = do
  mtr <- asks (.evalTrace)
  case mtr of
    Nothing -> pure ()
    Just tr -> liftIO (modifyIORef' tr (. (ta :)))

-- | For the given eval action, enable tracing and accumulate a trace.
--
-- We try to make it so that in principle, nested calls to `captureTrace`
-- will yield the correct result.
--
captureTrace :: Eval a -> Eval (a, [EvalTraceAction])
captureTrace m = do
  mtr <- asks (.evalTrace) -- save old state
  ntr <- liftIO (newIORef id)
  r <- local (\ s -> s { evalTrace = Just ntr }) m
  tas <- liftIO (($ []) <$> readIORef ntr)
  combine mtr tas -- combine our trace with old trace if it was active
  pure (r, tas)
  where
    combine Nothing   _   = pure ()
    combine (Just tr) tas = liftIO (modifyIORef' tr (. (tas ++)))

-- data EvalTrace =
--   Trace (Expr Resolved) [EvalTrace] (Either EvalException NF)

-- What is the strategy?
--
-- 1. We associate the eval actions with addresses, splitting the list
-- at SetRef actions.
--
-- Map (Maybe Address) [EvalTraceAction]
--
-- 2. Every list of actions is turned into a trace with placeholders.
-- For every alloc action, we introduce a placeholder for that address.
-- For every setref action, we introduce a placeholder for the value
-- belonging to that address. We keep WHNFs where eventually NFs are
-- expected.
--
-- (Map Address [EvalTraceAction], [EvalTraceAction]) -> (Map Address PreTrace, PreTrace)
--
-- 3. We "zonk" the whole structure, by replacing placeholders correspondingly,
-- and turning WHNFs into NFs.
--
-- (Map Address PreTrace, PreTrace) -> Trace
--
--
-- 0 >>>  test
--   !!!  &14@file://experiments/trace.l4
-- 1 >>>  inc OF (2 PLUS 2)
--   ???  2 PLUS 2 &16@file://experiments/trace.l4
-- 2 >>>  inc
--   !!!  &13@file://experiments/trace.l4
-- 2 <<<  <function>
-- 2 >>>  x PLUS 1
-- 3 >>>  x
--   !!!  &16@file://experiments/trace.l4
-- 4 >>>  2 PLUS 2
-- 5 >>>  2
-- 5 <<<  2
-- 5 >>>  2
-- 5 <<<  2
-- 4 <<<  4
-- 3 <<<  4
-- 3 >>>  1
-- 3 <<<  1
-- 2 <<<  5
-- 1 <<<  5
-- 0 <<<  5
--
-- Nothing: final value 5
--
-- 0 >>>  test
-- 1 !!!  &14
-- 0 <<<  5
--
-- &14: final value 5
--
-- 1 >>>  inc OF (2 PLUS 2)
-- 2 ???  2 PLUS 2 &16
-- 2 >>>  inc
-- 3 !!!  &13
-- 2 <<<  <function>
-- 2 >>>  x PLUS 1
-- 3 >>>  x
--   !!!  &16
-- 3 <<<  4
-- 3 >>>  1
-- 3 <<<  1
-- 2 <<<  5
-- 1 <<<  5
--
-- &13: final value <function>
--
-- (empty)
--
-- &16: final value 4
--
-- 4 >>>  2 PLUS 2
-- 5 >>>  2
-- 5 <<<  2
-- 5 >>>  2
-- 5 <<<  2
-- 4 <<<  4
--
--

--
splitEvalTraceActions :: [EvalTraceAction] -> Map (Maybe Address) (Either WHNF [EvalTraceAction])
splitEvalTraceActions = go 0 [(0, Nothing, id)] Map.empty
  where
    go ::    Int
          -> [(Int, Maybe Address, [EvalTraceAction] -> [EvalTraceAction])]
          -> Map (Maybe Address) (Either WHNF [EvalTraceAction])
          -> [EvalTraceAction]
          -> Map (Maybe Address) (Either WHNF [EvalTraceAction])
    -- go d1 stack _m actions
    --   | trace (show d1 <> " " <> show ((\ (x, _, _) -> x) <$> stack) <> " " <> show (prettyLayout' <$> take 2 actions)) False = undefined
    go d1 [] m (_a@(SetRef r) : as@(Exit v : _)) =
      let
        m' = Map.insertWith (\ _new old -> old) (Just r.address) (Left v) m
      in
        go d1 [] m' as
    go d1 [] m (_a@(SetRef r) : as) =
      go d1 ((d1 + 1, Just r.address, id) : []) m as
    go d1 ((d2, ma, casf) : stack) m (a@(SetRef r) : as@(Exit v : _)) =
      let
        m' = Map.insertWith (\ _new old -> old) (Just r.address) (Left v) m
      in
        go d1 ((d2, ma, casf . (a :)) : stack) m' as
    go d1 ((d2, ma, casf) : stack) m (a@(SetRef r) : as) =
      go d1 ((d1 + 1, Just r.address, id) : (d2, ma, casf . (a :)) : stack) m as
    go d1 ((d2, ma, casf) : stack) m (a@Push : as) =
      go (d1 + 1) ((d2, ma, casf . (a :)) : stack) m as
    go d1 ((d2, ma, casf) : stack) m (a1@(Exit _) : a2@Pop : as)
      | d1 == d2 =
        let
          m' = Map.insertWith (\ _new old -> old) ma (Right ((casf . (a1 :) . (a2 :)) [])) m
        in
          go (d1 - 1) stack m' as
      | otherwise =
        go (d1 - 1) ((d2, ma, casf . (a1 :) . (a2 :)) : stack) m as
    go d1 ((d2, ma, casf) : stack) m (a@Pop : as)
      | d1 == d2 =
        let
          m' = Map.insertWith (\ _new old -> old) ma (Right ((casf . (a :)) [])) m
        in
          go (d1 - 1) stack m' as
      | otherwise =
        go (d1 - 1) ((d2, ma, casf . (a :)) : stack) m as
    go d1 ((d2, ma, casf) : stack) m (a : as) =
      go d1 ((d2, ma, casf . (a :)) : stack) m as
    -- go 0 [] m (Exit _ : Pop : []) = m -- not good, we drop the exit
    -- go 0 [] m [Pop] = m
    go (-1) [] m [] = m
    go (-1) [] m (Exit _ : Pop : actions) = go (-1) [] m actions
    go d stack _m as = error $ "splitEvalTraceActions: internal error: " <> show (d, length stack, length as, show as)

data EvalPreTrace =
    PreTrace [(Expr Resolved, [EvalPreTrace])] WHNF
  | PrePlaceholder Address
  | PreValue WHNF

data EvalTrace =
    Trace [((Expr Resolved), [EvalTrace])] NF
  | TraceValue NF
  deriving stock (Generic, Show)
  deriving anyclass NFData

type PreTraceStack = [PreTraceFrame]

data PreTraceFrame =
    PreTraceFrame (RevList (Expr Resolved, RevList EvalPreTrace)) (Maybe WHNF)
  | HiddenFrame

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
      go (addValToFrame v frame : stack) actions
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

    addExprToFrame :: Expr Resolved -> PreTraceFrame -> PreTraceFrame
    addExprToFrame e (PreTraceFrame esubs Nothing) = PreTraceFrame (pushRevList (e, emptyRevList) esubs) Nothing
    addExprToFrame _ (PreTraceFrame _ (Just _))    = error "addExprToFrame: unexpected expression after value"
    addExprToFrame _ HiddenFrame                   = HiddenFrame

    addValToFrame :: WHNF -> PreTraceFrame -> PreTraceFrame
    addValToFrame v (PreTraceFrame esubs Nothing) = PreTraceFrame esubs (Just v)
    addValToFrame _ (PreTraceFrame _ (Just _))    = error "addValToFrame: double value"
    addValToFrame _ HiddenFrame                   = HiddenFrame

    closeFrame :: PreTraceFrame -> Maybe EvalPreTrace
    closeFrame (PreTraceFrame esubs (Just v)) = Just (PreTrace (second unRevList <$> unRevList esubs) v)
    closeFrame (PreTraceFrame _ Nothing)      = Nothing -- error "closeFrame: trying to close frame without value"
    closeFrame HiddenFrame                    = Nothing

    addSubTraceToFrame :: EvalPreTrace -> PreTraceFrame -> PreTraceFrame
    addSubTraceToFrame _ (PreTraceFrame (MkRevList []) Nothing)                   = HiddenFrame
    addSubTraceToFrame t (PreTraceFrame (MkRevList ((e, subs) : esubs')) Nothing) = PreTraceFrame (MkRevList ((e, pushRevList t subs) : esubs')) Nothing
    addSubTraceToFrame _ (PreTraceFrame _ (Just _))                               = error "addSubTraceToFrame: subtrace after value"
    addSubTraceToFrame _ HiddenFrame                                              = HiddenFrame

buildEvalPreTraces :: Map (Maybe Address) (Either WHNF [EvalTraceAction]) -> Map (Maybe Address) (Either WHNF EvalPreTrace)
buildEvalPreTraces = Map.map (bimap id buildEvalPreTrace)

buildEvalTrace :: Map (Maybe Address) (Either WHNF EvalPreTrace) -> EvalPreTrace -> EvalTrace
buildEvalTrace m (PreTrace esubs w)  = Trace (second (fmap (buildEvalTrace m)) <$> esubs) (nfFromTrace m w)
buildEvalTrace m (PrePlaceholder a)  = maybe (TraceValue Omitted) (either (TraceValue . nfFromTrace m) (buildEvalTrace m)) (Map.lookup (Just a) m)
buildEvalTrace m (PreValue v)        = TraceValue (nfFromTrace m v)

nfFromTrace :: Map (Maybe Address) (Either WHNF EvalPreTrace) -> WHNF -> NF
nfFromTrace m = \ case
  ValNumber i   -> MkNF (ValNumber i)
  ValString s   -> MkNF (ValString s)
  ValNil        -> MkNF ValNil
  ValCons r1 r2 ->
    MkNF (ValCons (rec r1) (rec r2))
  ValClosure givens e env ->
    MkNF (ValClosure givens e env)
  ValObligation env party act due followup lest ->
    MkNF (ValObligation env (fmap (fmap rec) party) act (fmap (fmap rec) due) followup lest)
  ValUnaryBuiltinFun b ->
    MkNF (ValUnaryBuiltinFun b)
  ValBinaryBuiltinFun b ->
    MkNF (ValBinaryBuiltinFun b)
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
  ValROp env op l r ->
    MkNF (ValROp env op (fmap (fmap rec) l) (fmap (fmap rec) r))
  where
    rec :: Reference -> NF
    rec r = rec' r.address

    rec' :: Address -> NF
    rec' a =
      maybe Omitted (either (nfFromTrace m) extractVal) (Map.lookup (Just a) m)

    extractVal :: EvalPreTrace -> NF
    extractVal (PreTrace _ v)     = nfFromTrace m v
    extractVal (PrePlaceholder a) = rec' a
    extractVal (PreValue v)       = nfFromTrace m v

runConfig :: Config -> Eval WHNF
runConfig = \ case
  ForwardMachine env expr -> do
    traceEval (Enter expr)
    next <- interpMachine (forwardExpr env expr)
    runConfig next
  MatchBranchesMachine scrutinee env branches -> do
    next <- interpMachine (matchBranches scrutinee env branches)
    runConfig next
  MatchPatternMachine r env pat -> do
    next <- interpMachine (matchPattern r env pat)
    runConfig next
  BackwardMachine whnf -> do
    traceEval (Exit whnf)
    next <- interpMachine (backward whnf)
    runConfig next
  EvalRefMachine r -> do
    traceEval (SetRef r)
    next <- interpMachine (evalRef r)
    runConfig next
  DoneMachine whnf ->
    pure whnf

-- | Evaluate an EVAL directive. For this, we evaluate to normal form,
-- not just WHNF.
nfDirective :: EvalDirective -> Eval EvalDirectiveResult
nfDirective (MkEvalDirective r traced expr env) = do
  (v, mt) <-
    if traced
      then second Just <$> do
        captureTrace $ tryEval $ do
          whnf <- runConfig $ ForwardMachine env expr
          nf whnf
      else fmap (, Nothing) $ tryEval $ do
        whnf <- runConfig $ ForwardMachine env expr
        nf whnf
  let finalTrace = postprocessTrace <$> mt
  pure (MkEvalDirectiveResult r v finalTrace)

postprocessTrace :: [EvalTraceAction] -> EvalTrace
postprocessTrace actions =
  let
    splitActions = splitEvalTraceActions actions
    tracedHeap = buildEvalPreTraces splitActions
    mainTrace = case Map.lookup Nothing tracedHeap of
                  Nothing -> err
                  Just t  -> t
    err = error "postprocessTrace: no trace for main value"
    finalTrace = buildEvalTrace tracedHeap (either err id mainTrace)
  in
    finalTrace

{-
debugEvalActions :: [EvalTraceAction] -> IO ()
debugEvalActions = go 0
  where
    go :: Int -> [EvalTraceAction] -> IO ()
    go d (a@Push : as) = showAt d a >> go (d + 1) as
    go d (a@Pop  : as) = showAt (d - 1) a >> go (d - 1) as
    go d (a      : as) = showAt d a >> go d as
    go _ []            = pure ()

    showAt d a = putStrLn (show d <> " " <> prettyLayout' a)
-}

data EvalDirectiveResult =
  MkEvalDirectiveResult
    { range  :: Maybe SrcRange -- ^ of the (L)EVAL / PROVISION directive
    , result :: Either EvalException NF
    , trace  :: Maybe EvalTrace
    }
  deriving stock (Generic, Show)
  deriving anyclass NFData

-- | Prints the results but not the range of an eval directive, including
-- the trace if present.
--
prettyEvalDirectiveResult :: EvalDirectiveResult -> Text
prettyEvalDirectiveResult (MkEvalDirectiveResult _range res mtrace) =
   either (Text.unlines . prettyEvalException) prettyLayout res
   <> case mtrace of
        Nothing -> Text.empty
        Just t  -> "\n─────\n" <> prettyLayout t

-- | Evaluate WHNF to NF, with a cutoff (which possibly could be made configurable).
nf :: WHNF -> Eval NF
nf = nfAux maximumStackSize

nfAux :: Int -> WHNF -> Eval NF
nfAux  d _v | d < 0                  = pure Omitted
nfAux _d (ValNumber i)               = pure (MkNF (ValNumber i))
nfAux _d (ValString s)               = pure (MkNF (ValString s))
nfAux _d ValNil                      = pure (MkNF ValNil)
nfAux  d (ValCons r1 r2)             = do
  v1 <- evalAndNF d r1
  v2 <- evalAndNF d r2
  pure (MkNF (ValCons v1 v2))
nfAux _d (ValClosure givens e env)   = pure (MkNF (ValClosure givens e env))
nfAux d (ValObligation env party act due followup lest) = do
  party' <- traverseAndNF d party
  due' <- traverseAndNF d due
  pure (MkNF (ValObligation env party' act due' followup lest))
nfAux _d (ValUnaryBuiltinFun b)      = pure (MkNF (ValUnaryBuiltinFun b))
nfAux _d (ValBinaryBuiltinFun b)     = pure (MkNF (ValBinaryBuiltinFun b))
nfAux _d (ValUnappliedConstructor n) = pure (MkNF (ValUnappliedConstructor n))
nfAux  d (ValConstructor n rs)       = do
  vs <- traverse (evalAndNF d) rs
  pure (MkNF (ValConstructor n vs))
nfAux _d (ValAssumed n)              = pure (MkNF (ValAssumed n))
nfAux _d (ValEnvironment env)        = pure (MkNF (ValEnvironment env))
nfAux d (ValBreached r')             = do
  r <- case r' of
    DeadlineMissed ev'party ev'act ev'timestamp party act deadline -> do
      ev'party' <- evalAndNF d ev'party
      act' <- evalAndNF d ev'act
      party' <- evalAndNF d party
      pure (DeadlineMissed ev'party' act' ev'timestamp party' act deadline)
  pure (MkNF (ValBreached r))
nfAux d (ValROp env op l r) = do
  l' <- traverseAndNF d l
  r' <- traverseAndNF d r
  pure (MkNF (ValROp env op l' r'))

traverseAndNF :: Int -> Either a WHNF -> Eval (Either a (Value NF))
traverseAndNF d = traverse (traverse (evalAndNF d))

evalAndNF :: Int -> Reference -> Eval NF
evalAndNF d r = do
  w <- runConfig (EvalRefMachine r)
  nfAux (d - 1) w

-- | Main entry point.
--
-- Given an initial environment (which is supposed to contain the environment for
-- imported entities), evaluate a module.
--
-- Returns the environment of the entities defined in *this* module, and
-- the results of the (L)EVAL directives in this module.
--
execEvalModuleWithEnv :: Environment -> Module Resolved -> IO (Environment, [EvalDirectiveResult])
execEvalModuleWithEnv env m@(MkModule _ moduleUri _) = do
  case evalModuleAndDirectives env m of
    MkEval f -> do
      stack     <- newIORef emptyStack
      supply    <- newIORef 0
      let evalTrace = Nothing
      r <- f MkEvalState {moduleUri, stack, supply, evalTrace}
      case r of
        Left _exc -> do
          -- exceptions at the top-level are unusual; after all, we don't actually
          -- force any evaluation here, and we catch exceptions for eval directives
          pure (emptyEnvironment, [])
        Right result -> do
          pure result

-- TODO: This currently allocates the initial environment once per module.
-- This isn't a big deal, but can we somehow do this only once per program,
-- for example by passing this in from the outside?
evalModuleAndDirectives :: Environment -> Module Resolved -> Eval (Environment, [EvalDirectiveResult])
evalModuleAndDirectives env m = do
  (env', directives) <- interpMachine do
    ienv <- initialEnvironment
    evalModule (env <> ienv) m
  results <- traverse nfDirective directives
  -- NOTE: We are only returning the new definitions of this module, not any imports.
  -- Depending on future export semantics, this may have to change.
  pure (env', results)


{- | Evaluate an expression in the context of a module and initial environment.

Didn't try to cache even more computation with rules,
because the current Rule type seems to
be Uri-focused, and so you'll emd up needing to pretty print and then re-parse.
Also, it's not clear how much caching can actually be done,
given that we won't be re-using the result from this.
 -}
execEvalExprInContextOfModule :: Expr Resolved -> (Environment, Module Resolved) -> IO (Maybe EvalDirectiveResult)
execEvalExprInContextOfModule expr (env, m) = do
  let
    evalExprDirective =
      Directive emptyAnno $ LazyEval emptyAnno expr
    -- Didn't make a new module that imported the context module,
    -- because making the import requires a Resolved.
    moduleWithoutDirectives = over moduleTopDecls (filter $ not . isDirective) m
  (_, res) <- execEvalModuleWithEnv env (evalExprDirective `prependToModule` moduleWithoutDirectives)
  case res of
    [result] -> pure (Just result)
    _        -> pure Nothing
  where
    isDirective :: TopDecl Resolved -> Bool
    isDirective (Directive _ _) = True
    isDirective _ = False

    prependToModule :: TopDecl Resolved -> Module Resolved -> Module Resolved
    prependToModule newDecl = over moduleTopDecls (newDecl :)
