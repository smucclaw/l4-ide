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
import qualified Base.DList as DList
import qualified Base.Map as Map
import qualified Base.Set as Set
import qualified Base.Text as Text
import L4.EvaluateLazy.Machine
import L4.EvaluateLazy.Trace
import L4.Evaluate.ValueLazy
import L4.Parser.SrcSpan (SrcRange)
import L4.Annotation
import L4.Print
import L4.Syntax

import Control.Concurrent

-----------------------------------------------------------------------------
-- The Eval monad and the required types for the monad
-----------------------------------------------------------------------------
data EvalState =
  MkEvalState
    { moduleUri :: !NormalizedUri
    , stack     :: !(IORef Stack)
    , supply    :: !(IORef Int)   -- used for uniques and addresses
    , evalTrace :: !(Maybe (IORef (DList EvalTraceAction)))
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

pushFrame :: (EvalException -> Eval ()) -> Frame -> Eval ()
pushFrame k frame = do
  s <- readRef (.stack)
  if s.size == maximumStackSize
    then k $ UserEvalException StackOverflow
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
exception :: (EvalException -> Eval a) -> EvalException -> Eval a
exception k exc =
  withPoppedFrame $ \ case
    Nothing -> throwError exc
    Just _f -> k exc

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
  Exception e -> do
    traceEval (ExitException e)
    exception (interpMachine . Exception) e
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
    pushFrame (interpMachine . Exception) f
  NewUnique -> newUnique

traceEval :: EvalTraceAction -> Eval ()
traceEval ta = do
  mtr <- asks (.evalTrace)
  case mtr of
    Nothing -> pure ()
    Just tr -> liftIO (modifyIORef' tr (`DList.snoc` ta))

-- | For the given eval action, enable tracing and accumulate a trace.
--
-- We try to make it so that in principle, nested calls to `captureTrace`
-- will yield the correct result.
--
captureTrace :: Eval a -> Eval (a, [EvalTraceAction])
captureTrace m = do
  mtr <- asks (.evalTrace) -- save old state
  ntr <- liftIO (newIORef mempty)
  r <- local (\ s -> s { evalTrace = Just ntr }) m
  tas <- liftIO (readIORef ntr)
  combine mtr tas -- combine our trace with old trace if it was active
  pure (r, toList tas)
  where
    combine :: Maybe (IORef (DList EvalTraceAction)) -> DList EvalTraceAction -> Eval ()
    combine Nothing   _   = pure ()
    combine (Just tr) tas = liftIO (modifyIORef' tr (<> tas))

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
