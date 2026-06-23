{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module L4.EvaluateLazy.Machine
( Frame
, module L4.EvaluateLazy.Exceptions
, Machine
, Eval
, EvalState (..)
, Stack (..)
, emptyStack
, runEval
, tryEval
, traceEval
, raiseException
, withPoppedFrame
, newUnique
, getTemporalContext
, putTemporalContext
, getEvalTime
, getModuleUri
, getSafeMode
, Config (..)
, forwardExpr
, matchBranches
, matchPattern
, backward
, EvalDirective (..)
, evalModule
, initialEnvironment
, evalRef
, emptyEnvironment
, boolView
, pattern ValBool
-- * Constants exposed for the eager evaluator
, builtinBinOps
, writeJSONToReferences
)
where

import Base
import qualified Base.DList as DList
import qualified Base.Text as Text
import qualified Base.Map as Map
import qualified Base.Set as Set
import Control.Concurrent
import System.Environment (lookupEnv)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
#ifdef HTTP_ENABLED
import           Network.HTTP.Req ((=:))
import qualified Network.HTTP.Req as Req
import qualified Data.ByteString.Lazy.Char8 as LBS
#endif
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import qualified Data.Text.Read as TR
import qualified Data.Char as Char
import Data.Fixed (Pico)
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Time.LocalTime (TimeOfDay(..), LocalTime(..), timeToTimeOfDay, timeOfDayToTime)
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.Zones as TZ
import qualified Data.Time.Zones.All as TZAll
import L4.Annotation
import L4.Evaluate.Operators
import L4.Evaluate.ValueLazy
import L4.TemporalContext (EvalClause (..), TemporalContext (..), applyEvalClauses)
import L4.Parser.SrcSpan (SrcRange)
import L4.Print hiding (tryLoadTZ, tryLoadTZPure, formatDateTimeIso)
import L4.Syntax
import qualified L4.TypeCheck as TypeCheck
import L4.TypeCheck.Types (EntityInfo)
import L4.EvaluateLazy.ContractFrame
import L4.EvaluateLazy.Exceptions
import L4.EvaluateLazy.Trace (EvalTraceAction (..))
import L4.TracePolicy (TracePolicy)
import qualified L4.TracePolicy as TracePolicy
import L4.Utils.Ratio
import Text.Read (readMaybe)
import qualified Data.Scientific as Sci
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException, catch)
import qualified Control.Exception

data Frame =
    BinOp1 BinOp {- -} (Expr Resolved) Environment
  | BinOp2 BinOp WHNF {- -}
  | Post1 {- -} (Expr Resolved) (Expr Resolved) Environment
  | Post2 WHNF {- -} (Expr Resolved) Environment
  | Post3 WHNF WHNF {- -}
  | App1 {- -} [Reference] (Maybe (Type' Resolved)) -- Added type for type-directed builtins
  | IfThenElse1 {- -} (Expr Resolved) (Expr Resolved) Environment
  | ConsiderWhen1 Reference {- -} (Expr Resolved) [Branch Resolved] Environment
  | PatNil0
  | PatCons0 (Pattern Resolved) Environment (Pattern Resolved)
  | PatCons1 {- -} Reference Environment (Pattern Resolved)
  | PatCons2 Environment {- -}
  | PatLit0 Environment (Expr Resolved) -- env, literal
  | PatLit1 WHNF -- the scrutinee
  | PatLit2
  | PatApp0 Resolved Environment [Pattern Resolved]
  | PatApp1 [Environment] {- -} [(Reference, Pattern Resolved)]
  | EqConstructor1 {- -} Reference [(Reference, Reference)]
  | EqConstructor2 WHNF {- -} [(Reference, Reference)]
  | EqConstructor3 {- -} [(Reference, Reference)]
  | UnaryBuiltin0 UnaryBuiltinFun (Maybe (Type' Resolved)) -- Added type for type-directed decoding
  | BinBuiltin1 BinOp Reference
  | BinBuiltin2 BinOp WHNF
  -- Ternary builtin frames: accumulate evaluated args
  | TernaryBuiltin1 TernaryBuiltinFun Reference Reference  -- evaluating 1st arg, has refs to 2nd and 3rd
  | TernaryBuiltin2 TernaryBuiltinFun WHNF Reference       -- has 1st arg value, evaluating 2nd, has ref to 3rd
  | TernaryBuiltin3 TernaryBuiltinFun WHNF WHNF            -- has 1st and 2nd arg values, evaluating 3rd
  -- Temporal context scoping for EVAL AS OF SYSTEM TIME
  | EvalAsOfSystemTime1 Reference Environment
  | EvalAsOfSystemTime2 TemporalContext
  -- Temporal context scoping for EVAL UNDER VALID TIME
  | EvalUnderValidTime1 Reference Environment
  | EvalUnderValidTime2 TemporalContext
  -- Temporal context scoping for rules effective time
  | EvalUnderRulesEffectiveAt1 Reference Environment
  | EvalUnderRulesEffectiveAt2 TemporalContext
  -- Temporal context scoping for rules encoded time
  | EvalUnderRulesEncodedAt1 Reference Environment
  | EvalUnderRulesEncodedAt2 TemporalContext
  -- Temporal iteration frames
  | EverBetweenFrame TemporalContext WHNF Time.Day Time.Day Integer
  | AlwaysBetweenFrame TemporalContext WHNF Time.Day Time.Day Integer
  | WhenLastFrame TemporalContext WHNF Time.Day
  | WhenNextFrame TemporalContext WHNF Time.Day Time.Day
  | ValueAtFrame TemporalContext
  | UpdateThunk Reference
  | ContractFrame ContractFrame
  | ConcatFrame [WHNF] {- -} [Expr Resolved] Environment -- accumulated values, remaining exprs, env
  | AsStringFrame -- AsString frame
  | ToStringDate1 Reference Reference -- evaluating DATE day, have month & year refs
  | ToStringDate2 Rational Reference  -- have day, evaluating month, have year ref
  | ToStringDate3 Rational Rational   -- have day & month, evaluating year
  | JsonEncodeListFrame [Text] {- -} Reference Bool -- accumulated JSON strings, tail reference, expecting_tail (True = next value is tail, False = next value is element)
  | JsonEncodeNestedFrame [Text] {- -} Reference -- accumulated JSON strings, tail reference (waiting for element encoding to complete)
  | JsonEncodeConstructorFrame [(Text, Text)] Text [(Text, Reference)] -- accumulated (fieldName, encodedJson) pairs, current field name, remaining (fieldName, fieldRef) pairs to encode
  deriving stock Show

-- ----------------------------------------------------------------------------
-- The evaluation monad.
--
-- This used to be a defunctionalized free monad ('Machine' as a GADT with a
-- 'Bind' constructor) interpreted into an @ExceptT EvalException (ReaderT
-- EvalState IO)@ stack. That double interpretation dominated evaluation
-- allocation (every machine step allocated GADT nodes, interpreter closures
-- and 'Either' results, and it kept GHC from inlining the primitives).
-- It is now a plain reader-over-IO monad: exceptions are thrown as
-- synchronous IO exceptions ('EvalException' has an 'Exception' instance)
-- and caught at directive boundaries via 'tryEval'.
-- ----------------------------------------------------------------------------

data EvalState =
  MkEvalState
    { moduleUri  :: !NormalizedUri
    , stack      :: !(IORef Stack)
    , supply     :: !(IORef Int)   -- used for uniques and addresses
    , evalTrace  :: !(Maybe (IORef (DList EvalTraceAction)))
    , entityInfo :: !EntityInfo    -- type information for constructors/records
    , evalTime   :: !UTCTime
    , temporalContext :: !(IORef TemporalContext)
    , tracePolicy :: !TracePolicy  -- controls trace collection and output
    , safeMode   :: !Bool          -- when True, HTTP operations return errors
    }

data Stack =
  MkStack
    { size   :: !Int
    , frames :: [Frame]
    }
  deriving stock (Generic, Show)

emptyStack :: Stack
emptyStack = MkStack 0 []

newtype Eval a = MkEval (EvalState -> IO a)
  deriving (Functor, Applicative, Monad, MonadReader EvalState, MonadIO)
    via ReaderT EvalState IO

-- | The historical name of the evaluation monad; the machine code below is
-- written against this alias.
type Machine = Eval

runEval :: EvalState -> Eval a -> IO a
runEval s (MkEval f) = f s

-- | Catch evaluation exceptions (used at directive boundaries).
tryEval :: Eval a -> Eval (Either EvalException a)
tryEval (MkEval f) = MkEval \s -> Control.Exception.try (f s)

nextSupply :: Eval Int
nextSupply = do
  supplyRef <- asks (.supply)
  liftIO do
    i <- readIORef supplyRef
    writeIORef supplyRef $! i + 1
    pure i

newUnique :: Eval Unique
newUnique = do
  i <- nextSupply
  u <- asks (.moduleUri)
  pure (MkUnique 'e' i u)

newAddress :: Eval Address
newAddress = do
  i <- nextSupply
  u <- asks (.moduleUri)
  pure (MkAddress u i)

traceEval :: EvalTraceAction -> Eval ()
traceEval ta = do
  mtr <- asks (.evalTrace)
  case mtr of
    Nothing -> pure ()
    Just tr -> liftIO (modifyIORef' tr (`DList.snoc` ta))

-- | Throw an evaluation exception: unwind the stack frame by frame (so an
-- active trace records the pops, mirroring the historical behaviour) and
-- then throw an IO exception.
raiseException :: EvalException -> Eval a
raiseException e = do
  traceEval (Exit (Left e))
  withPoppedFrame \ case
    Nothing -> liftIO (Control.Exception.throwIO e)
    Just _f -> raiseException e

internalException :: InternalEvalException -> Eval a
internalException = raiseException . InternalEvalException

userException :: UserEvalException -> Eval a
userException = raiseException . UserEvalException

stuckOnAssumed :: Resolved -> Eval a
stuckOnAssumed assumedResolved = userException (Stuck assumedResolved)

pushFrame :: Frame -> Eval ()
pushFrame frame = do
  traceEval Push
  stackRef <- asks (.stack)
  s <- liftIO (readIORef stackRef)
  if s.size >= maximumFrameDepth
    then userException StackOverflow
    else liftIO (writeIORef stackRef (MkStack (s.size + 1) (frame : s.frames)))

-- | Pops a stack frame (if any are left) and calls the continuation on it.
withPoppedFrame :: (Maybe Frame -> Eval a) -> Eval a
withPoppedFrame k = do
  traceEval Pop
  stackRef <- asks (.stack)
  s <- liftIO (readIORef stackRef)
  case s.frames of
    []       -> k Nothing
    (f : fs) -> do
      liftIO (writeIORef stackRef (MkStack (s.size - 1) fs))
      k (Just f)
{-# INLINE withPoppedFrame #-}

getEvalTime :: Eval UTCTime
getEvalTime = asks (.evalTime)

getTracePolicy :: Eval TracePolicy
getTracePolicy = asks (.tracePolicy)

getSafeMode :: Eval Bool
getSafeMode = asks (.safeMode)

getEntityInfo :: Eval EntityInfo
getEntityInfo = asks (.entityInfo)

getModuleUri :: Eval NormalizedUri
getModuleUri = asks (.moduleUri)

getTemporalContext :: Eval TemporalContext
getTemporalContext = do
  r <- asks (.temporalContext)
  liftIO (readIORef r)

putTemporalContext :: TemporalContext -> Eval ()
putTemporalContext ctx = do
  r <- asks (.temporalContext)
  liftIO (writeIORef r ctx)

-- | Atomically inspect-and-update a thunk. The update function additionally
-- receives the current thread id (for blackhole bookkeeping).
pokeThunk :: Reference -> (ThreadId -> Thunk -> (Thunk, a)) -> Eval a
pokeThunk rf k = liftIO do
  tid <- myThreadId
  atomicModifyIORef' rf.pointer (k tid)

readThunk :: Reference -> Eval Thunk
readThunk rf = liftIO (readIORef rf.pointer)

-- | allocateRecursive a recursive thunk: the environment may refer back to the
-- freshly created reference. The reference does not escape before it is
-- filled in, so a plain write suffices.
allocateRecursive :: Expr Resolved -> (Reference -> Environment) -> Eval (Reference, Environment)
allocateRecursive expr env = do
  address <- newAddress
  pointer <- liftIO (newIORef (WHNF ValNil)) -- placeholder, overwritten below before rf escapes
  let rf = MkReference address pointer
      env' = env rf
  liftIO (writeIORef pointer (Unevaluated Set.empty expr env'))
  traceEval (Alloc expr rf)
  pure (rf, env')

allocateValue :: WHNF -> Eval Reference
allocateValue whnf = do
  address <- newAddress
  pointer <- liftIO (newIORef (WHNF whnf))
  -- we don't trace this because it is used for allocating values in the
  -- initial environment which would be misleading in the trace
  pure (MkReference address pointer)

-- | allocateRecursive a blackhole that will be filled in later (used for mutually
-- recursive bindings). Forcing it before it is written is an error.
preAllocateRef :: Resolved -> Eval (Unique, Reference)
preAllocateRef r = do
  address <- newAddress
  rf <- liftIO do
    tid <- myThreadId
    pointer <- newIORef (Unevaluated (Set.singleton tid) (error "blackhole") Map.empty)
    pure (MkReference address pointer)
  traceEval (AllocPre r rf)
  pure (getUnique r, rf)

data Config
  = ForwardMachine Environment (Expr Resolved)
  | MatchBranchesMachine Reference Environment [Branch Resolved]
  | MatchPatternMachine Reference Environment (Pattern Resolved)
  | BackwardMachine WHNF
  | EvalRefMachine Reference
  | DoneMachine WHNF

-- Smart constructors for the next machine configuration. These used to be
-- pattern synonyms over the defunctionalized 'Machine' GADT.

continueExpr :: Environment -> Expr Resolved -> Machine Config
continueExpr env e = pure (ForwardMachine env e)
{-# INLINE continueExpr #-}

continueBranches :: Reference -> Environment -> [Branch Resolved] -> Machine Config
continueBranches r env e = pure (MatchBranchesMachine r env e)
{-# INLINE continueBranches #-}

continuePattern :: Reference -> Environment -> Pattern Resolved -> Machine Config
continuePattern r env pat = pure (MatchPatternMachine r env pat)
{-# INLINE continuePattern #-}

continueBackward :: WHNF -> Machine Config
continueBackward whnf = pure (BackwardMachine whnf)
{-# INLINE continueBackward #-}

continueRef :: Reference -> Machine Config
continueRef r = pure (EvalRefMachine r)
{-# INLINE continueRef #-}

continueDone :: WHNF -> Machine Config
continueDone whnf = pure (DoneMachine whnf)
{-# INLINE continueDone #-}


forwardExpr :: Environment -> Expr Resolved -> Machine Config
forwardExpr env = \ case
  RAnd _ann e1 e2 -> continueBackward (ValROp env ValRAnd (Left e1) (Left e2))
  ROr  _ann e1 e2 -> continueBackward (ValROp env ValROr (Left e1) (Left e2))
  And  _ann e1 e2 ->
    continueExpr env (IfThenElse emptyAnno e1 e2 falseExpr)
  Or   _ann e1 e2 ->
    continueExpr env (IfThenElse emptyAnno e1 trueExpr e2)
  Implies _ann e1 e2 ->
    continueExpr env (IfThenElse emptyAnno e1 e2 trueExpr)
  Not _ann e ->
    continueExpr env (IfThenElse emptyAnno e falseExpr trueExpr)
  Equals _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpEquals e2 env)
    continueExpr env e1
  Plus _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpPlus e2 env)
    continueExpr env e1
  Minus _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpMinus e2 env)
    continueExpr env e1
  Times _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpTimes e2 env)
    continueExpr env e1
  DividedBy _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpDividedBy e2 env)
    continueExpr env e1
  Modulo _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpModulo e2 env)
    continueExpr env e1
  Exponent _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpExponent e2 env)
    continueExpr env e1
  Leq _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpLeq e2 env)
    continueExpr env e1
  Geq _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpGeq e2 env)
    continueExpr env e1
  Lt _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpLt e2 env)
    continueExpr env e1
  Gt _ann e1 e2 -> do
    pushFrame (BinOp1 BinOpGt e2 env)
    continueExpr env e1
  Proj _ann e l ->
    continueExpr env (App emptyAnno l [e]) -- we desugar projection to plain function application
  Var _ann n -> -- still problematic: similarity / overlap between this and App with no args
    expectTerm env n >>= continueRef
  Cons _ann e1 e2 -> do
    rf1 <- allocate_ e1 env
    rf2 <- allocate_ e2 env
    continueBackward (ValCons rf1 rf2)
  Lam _ann givens e ->
    continueBackward (ValClosure givens e env)
  App _ann n [] ->
    expectTerm env n >>= continueRef
  App ann n es@(_ : _) -> do
    -- Handle temporal context override: EVAL AS OF SYSTEM TIME <serial> <thunk>
    -- The second argument is evaluated under the mutated temporal context.
    case getUnique n of
      uniq | uniq == TypeCheck.evalAsOfSystemTimeUnique
           , [dateExpr, thunkExpr] <- es -> do
               thunkRef <- allocate_ thunkExpr env
               pushFrame (EvalAsOfSystemTime1 thunkRef env)
               continueExpr env dateExpr
      uniq | uniq == TypeCheck.evalUnderValidTimeUnique
           , [dateExpr, thunkExpr] <- es -> do
               thunkRef <- allocate_ thunkExpr env
               pushFrame (EvalUnderValidTime1 thunkRef env)
               continueExpr env dateExpr
      uniq | uniq == TypeCheck.evalUnderRulesEffectiveAtUnique
           , [dateExpr, thunkExpr] <- es -> do
               thunkRef <- allocate_ thunkExpr env
               pushFrame (EvalUnderRulesEffectiveAt1 thunkRef env)
               continueExpr env dateExpr
      uniq | uniq == TypeCheck.evalUnderRulesEncodedAtUnique
           , [dateExpr, thunkExpr] <- es -> do
               thunkRef <- allocate_ thunkExpr env
               pushFrame (EvalUnderRulesEncodedAt1 thunkRef env)
               continueExpr env dateExpr
      _ -> do
        let expectedType = case getAnno ann of
              Anno {extra = Extension {resolvedInfo = Just (TypeInfo ty _)}} -> Just ty
              _ -> Nothing
        rs <- traverse (`allocate_` env) es
        pushFrame (App1 rs expectedType)
        continueExpr env (Var emptyAnno n)
  AppNamed ann n [] _ ->
    continueExpr env (App ann n [])
  AppNamed _ann _n _nes Nothing ->
    internalException $ RuntimeTypeError
      "named application where the order of arguments is not resolved"
  AppNamed ann n nes (Just order) ->
    let
     -- move expressions into order, drop names
      es = (\ (MkNamedExpr _ _ e) -> e) . snd <$> sortOn fst (zip order nes)
    in
      continueExpr env (App ann n es)
  IfThenElse _ann e1 e2 e3 -> do
    pushFrame (IfThenElse1 e2 e3 env)
    continueExpr env e1
  MultiWayIf _ann es e -> continueExpr env $ desugarMultiWayIf es e
    where
    desugarMultiWayIf :: [GuardedExpr Resolved] -> Expr Resolved -> Expr Resolved
    desugarMultiWayIf [] o = o
    desugarMultiWayIf (MkGuardedExpr _ann c f : es') o = IfThenElse emptyAnno c f $ desugarMultiWayIf es' o
  Consider _ann e branches -> do
    rf <- allocate_ e env
    continueBranches rf env branches
  Lit _ann lit -> do
    rval <- runLit lit
    continueBackward rval
  Percent _ann e -> do
    pushFrame (UnaryBuiltin0 UnaryPercent Nothing)
    continueExpr env e
  List _ann [] ->
    continueBackward ValNil
  List _ann (e : es) ->
    continueExpr env (Cons emptyAnno e (List emptyAnno es))
  Where _ann e ds -> do
    env' <- evalRecLocalDecls env ds
    let combinedEnv = Map.union env' env
    continueExpr combinedEnv e
  LetIn _ann ds e -> do
    env' <- evalRecLocalDecls env ds
    let combinedEnv = Map.union env' env
    continueExpr combinedEnv e
  Regulative _ann (MkDeonton _ party action due followup lest) ->
    continueBackward (ValObligation env (Left party) action (Left due) (fromMaybe fulfilExpr followup) lest)
  Event _ann ev ->
    continueExpr env (desugarEvent ev)
  Fetch _ann e -> do
    pushFrame (UnaryBuiltin0 UnaryFetch Nothing)
    continueExpr env e
  Env _ann e -> do
    pushFrame (UnaryBuiltin0 UnaryEnv Nothing)
    continueExpr env e
  Post _ann e1 e2 e3 -> do
    pushFrame (Post1 e2 e3 env)
    continueExpr env e1
  Concat _ann [] ->
    continueBackward (ValString "")
  Concat _ann (e : es) -> do
    pushFrame (ConcatFrame [] es env)
    continueExpr env e
  AsString _ann e -> do
    pushFrame AsStringFrame
    continueExpr env e
  Breach _ann mParty mReason -> do
    -- Explicit breach terminal clause - immediately produces a breach value
    mPartyRef <- traverse (\p -> allocate_ p env) mParty
    mReasonRef <- traverse (\r -> allocate_ r env) mReason
    continueBackward (ValBreached (ExplicitBreach mPartyRef mReasonRef))
  Inert _ann _txt ctx ->
    -- Inert elements are grammatical scaffolding with context-aware evaluation
    -- In AND context: True (identity), in OR context: False (identity)
    case ctx of
      InertCtxAnd  -> continueBackward (ValBool True)
      InertCtxOr   -> continueBackward (ValBool False)
      InertCtxNone -> continueBackward (ValBool True)  -- Default to True for compatibility

backward :: WHNF -> Machine Config
backward val = withPoppedFrame $ \ case
  Nothing -> continueDone val
  Just (BinOp1 binOp e2 env) -> do
    pushFrame (BinOp2 binOp val)
    continueExpr env e2
  Just (BinOp2 binOp val1) -> do
    runBinOp binOp val1 val
  Just (Post1 e2 e3 env) -> do
    pushFrame (Post2 val e3 env)
    continueExpr env e2
  Just (Post2 val1 e3 env) -> do
    pushFrame (Post3 val1 val)
    continueExpr env e3
  Just (Post3 val1 val2) -> do
    runPost val1 val2 val
  Just (BinBuiltin1 binOp r) -> do
    pushFrame (BinBuiltin2 binOp val)
    continueRef r
  Just (BinBuiltin2 binOp val1) ->
    runBinOp binOp val1 val
  Just f@(App1 rs mTy) -> do
    case val of
      ValClosure givens e env' -> do
        env'' <- matchGivens givens f rs
        continueExpr (Map.union env'' env') e
      ValUnappliedConstructor r ->
        continueBackward (ValConstructor r rs)
      ValObligation env party act due followup lest -> do
        (time, events) <- case rs of
          [t, r] -> pure (t, r)
          rs' -> internalException $ RuntimeTypeError $
            "expected a time stamp, and a list of events but found: " <> foldMap prettyLayout rs'
        pushFrame (ContractFrame (Contract1 ScrutinizeEvents {..}))
        continueRef events
      ValROp env op rexpr1 rexpr2 -> do
        -- make sure to reassemble the operation after returning
        pushFrame $ ContractFrame $ RBinOp1 MkRBinOp1 {args = rs, ..}
        -- apply the arguments of the left hand expression to the
        -- expression
        pushFrame f
        maybeEvaluate env rexpr1 -- TODO: build application
      ValUnaryBuiltinFun fn -> do
        r <- expect1 rs
        pushFrame (UnaryBuiltin0 fn mTy)
        continueRef r
      ValBinaryBuiltinFun fn -> do
        (x, y) <- expect2 rs
        case fn of
          -- 'BinOpCons' doesn't need to evaluate anything!
          BinOpCons -> do
            continueBackward $ ValCons x y
          _ -> do
            pushFrame (BinBuiltin1 fn y)
            continueRef x
      ValTernaryBuiltinFun fn -> do
        (x, y, z) <- expect3 rs
        -- Push frame for when we have all 3 args, then evaluate args right-to-left
        pushFrame (TernaryBuiltin1 fn y z)
        continueRef x
      ValPartialTernary fn arg1 -> do
        -- Already has 1 arg (as ref), need 2 more
        (y, z) <- expect2 rs
        -- We need to evaluate arg1, then y, then z
        pushFrame (TernaryBuiltin1 fn y z)
        continueRef arg1
      ValPartialTernary2 fn arg1 arg2 -> do
        -- Already has 2 args (as refs), need 1 more
        z <- expect1 rs
        -- We need to evaluate arg1, then arg2, then z
        pushFrame (TernaryBuiltin1 fn arg2 z)
        continueRef arg1
      ValFulfilled -> continueBackward ValFulfilled
      ValBreached r -> continueBackward (ValBreached r)
      ValAssumed r ->
        stuckOnAssumed r -- TODO: we can do better here
      res -> internalException (RuntimeTypeError $ "expected a function but found: " <> prettyLayout res)
  -- Evaluate thunk under overridden system time (serial number)
  Just (EvalAsOfSystemTime1 thunkRef _env) -> do
    serial <- expectNumber val
    originalCtx <- getTemporalContext
    let newCtx = applyEvalClauses [AsOfSystemTime (serialToUTCTime serial)] originalCtx
    putTemporalContext newCtx
    pushFrame (EvalAsOfSystemTime2 originalCtx)
    continueRef thunkRef
  Just (EvalUnderValidTime1 thunkRef _env) -> do
    serial <- expectNumber val
    originalCtx <- getTemporalContext
    let newCtx = applyEvalClauses [UnderValidTime (Time.utctDay (serialToUTCTime serial))] originalCtx
    putTemporalContext newCtx
    pushFrame (EvalUnderValidTime2 originalCtx)
    continueRef thunkRef
  Just (EvalUnderRulesEffectiveAt1 thunkRef _env) -> do
    serial <- expectNumber val
    originalCtx <- getTemporalContext
    let retroDay = Time.utctDay (serialToUTCTime serial)
        newCtx = applyEvalClauses [UnderRulesEffectiveAt retroDay] originalCtx
    putTemporalContext newCtx
    pushFrame (EvalUnderRulesEffectiveAt2 originalCtx)
    continueRef thunkRef
  Just (EvalUnderRulesEncodedAt1 thunkRef _env) -> do
    serial <- expectNumber val
    originalCtx <- getTemporalContext
    let newCtx = applyEvalClauses [UnderRulesEncodedAt (serialToUTCTime serial)] originalCtx
    putTemporalContext newCtx
    pushFrame (EvalUnderRulesEncodedAt2 originalCtx)
    continueRef thunkRef
  Just (IfThenElse1 e2 e3 env) ->
    case val of
      ValBool True -> continueExpr env e2
      ValBool False -> continueExpr env e3

      ValAssumed r -> stuckOnAssumed r

      _ -> internalException $ RuntimeTypeError $
        "expected a BOOLEAN but found: " <> prettyLayout val <> " when evaluating IF-THEN-ELSE"
  Just (ConsiderWhen1 _scrutinee e _branches env) -> do
    case val of
      ValEnvironment env' ->
        continueExpr (Map.union env' env) e
      _ ->
        internalException $ RuntimeTypeError $
          "expected an environment but found: " <> prettyLayout val <> " when evaluating WHEN"
  Just PatNil0 -> do
    case val of
      ValNil ->
        continueBackward (ValEnvironment Map.empty)
      _ ->
        patternMatchFailure
  Just (PatCons0 p1 env p2) -> do
    case val of
      ValCons rf1 rf2 -> do
        pushFrame (PatCons1 rf2 env p2)
        continuePattern rf1 env p1
      _ ->
        patternMatchFailure
  Just (PatCons1 rf2 env p2) -> do
    case val of
      ValEnvironment env1 -> do
        pushFrame (PatCons2 env1)
        continuePattern rf2 env p2
      _ ->
        internalException $ RuntimeTypeError $
          "expected an environment but found: " <> prettyLayout val <> " when matching FOLLOWED BY"
  Just (PatCons2 env1) ->
    case val of
      ValEnvironment env2 ->
        continueBackward (ValEnvironment (Map.union env2 env1))
      _ -> internalException $ RuntimeTypeError $
        "expected an environment but found: " <> prettyLayout val <> " when matching FOLLOWED BY"
  Just (PatApp0 n env ps) ->
    case val of
      ValConstructor n' rfs
        | sameResolved n n' ->
          if length rfs == length ps
            then
              let
                pairs = zip rfs ps
              in
                case pairs of
                  []             -> continueBackward (ValEnvironment Map.empty)
                  ((r, p) : rps) -> do
                    pushFrame (PatApp1 [] rps)
                    continuePattern r env p
            else internalException $ RuntimeTypeError
              "pattern for constructor has the wrong number of arguments"
      _ ->
        patternMatchFailure
  Just (PatApp1 envs rps) ->
    case val of
      ValEnvironment env ->
        case rps of
          []              -> continueBackward (ValEnvironment (Map.unions (env : envs)))
          ((r, p) : rps') -> do
            pushFrame (PatApp1 (env : envs) rps')
            continuePattern r env p
      _ -> internalException $ RuntimeTypeError $
        "expected an environment but found: " <> prettyLayout val <> " when matching constructor"
  Just (PatLit0 env lit) -> do
    pushFrame (PatLit1 val)
    continueExpr env lit
  Just (PatLit1 lit) -> do
    pushFrame PatLit2
    runBinOpEquals lit val
  Just PatLit2 ->
    case val of
      -- NOTE: in future, we may give the pattern that was matched a name, potentially
      ValBool True -> continueBackward $ ValEnvironment emptyEnvironment
      ValBool False -> patternMatchFailure
      _ -> internalException $ RuntimeTypeError $
        "expected a boolean but found: " <> prettyLayout val <> " while matching literal pattern"
  Just (EqConstructor1 rf rfs) -> do
    pushFrame (EqConstructor2 val rfs)
    continueRef rf
  Just (EqConstructor2 val1 rfs) -> do
    pushFrame (EqConstructor3 rfs)
    runBinOpEquals val1 val
  Just (EqConstructor3 rfs) ->
    case boolView val of
      Just False -> continueBackward $ valBool False
      Just True ->
        case rfs of
          [] -> continueBackward $ valBool True
          ((r1, r2) : rfs') -> do
            pushFrame (EqConstructor1 r2 rfs')
            continueRef r1
      Nothing -> internalException $ RuntimeTypeError $
        "expected a BOOLEAN but found: " <> prettyLayout val <> " when testing equality"
  Just (UnaryBuiltin0 fn mTy) -> do
    runBuiltin val fn mTy
  -- Ternary builtin handling: got 1st arg value, need to eval 2nd
  Just (TernaryBuiltin1 fn refArg2 refArg3) -> do
    pushFrame (TernaryBuiltin2 fn val refArg3)
    continueRef refArg2
  -- Ternary builtin handling: got 2nd arg value, need to eval 3rd
  Just (TernaryBuiltin2 fn val1 refArg3) -> do
    pushFrame (TernaryBuiltin3 fn val1 val)
    continueRef refArg3
  -- Ternary builtin handling: got all 3 args
  Just (TernaryBuiltin3 fn val1 val2) -> do
    runTernaryBuiltin fn val1 val2 val
  -- Temporal context scoping: restore original context after thunk evaluation
  Just (EvalAsOfSystemTime2 originalCtx) -> do
    putTemporalContext originalCtx
    continueBackward val
  Just (EvalUnderValidTime2 originalCtx) -> do
    putTemporalContext originalCtx
    continueBackward val
  Just (EvalUnderRulesEffectiveAt2 originalCtx) -> do
    putTemporalContext originalCtx
    continueBackward val
  Just (EvalUnderRulesEncodedAt2 originalCtx) -> do
    putTemporalContext originalCtx
    continueBackward val
  Just (EverBetweenFrame originalCtx predicate endDay currentDay step) -> do
    putTemporalContext originalCtx
    case boolView val of
      Just True -> continueBackward (valBool True)
      Just False ->
        if currentDay == endDay
          then continueBackward (valBool False)
          else do
            let nextDay = Time.addDays (fromIntegral step) currentDay
            let ctxForDay = applyEvalClauses [UnderValidTime nextDay, UnderRulesEffectiveAt nextDay] originalCtx
            putTemporalContext ctxForDay
            pushFrame (EverBetweenFrame originalCtx predicate endDay nextDay step)
            applyDatePredicate predicate nextDay
      Nothing ->
        userException $ UserError "EVER BETWEEN expects predicate returning BOOLEAN"
  Just (AlwaysBetweenFrame originalCtx predicate endDay currentDay step) -> do
    putTemporalContext originalCtx
    case boolView val of
      Just False -> continueBackward (valBool False)
      Just True ->
        if currentDay == endDay
          then continueBackward (valBool True)
          else do
            let nextDay = Time.addDays (fromIntegral step) currentDay
            let ctxForDay = applyEvalClauses [UnderValidTime nextDay, UnderRulesEffectiveAt nextDay] originalCtx
            putTemporalContext ctxForDay
            pushFrame (AlwaysBetweenFrame originalCtx predicate endDay nextDay step)
            applyDatePredicate predicate nextDay
      Nothing ->
        userException $ UserError "ALWAYS BETWEEN expects predicate returning BOOLEAN"
  Just (WhenLastFrame originalCtx predicate currentDay) -> do
    putTemporalContext originalCtx
    case boolView val of
      Just True -> do
        dateRef <- allocateValue (ValDate currentDay)
        continueBackward $ ValConstructor TypeCheck.justRef [dateRef]
      Just False -> do
        if dayNumberFromDay currentDay <= 0
          then continueBackward $ ValConstructor TypeCheck.nothingRef []
          else do
            let nextDay = Time.addDays (-1) currentDay
            let ctxForDay = applyEvalClauses [UnderValidTime nextDay, UnderRulesEffectiveAt nextDay] originalCtx
            putTemporalContext ctxForDay
            pushFrame (WhenLastFrame originalCtx predicate nextDay)
            applyDatePredicate predicate nextDay
      Nothing ->
        userException $ UserError "WHEN LAST expects predicate returning BOOLEAN"
  Just (WhenNextFrame originalCtx predicate currentDay limitDay) -> do
    putTemporalContext originalCtx
    case boolView val of
      Just True -> do
        dateRef <- allocateValue (ValDate currentDay)
        continueBackward $ ValConstructor TypeCheck.justRef [dateRef]
      Just False -> do
        if currentDay >= limitDay
          then continueBackward $ ValConstructor TypeCheck.nothingRef []
          else do
            let nextDay = Time.addDays 1 currentDay
            let ctxForDay = applyEvalClauses [UnderValidTime nextDay, UnderRulesEffectiveAt nextDay] originalCtx
            putTemporalContext ctxForDay
            pushFrame (WhenNextFrame originalCtx predicate nextDay limitDay)
            applyDatePredicate predicate nextDay
      Nothing ->
        userException $ UserError "WHEN NEXT expects predicate returning BOOLEAN"
  Just (ValueAtFrame originalCtx) -> do
    putTemporalContext originalCtx
    continueBackward val
  Just (ConcatFrame acc [] _env) -> do
    -- All arguments evaluated, concatenate them
    runConcat (reverse (val : acc))
  Just (ConcatFrame acc (e : es) env) -> do
    -- Evaluate next argument
    pushFrame (ConcatFrame (val : acc) es env)
    continueExpr env e
  Just AsStringFrame -> do
    -- Convert the value to string
    runAsString val
  Just (ToStringDate1 monthRef yearRef) -> do
    dayNum <- expectNumber val
    pushFrame (ToStringDate2 dayNum yearRef)
    continueRef monthRef
  Just (ToStringDate2 dayNum yearRef) -> do
    monthNum <- expectNumber val
    pushFrame (ToStringDate3 dayNum monthNum)
    continueRef yearRef
  Just (ToStringDate3 dayNum monthNum) -> do
    yearNum <- expectNumber val
    runDateToString dayNum monthNum yearNum
  Just (JsonEncodeListFrame acc tailRef expectingTail) -> do
    -- Handle the value we got back
    if expectingTail
      then
        -- We just evaluated the tail, so val is either ValNil or ValCons
        case val of
          ValNil -> do
            -- We're done! Combine all accumulated JSON strings into an array
            let jsonArray = "[" <> Text.intercalate "," (reverse acc) <> "]"
            continueBackward $ ValString jsonArray
          ValCons headRef nextTailRef -> do
            -- More elements to process. Evaluate the head element first
            pushFrame (JsonEncodeListFrame acc nextTailRef False)
            continueRef headRef
          _ ->
            -- Should not happen - tail should be ValNil or ValCons
            internalException $ RuntimeTypeError "Expected list (ValNil or ValCons) for tail"
      else
        -- We just evaluated an element, so encode it and continue with the tail
        case val of
          ValNil -> do
            -- Element is an empty list
            pushFrame (JsonEncodeListFrame ("[]" : acc) tailRef True)
            continueRef tailRef
          ValCons elemHeadRef elemTailRef -> do
            -- Element is a non-empty list, need to recursively encode it
            -- Push a frame to wait for the nested encoding, then start encoding the nested list
            pushFrame (JsonEncodeNestedFrame acc tailRef)  -- Will continue with tail after nested encoding
            pushFrame (JsonEncodeListFrame [] elemTailRef False)  -- Encode the nested list
            continueRef elemHeadRef
          _ -> do
            -- Element needs encoding (could be constructor, primitive, etc.)
            -- allocateRecursive it and use frame-based encoding to handle all cases properly
            elemRef <- allocateValue val
            pushFrame (JsonEncodeNestedFrame acc tailRef)  -- Will add result to acc and continue with tail
            pushFrame (UnaryBuiltin0 UnaryJsonEncode Nothing)  -- Encode using proper frame-based logic
            continueRef elemRef
  Just (JsonEncodeNestedFrame acc tailRef) -> do
    -- We just finished encoding an element (nested list, constructor, or primitive), val should be a ValString with the JSON
    case val of
      ValString encodedJson -> do
        -- Add the encoded JSON to accumulator and continue with the tail
        pushFrame (JsonEncodeListFrame (encodedJson : acc) tailRef True)
        continueRef tailRef
      _ ->
        -- Should not happen - encoding should return ValString
        internalException $ RuntimeTypeError "Expected ValString from element encoding"
  Just (JsonEncodeConstructorFrame acc currentFieldName remaining) -> do
    -- We just finished encoding a field value, val should be a ValString with the JSON
    case val of
      ValString encodedJson -> do
        -- Pair the current field name with the encoded value
        let newPair = (currentFieldName, encodedJson)
            newAcc = newPair : acc
        -- Check if there are more fields to encode
        case remaining of
          [] -> do
            -- All fields encoded! Build the final JSON object
            -- Note: acc is in reverse order, so reverse it
            let allPairs = reverse newAcc
                jsonFields = map (\(fname, fval) -> "\"" <> fname <> "\":" <> fval) allPairs
                jsonObject = "{" <> Text.intercalate "," jsonFields <> "}"
            continueBackward $ ValString jsonObject
          ((nextFieldName, nextFieldRef):rest) -> do
            -- More fields to encode
            pushFrame (JsonEncodeConstructorFrame newAcc nextFieldName rest)
            pushFrame (UnaryBuiltin0 UnaryJsonEncode Nothing)
            continueRef nextFieldRef
      _ ->
        -- Should not happen - field encoding should return ValString
        internalException $ RuntimeTypeError "Expected ValString from field encoding"
  Just (UpdateThunk rf) -> do
    updateThunkToWHNF rf val
    continueBackward val
  Just (ContractFrame cFrame) -> backwardContractFrame val cFrame

backwardContractFrame :: Value Reference -> ContractFrame -> Machine Config
backwardContractFrame val = \ case
  Contract1 ScrutinizeEvents {..} -> do
    case val of
      ValCons e es -> do
        pushCFrame (Contract2 ScrutinizeEvent {events = es, ..})
        continueRef e
      ValNil -> continueBackward (ValObligation env party act due followup lest)
      _ -> internalException $ RuntimeTypeError $
        "expected LIST EVENT but found: " <> prettyLayout val <> " when scrutinizing regulative events"
  Contract2 ScrutinizeEvent {..} -> case val of
    ValEvent ev'party ev'act ev'time -> do
      pushCFrame (Contract3 CurrentTimeWHNF {..})
      continueRef ev'time
    _ -> internalException $ RuntimeTypeError $
      "expected an EVENT but found: " <> prettyLayout val <> " when scrutinizing a regulative event"
  Contract3 CurrentTimeWHNF {..} -> do
    pushCFrame (Contract4 ScrutinizeDue {ev'time = val, ..})
    continueRef time
  Contract4 ScrutinizeDue {..} -> do
    case due of
       Right due' -> do
         pushCFrame (Contract5 CheckTiming {time = val, ..})
         continueBackward due'
       Left (Just due') -> do
         pushCFrame (Contract5 CheckTiming {time = val,..})
         continueExpr env due'
       Left Nothing -> do
         -- NOTE: we skip the timing step, hence we need to immediately update the current time to the event time
         -- because normally the timing check does that.
         pushCFrame (Contract6 PartyWHNF {time = ev'time, ..})
         maybeEvaluate env party
  Contract5 CheckTiming {..} -> do
    stamp <- assertTime ev'time
    due' <- assertTime val
    time' <- assertTime time
    let
      deadline = time' + due'
      -- NOTE: the new due is the current due minus the time that has passed
      -- by observing the current event e.g. if the thing
      -- was due within 3, then if the last current time
      -- is 2, and we are looking at an event at 3, then
      -- the current time is advances to 3 but the due is
      -- now earlier, it is  due within 2, i.e. 3 - (3 - 2)
      newDue = due' - (stamp - time')
    if stamp > deadline
      -- NOTE: the deadline has passed. What happens depends on the deontic modal:
      -- MUST/DO: deadline passed without action = BREACH (or LEST if specified)
      -- MUST NOT: deadline passed without prohibited action = FULFILLED (or HENCE if specified)
      -- MAY: deadline passed without exercising permission = FULFILLED (or HENCE if specified)
      then case act.modal of
        DMustNot ->
          -- Prohibition was RESPECTED: the prohibited action didn't occur before deadline
          -- Continue with HENCE (followup), which defaults to FULFILLED
          allocateValue ev'time >>= continueWithFollowup env followup events
        DMay ->
          -- Permission was NOT EXERCISED: that's fine, continue with HENCE/FULFILLED
          allocateValue ev'time >>= continueWithFollowup env followup events
        _ -> -- DMust, DDo: deadline passed = failure
          case lest of
            Nothing -> do
              -- NOTE: this is not too nice, but not wanting this would require to change `App1` to take MaybeEvaluated's
              partyR <- either (`allocate_` env) allocateValue party
              continueBackward (ValBreached (DeadlineMissed ev'party ev'act stamp partyR act deadline))
            Just lestFollowup -> allocateValue ev'time
              >>= continueWithFollowup env lestFollowup events
      else do
        -- NOTE: we have observed the event and do not branch, either, the
        -- only thing that may now happen is that we try a new event. Hence we
        -- drop the ev'time, set our time to ev'time and set our due to the new due
        pushCFrame (Contract6 PartyWHNF {time = ev'time, due = Right $ ValNumber newDue, ..})
        maybeEvaluate env party
  Contract6 PartyWHNF {..} -> do
    pushCFrame (Contract7 PartyEqual {party = val, ..})
    continueRef ev'party
  Contract7 PartyEqual {..} -> do
    pushCFrame (Contract8 ScrutinizeParty {ev'party = val, ..})
    runBinOpEquals party val
  Contract8 ScrutinizeParty {..} ->
    case val of
      ValBool True -> do
        pushCFrame (Contract11 (ActionDoesn'tmatch {..}))
        pushCFrame (Contract9 ScrutinizeEnvironment {..})
        continuePattern ev'act env act.action
      ValBool False -> do
        newTime <- allocateValue time
        tryNextEvent ScrutinizeEvents {party = Right party, time = newTime, ..} events
      _ -> internalException $ RuntimeTypeError $
        "expected BOOLEAN but found: " <> prettyLayout val
  Contract11 ActionDoesn'tmatch {} ->
    -- NOTE: this is a "guard frame" which only matters if we're unwinding the stack after a pattern match failure
    continueBackward val
  Contract9 ScrutinizeEnvironment {..} ->
    case val of
      ValEnvironment henceEnv -> do
        pushCFrame $ Contract10 ScrutinizeActions {..}
        continueExpr (env `Map.union` henceEnv) (fromMaybe trueExpr act.provided)
      _ -> internalException $ RuntimeTypeError $
        "expected environment but found: " <> prettyLayout val
  Contract10 ScrutinizeActions {..} ->
    case val of
      ValBool True ->
        -- Action matched! What happens depends on the deontic modal:
        -- MUST/MAY/DO: action done = success → continue with HENCE (followup)
        -- MUST NOT: action done = VIOLATION → continue with LEST (or BREACH if no LEST)
        case act.modal of
          DMustNot -> case lest of
            -- Prohibition violated: action was done, trigger LEST clause
            Just lestFollowup -> allocateValue time
              >>= continueWithFollowup (env `Map.union` henceEnv) lestFollowup events
            -- No LEST clause: immediate breach
            Nothing -> do
              -- Extract timestamp from time (which has been updated to event time)
              stamp <- assertTime time
              -- allocateRecursive references for the WHNF values
              ev'partyRef <- allocateValue ev'party
              partyRef <- allocateValue party
              -- For prohibition breach, we use the action time as "deadline"
              -- since the action should never have happened
              continueBackward (ValBreached (DeadlineMissed ev'partyRef ev'act stamp partyRef act stamp))
          -- MUST, MAY, DO: action done = success
          _ -> allocateValue time
            >>= continueWithFollowup (env `Map.union` henceEnv) followup events
      ValBool False -> do
        newTime <- allocateValue time
        tryNextEvent ScrutinizeEvents {party = Right party, time = newTime, ..} events
      _ -> internalException $ RuntimeTypeError $
        "expected BOOLEAN but found: " <> prettyLayout val
  RBinOp1 MkRBinOp1 {..}
    -- NOTE: this is weirdly asymmetric because
    -- in case of AND we can never abort earlier but have to instead
    -- wait for the left hand side expression to run to observe
    -- how we'll have to do the blame assignment
    | ValROr <- op
    , ValFulfilled <- val -> continueBackward ValFulfilled

  RBinOp1 MkRBinOp1 {..} -> do

    -- push a frame for when the evaluation of the
    -- second argument has completed
    pushCFrame $ RBinOp2 MkRBinOp2 {rval1 = val, ..}
    -- pass the arguments to the regulative expression
    pushFrame $ App1 args Nothing
    maybeEvaluate env rexpr2

  RBinOp2 MkRBinOp2 {..}
    -- if both obligations have been
    -- breached, then we can return breached
    | b1@(ValBreached (DeadlineMissed _ _ vt _ _ _)) <- rval1
    , b2@(ValBreached (DeadlineMissed _ _ vt' _ _ _)) <- val
    -> do
      -- NOTE: depending on the operation, we return
      -- the first breach, if the operation was and,
      -- because they're already the reason
      -- the second breach, if the operation was or,
      -- because they "missed their chance"
      -- If both happen at the same time, we return
      -- an arbitrary one (consistently with CSL)
      continueBackward
        if vt <= vt'
        then case op of
           ValRAnd -> b1
           ValROr -> b2
        else case op of
           ValROr -> b1
           ValRAnd -> b2

  RBinOp2 MkRBinOp2 {..}
    | ValFulfilled <- val
    , ValFulfilled <- rval1
    -> continueBackward ValFulfilled

  -- NOTE: note that blame assignment in the case of AND
  -- operators may be wrong if the events are passed out
  -- of order wrt time

  -- AND
  RBinOp2 MkRBinOp2 {..}
    | ValRAnd <- op
    , ValBreached reason <- rval1
    -- NOTE: we have a breach and we assume that all "previous"
    -- events have been seen, thus this is the "actual" breach
    -- more specifically, with this assumption, there's no
    -- possibility for future events to advance a possible
    -- remaining obligation while changing the blame assignment
    -> continueBackward (ValBreached reason)
  RBinOp2 MkRBinOp2 {..}
    | ValRAnd <- op
    , ValBreached reason <- val
    -> continueBackward (ValBreached reason)

  -- OR
  RBinOp2 MkRBinOp2 {..}
    | ValROr <- op
    , ValFulfilled <- val
    -> continueBackward ValFulfilled
  RBinOp2 MkRBinOp2 {..}
    | ValROr <- op
    , ValFulfilled <- rval1
    -> continueBackward ValFulfilled


  -- NOTE: otherwise, we do not have enough information to do
  -- any reduction of the contract clauses and thus have to return
  -- a value that represents the operator applied to each operand
  RBinOp2 MkRBinOp2 {..} ->
    continueBackward (ValROp env op (Right rval1) (Right val))
  where
    tryNextEvent :: ScrutinizeEvents -> Reference -> Machine Config
    tryNextEvent frame events = do
      pushCFrame (Contract1 frame)
      continueRef events

    pushCFrame = pushFrame . ContractFrame

    continueWithFollowup :: Environment -> RExpr -> Reference -> Reference -> Machine Config
    continueWithFollowup env followup events time = do
      pushFrame (App1 [time, events] Nothing)
      continueExpr env followup

    assertTime = \ case
      ValNumber i -> pure i
      v -> internalException $ RuntimeTypeError $
        "expected a NUMBER but got: " <> prettyLayout v

maybeEvaluate :: Environment -> MaybeEvaluated -> Machine Config
maybeEvaluate env = either (continueExpr env) continueBackward

matchGivens :: GivenSig Resolved -> Frame -> [Reference] -> Machine Environment
matchGivens (MkGivenSig _ann otns) f es = do
  let others = foldMap (either (const []) (\x -> [fst x]) . TypeCheck.isQuantifier) otns
  matchGivens' others f es

matchGivens' :: [Resolved] -> Frame -> [Reference] -> Machine Environment
matchGivens' ns f rs = do
  if length ns == length rs
    then do
      pure $ Map.fromList (zipWith (\ r v -> (getUnique r, v)) ns rs)
    else do
      pushFrame f -- provides better error context
      internalException $
        RuntimeTypeError "given signatures' values' lengths do not match"

matchBranches :: Reference -> Environment -> [Branch Resolved] -> Machine Config
matchBranches scrutinee _env [] =
  userException (NonExhaustivePatterns scrutinee)
matchBranches _scrutinee env (MkBranch _ann (Otherwise _ann') e : _) =
  continueExpr env e
matchBranches scrutinee env (MkBranch _ann (When _ann' pat) e : branches) = do
  pushFrame (ConsiderWhen1 scrutinee e branches env)
  continuePattern scrutinee env pat

matchPattern :: Reference -> Environment -> Pattern Resolved -> Machine Config
matchPattern scrutinee _env (PatVar _ann n) = do
  continueBackward (ValEnvironment (Map.singleton (getUnique n) scrutinee))
matchPattern scrutinee _env (PatApp _ann n [])
  | getUnique n == TypeCheck.emptyUnique = do -- pattern for the empty list
  pushFrame PatNil0
  continueRef scrutinee
matchPattern scrutinee env (PatCons _ann p1 p2) = do
  pushFrame (PatCons0 p1 env p2 )
  continueRef scrutinee
matchPattern scrutinee env (PatApp _ann n ps) = do
  pushFrame (PatApp0 n env ps)
  continueRef scrutinee
matchPattern scrutinee env (PatExpr _ann expr) = do
  pushFrame (PatLit0 env expr)
  continueRef scrutinee
matchPattern scrutinee _env (PatLit _ann lit) = do
  pushFrame $ PatLit1 case lit of
    NumericLit _ n -> ValNumber n
    StringLit _ s -> ValString  s
  continueRef scrutinee

-- | This unwinds the stack until it finds the enclosing pattern match and then resumes.
patternMatchFailure :: Machine Config
patternMatchFailure = withPoppedFrame $ \ case
  Nothing ->
    internalException UnhandledPatternMatch
  Just (ConsiderWhen1 scrutinee _ branches env) ->
    continueBranches scrutinee env branches
  -- we have unwound the frame that would reenter when scrutinizing the event
  Just (ContractFrame (Contract11 ActionDoesn'tmatch {..})) -> do
    newTime <- allocateValue time
    pushFrame $ ContractFrame $ Contract1 ScrutinizeEvents {party = Right party, time = newTime, ..}
    continueRef events
  Just _ ->
    patternMatchFailure

runLit :: Lit -> Machine WHNF
runLit (NumericLit _ann num) = pure (ValNumber num)
runLit (StringLit _ann str)  = pure (ValString str)

expect1 :: [a] -> Machine a
expect1 = \ case
  [x] -> pure x
  xs -> internalException (RuntimeTypeError $ "Expected 1 argument, but got " <> Text.textShow (length xs))

expect2 :: [a] -> Machine (a, a)
expect2 = \ case
  [x, y] -> pure (x, y)
  xs -> internalException (RuntimeTypeError $ "Expected 2 arguments, but got " <> Text.textShow (length xs))

expect3 :: [a] -> Machine (a, a, a)
expect3 = \ case
  [x, y, z] -> pure (x, y, z)
  xs -> internalException (RuntimeTypeError $ "Expected 3 arguments, but got " <> Text.textShow (length xs))

expectNumber :: WHNF -> Machine Rational
expectNumber = \ case
  ValNumber f -> pure f
  v -> internalException $ RuntimeTypeError $ "expected a NUMBER but got: " <> prettyLayout v

expectString :: WHNF -> Machine Text
expectString = \ case
  ValString f -> pure f
  v -> internalException $ RuntimeTypeError $ "expected a STRING but got: " <> prettyLayout v

expectDateValue :: WHNF -> Machine Time.Day
expectDateValue = \ case
  ValDate d -> pure d
  ValNumber serial -> pure (Time.utctDay (serialToUTCTime serial))
  v -> internalException $ RuntimeTypeError $ "expected a DATE but got: " <> prettyLayout v

expectInteger :: BinOp -> Rational -> Machine Integer
expectInteger op n = do
  case isInteger n of
    Nothing -> userException (NotAnInteger op n)
    Just i -> pure i

expectWhole :: Text -> Rational -> Machine Integer
expectWhole label n =
  case isInteger n of
    Nothing -> internalException $ RuntimeTypeError label
    Just i -> pure i

-- | Extract field names from a constructor's function type
-- The constructor type is typically: forall args. (field1: Type1, field2: Type2, ...) -> ResultType
extractFieldNames :: Type' Resolved -> Machine [Text]
extractFieldNames ty = case ty of
  -- Strip forall quantifier if present
  Forall _ _ innerTy -> extractFieldNames innerTy
  -- Extract field names from function arguments
  Fun _ argTypes _resultTy -> do
    pure $ mapMaybe getFieldName argTypes
  -- Not a function type - might be a nullary constructor
  _ -> pure []
  where
    getFieldName :: OptionallyNamedType Resolved -> Maybe Text
    getFieldName (MkOptionallyNamedType _ maybeName _ty) =
      nameToText . TypeCheck.getName <$> maybeName

-- | Extract both field names AND types from a constructor's function type
-- This is needed for type-directed JSON decoding of nested structures
extractFieldNamesAndTypes :: Type' Resolved -> Machine [(Text, Type' Resolved)]
extractFieldNamesAndTypes ty = case ty of
  -- Strip forall quantifier if present
  Forall _ _ innerTy -> extractFieldNamesAndTypes innerTy
  -- Extract field names and types from function arguments
  Fun _ argTypes _resultTy -> do
    pure $ mapMaybe getFieldNameAndType argTypes
  -- Not a function type - might be a nullary constructor
  _ -> pure []
  where
    getFieldNameAndType :: OptionallyNamedType Resolved -> Maybe (Text, Type' Resolved)
    getFieldNameAndType (MkOptionallyNamedType _ maybeName fieldType) =
      case maybeName of
        Just name -> Just (nameToText (TypeCheck.getName name), fieldType)
        Nothing -> Nothing

-- | Check if a constructor type is nullary (no arguments) and returns the given type
-- Used for enum (ONE OF) type detection
isNullaryConstructorReturning :: Type' Resolved -> Resolved -> Bool
isNullaryConstructorReturning conType targetTypeRef = case conType of
  -- Strip forall quantifier if present
  Forall _ _ innerTy -> isNullaryConstructorReturning innerTy targetTypeRef
  -- If it's a function type, it's not nullary
  Fun {} -> False
  -- If it's directly a type application, check if it matches our target
  TyApp _ tyRef [] -> getUnique tyRef == getUnique targetTypeRef
  -- Other cases: not a match
  _ -> False

-- | Encode an L4 value to JSON string
encodeValueToJson :: WHNF -> Machine Text
encodeValueToJson = \case
  ValString s -> pure $ "\"" <> escapeJson s <> "\""
  ValNumber n
    | denominator n == 1 -> pure $ Text.pack $ show (numerator n)
    | otherwise -> pure $ Text.pack $ show (fromRational n :: Double)
  ValBool True -> pure "true"
  ValBool False -> pure "false"
  ValNil -> pure "[]"
  ValCons _x _xs ->
    -- This should not be reached as lists are handled in runBuiltin with frames
    internalException $ RuntimeTypeError "Internal error: ValCons should be handled by frame-based evaluation in runBuiltin"
  ValConstructor conRef []
    | nameToText (TypeCheck.getName conRef) == "NOTHING" -> pure "null"
    | nameToText (TypeCheck.getName conRef) == "TRUE" -> pure "true"
    | nameToText (TypeCheck.getName conRef) == "FALSE" -> pure "false"
    | otherwise -> pure $ "\"" <> escapeJson (nameToText (TypeCheck.getName conRef)) <> "\""
  -- Note: For constructors with fields, we can't encode them directly within encodeValueToJson
  -- because we need to evaluate (force) each field reference. This requires frames.
  -- So constructors are handled in runBuiltin where we can push frames.
  ValConstructor conRef _fields -> do
    internalException $ RuntimeTypeError $
      "Internal error: Constructor encoding should be handled in runBuiltin, not encodeValueToJson: " <>
      nameToText (TypeCheck.getName conRef)
  val -> internalException $ RuntimeTypeError $ "Cannot encode value to JSON: " <> prettyLayout val
  where
    escapeJson :: Text -> Text
    escapeJson = Text.concatMap \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      c -> Text.singleton c

-- | Decode JSON string to L4 value wrapped in EITHER
-- Returns LEFT errorMsg on parse error, RIGHT value on success
-- | Type-directed JSON decoding - uses type information to construct proper records
decodeJsonToValueTyped :: Text -> Type' Resolved -> Machine WHNF
decodeJsonToValueTyped jsonStr ty = do
  let jsonBytes :: BS.ByteString
      jsonBytes = TE.encodeUtf8 jsonStr
  case Aeson.eitherDecodeStrict' jsonBytes of
    Left err -> do
      -- Parse error: return LEFT errorMsg
      errorRef <- allocateValue (ValString (Text.pack err))
      pure $ ValConstructor TypeCheck.leftRef [errorRef]
    Right jsonValue -> do
      -- Parse success: convert to L4 value using type information and wrap in RIGHT
      l4Value <- jsonValueToWHNFTyped jsonValue ty
      valueRef <- allocateValue l4Value
      pure $ ValConstructor TypeCheck.rightRef [valueRef]

-- | Convert Aeson Value to L4 WHNF using type information
-- This function recursively handles nested structures: lists of records, records containing records, etc.
jsonValueToWHNFTyped :: Aeson.Value -> Type' Resolved -> Machine WHNF
jsonValueToWHNFTyped jsonValue ty = do
  case ty of
    -- Handle LIST OF α
    TyApp _anno listRef [elementType]
      | nameToText (TypeCheck.getName listRef) == "LIST" -> do
        case jsonValue of
          Aeson.Array vec -> do
            -- Recursively decode each element with the element type
            let values = Vector.toList vec
            jsonListToWHNFTyped values elementType
          _ -> do
            userException $ UserError $
              "Expected JSON array to decode to LIST type, but got: " <> Text.pack (show jsonValue)

    -- Handle MAYBE α
    TyApp _anno maybeRef [innerType]
      | nameToText (TypeCheck.getName maybeRef) == "MAYBE" -> do
        case jsonValue of
          Aeson.Null ->
            -- JSON null maps to NOTHING
            pure $ ValConstructor TypeCheck.nothingRef []
          _ -> do
            -- Non-null value: decode and wrap in JUST
            innerVal <- jsonValueToWHNFTyped jsonValue innerType
            innerRef <- allocateValue innerVal
            pure $ ValConstructor TypeCheck.justRef [innerRef]

    -- Handle custom record types and primitives: TyApp conRef []
    TyApp _anno tyRef [] -> do
      let typeName = nameToText (TypeCheck.getName tyRef)

      -- Handle primitive types first
      case typeName of
        "STRING" -> do
          case jsonValue of
            Aeson.String s -> pure $ ValString s
            _ -> userException $ UserError $
                  "Expected JSON string but got: " <> Text.pack (show jsonValue)
        "NUMBER" -> do
          case jsonValue of
            Aeson.Number n -> pure $ ValNumber (toRational n)
            _ -> userException $ UserError $
                  "Expected JSON number but got: " <> Text.pack (show jsonValue)
        "BOOLEAN" -> do
          case jsonValue of
            Aeson.Bool b -> pure $ if b then ValBool True else ValBool False
            _ -> userException $ UserError $
                  "Expected JSON boolean but got: " <> Text.pack (show jsonValue)
        "DATE" -> do
          -- DATE fields in JSON should be ISO-8601 strings (YYYY-MM-DD)
          case jsonValue of
            Aeson.String s -> do
              case parseDateText s of
                Just day -> pure $ ValDate day
                Nothing -> userException $ UserError $
                  "Could not parse date string '" <> s <> "'. Expected format: YYYY-MM-DD"
            _ -> userException $ UserError $
                  "Expected JSON string for DATE field but got: " <> Text.pack (show jsonValue)
        "TIME" -> do
          -- TIME fields in JSON should be strings (HH:MM:SS or HH:MM)
          case jsonValue of
            Aeson.String s -> do
              case parseTimeText s of
                Just tod -> pure $ ValTime tod
                Nothing -> userException $ UserError $
                  "Could not parse time string '" <> s <> "'. Expected format: HH:MM:SS or HH:MM"
            _ -> userException $ UserError $
                  "Expected JSON string for TIME field but got: " <> Text.pack (show jsonValue)
        "DATETIME" -> do
          -- DATETIME fields in JSON should be ISO-8601 strings with timezone
          case jsonValue of
            Aeson.String s -> do
              case parseDatetimeText s of
                Just utc -> do
                  tc <- getTemporalContext
                  let tzName = fromMaybe "Etc/UTC" tc.tcDocumentTimezone
                  pure $ ValDateTime utc tzName
                Nothing -> userException $ UserError $
                  "Could not parse datetime string '" <> s <> "'. Expected ISO-8601 format: YYYY-MM-DDTHH:MM:SSZ"
            _ -> userException $ UserError $
                  "Expected JSON string for DATETIME field but got: " <> Text.pack (show jsonValue)

        -- Not a primitive, check if it's a custom record type
        _ -> do
          -- For record types, we need to find the constructor with the same name
          entityInfo <- getEntityInfo

          -- First check if this is a type
          case Map.lookup (getUnique tyRef) entityInfo of
            Nothing ->
              jsonValueToWHNF jsonValue
            Just (typeNameRef, _checkEntity) -> do
              -- Now look for a constructor with the same name
              let constructors = Map.toList entityInfo
                  matchingConstructor = listToMaybe
                    [ (unique, name, conType)
                    | (unique, (name, TypeCheck.KnownTerm conType Constructor)) <- constructors
                    , nameToText (TypeCheck.getName name) == nameToText (TypeCheck.getName typeNameRef)
                    ]

              case matchingConstructor of
                Nothing -> do
                  -- No record constructor found. Check if this is an enum type
                  -- by looking for nullary constructors that return this type.
                  case jsonValue of
                    Aeson.String enumName -> do
                      -- Look for a nullary constructor with this name that returns tyRef
                      let enumConstructor = listToMaybe
                            [ (unique, name)
                            | (unique, (name, TypeCheck.KnownTerm conType Constructor)) <- constructors
                            , nameToText (TypeCheck.getName name) == enumName
                            , isNullaryConstructorReturning conType tyRef
                            ]
                      case enumConstructor of
                        Just (enumUnique, enumConName) -> do
                          -- Found a matching enum constructor
                          let enumRef = Def enumUnique enumConName
                          pure $ ValConstructor enumRef []
                        Nothing ->
                          -- No matching enum constructor, fall back to generic decoding
                          jsonValueToWHNF jsonValue
                    _ ->
                      -- Not a string, fall back to generic decoding
                      jsonValueToWHNF jsonValue
                Just (conUnique, conName, conType) -> do
                  -- Construct a Resolved reference for the constructor
                  let conRef = Def conUnique conName
                  -- Extract BOTH field names AND types from the constructor type
                  fieldNamesAndTypes <- extractFieldNamesAndTypes conType
                  case jsonValue of
                    Aeson.Object obj -> do
                      -- Decode each field from the JSON object WITH TYPE INFORMATION
                      -- Note: We ignore extra fields in the JSON (Postel's Law)
                      fieldRefs <- forM fieldNamesAndTypes $ \(fieldName, fieldType) -> do
                        case KeyMap.lookup (Key.fromText fieldName) obj of
                          Nothing
                            | isMaybeFieldTy fieldType ->
                              -- MAYBE field missing in JSON: treat as NOTHING
                              allocateValue $ ValConstructor TypeCheck.nothingRef []
                            | otherwise -> do
                              -- Required field missing in JSON: error
                              userException $ UserError $
                                "Missing required field '" <> fieldName <> "' in JSON object"
                          Just fieldValue -> do
                            -- RECURSIVELY decode the field value WITH TYPE INFORMATION
                            fieldWHNF <- jsonValueToWHNFTyped fieldValue fieldType
                            allocateValue fieldWHNF
                      -- Construct the record with the decoded fields
                      pure $ ValConstructor conRef fieldRefs
                    _ -> do
                      -- JSON value is not an object, can't decode to record
                      userException $ UserError $
                        "Expected JSON object to decode to record type, but got: " <> Text.pack (show jsonValue)

    -- For other types, fall back to generic decoding
    _ -> jsonValueToWHNF jsonValue

-- | Check if a type is MAYBE α (used for optional record field handling)
isMaybeFieldTy :: Type' Resolved -> Bool
isMaybeFieldTy (TyApp _ tyName [_]) = nameToText (TypeCheck.getName tyName) == "MAYBE"
isMaybeFieldTy _ = False

-- | Convert list of JSON values to L4 list (ValCons/ValNil) with type information
-- This recursively decodes each element using the provided element type
jsonListToWHNFTyped :: [Aeson.Value] -> Type' Resolved -> Machine WHNF
jsonListToWHNFTyped [] _elementType = pure ValNil
jsonListToWHNFTyped (x:xs) elementType = do
  headVal <- jsonValueToWHNFTyped x elementType
  headRef <- allocateValue headVal
  tailVal <- jsonListToWHNFTyped xs elementType
  tailRef <- allocateValue tailVal
  pure $ ValCons headRef tailRef

decodeJsonToValue :: Text -> Machine WHNF
decodeJsonToValue jsonStr = do
  case Aeson.eitherDecodeStrict' (TE.encodeUtf8 jsonStr) of
    Left err -> do
      -- Parse error: return LEFT errorMsg
      errorRef <- allocateValue (ValString (Text.pack err))
      pure $ ValConstructor TypeCheck.leftRef [errorRef]
    Right jsonValue -> do
      -- Parse success: convert to L4 value and wrap in RIGHT
      l4Value <- jsonValueToWHNF jsonValue
      valueRef <- allocateValue l4Value
      pure $ ValConstructor TypeCheck.rightRef [valueRef]

-- | Convert Aeson Value to L4 WHNF
jsonValueToWHNF :: Aeson.Value -> Machine WHNF
jsonValueToWHNF = \case
  Aeson.Null -> pure $ ValConstructor TypeCheck.nothingRef []
  Aeson.Bool b -> pure $ if b then ValBool True else ValBool False
  Aeson.Number n -> pure $ ValNumber (toRational n)
  Aeson.String s -> pure $ ValString s
  Aeson.Array vec -> do
    -- Convert array to L4 list (cons cells)
    let values = Vector.toList vec
    jsonListToWHNF values
  Aeson.Object _obj -> do
    -- Convert object to L4 constructor with named fields
    -- Without type information, we can't properly construct typed records
    -- Return NOTHING to indicate we can't decode this
    pure $ ValConstructor TypeCheck.nothingRef []

-- | Convert list of JSON values to L4 list (ValCons/ValNil)
jsonListToWHNF :: [Aeson.Value] -> Machine WHNF
jsonListToWHNF [] = pure ValNil
jsonListToWHNF (x:xs) = do
  headVal <- jsonValueToWHNF x
  headRef <- allocateValue headVal
  tailVal <- jsonListToWHNF xs
  tailRef <- allocateValue tailVal
  pure $ ValCons headRef tailRef

-- | Convert list of Text values to L4 list (ValCons/ValNil)
textListToWHNF :: [Text] -> Machine WHNF
textListToWHNF [] = pure ValNil
textListToWHNF (x:xs) = do
  headRef <- allocateValue (ValString x)
  tailVal <- textListToWHNF xs
  tailRef <- allocateValue tailVal
  pure $ ValCons headRef tailRef

#ifdef HTTP_ENABLED
runPost :: WHNF -> WHNF -> WHNF -> Machine Config
runPost urlVal headersVal bodyVal = do
  safe <- getSafeMode
  if safe
    then internalException (RuntimeTypeError "POST is disabled in safe mode (no HTTP requests allowed)")
    else do
      url <- expectString urlVal
      headersStr <- expectString headersVal
      body <- expectString bodyVal
      let (url', options) = Text.breakOn "?" url
          (protocol, _) = Text.breakOn "://" url'
      case protocol of
        "https" -> do
          let (hostname, path) = Text.breakOn "/" (Text.drop (Text.length "https://") url')
              pathSegments = filter (not . Text.null) $ Text.splitOn "/" path
              reqBase = Req.https hostname
              reqWithPath = foldl (Req./:) reqBase pathSegments
              params = if Text.null options then [] else Text.splitOn "&" (Text.drop 1 options)

              -- Parse headers from newline-separated format: "Header-Name: value\nAnother-Header: value"
              headerLines = filter (not . Text.null) $ Text.splitOn "\n" headersStr
              parseHeader line =
                let (name, rest) = Text.breakOn ":" line
                    value = Text.strip $ Text.drop 1 rest  -- drop the colon and strip whitespace
                in if Text.null rest
                   then Nothing  -- invalid header format
                   else Just (Req.header (TE.encodeUtf8 name) (TE.encodeUtf8 value))
              headerOptions = mapMaybe parseHeader headerLines

              queryOptions = map (\p -> let (k,v) = Text.breakOn "=" p in k =: Text.drop 1 v) params
              req_options = mconcat (headerOptions <> queryOptions)

          res <- liftIO $ Req.runReq Req.defaultHttpConfig $ do
            Req.req Req.POST reqWithPath (Req.ReqBodyLbs $ LBS.fromStrict $ TE.encodeUtf8 body) Req.lbsResponse req_options
          continueBackward $ ValString (TE.decodeUtf8 . LBS.toStrict $ Req.responseBody res)
        _ -> internalException (RuntimeTypeError "POST only supports https")
#else
runPost :: WHNF -> WHNF -> WHNF -> Machine Config
runPost _ _ _ = internalException (RuntimeTypeError "POST is not available (HTTP support disabled at compile time)")
#endif

runConcat :: [WHNF] -> Machine Config
runConcat vals = do
  strings <- traverse expectString vals
  continueBackward $ ValString (Text.concat strings)

runAsString :: WHNF -> Machine Config
runAsString = coerceToString

coerceToString :: WHNF -> Machine Config
coerceToString val = case val of
  ValNumber n ->
    continueBackward $ ValString (prettyRatio n)
  ValString s ->
    continueBackward $ ValString s
  ValBool b ->
    continueBackward $ ValString (if b then "TRUE" else "FALSE")
  ValDate day ->
    continueBackward $ ValString (formatDateIso day)
  ValTime tod ->
    continueBackward $ ValString (formatTimeOfDay tod)
  ValDateTime utc tzName ->
    continueBackward $ ValString (formatDateTimeIso utc tzName)
  ValConstructor con fields
    | isDateConstructor con -> do
        case fields of
          [dayRef, monthRef, yearRef] -> do
            pushFrame (ToStringDate1 monthRef yearRef)
            continueRef dayRef
          _ ->
            internalException $ RuntimeTypeError "DATE values must have three fields (day, month, year) for string conversion"
    | otherwise ->
        incompatible
  _ ->
    incompatible
  where
    incompatible =
      userException $ UserError $
        "AS STRING/TOSTRING can only convert NUMBER, BOOLEAN, DATE, TIME, DATETIME, or STRING to STRING, but found: " <> prettyLayout val

formatDateIso :: Time.Day -> Text
formatDateIso day =
  let (year, month, dayOfMonth) = Time.toGregorian day
  in formatDateParts (fromIntegral year) (fromIntegral month) (fromIntegral dayOfMonth)

isDateConstructor :: Resolved -> Bool
isDateConstructor con =
  Text.toUpper (nameToText (TypeCheck.getName con)) == "DATE"

runDateToString :: Rational -> Rational -> Rational -> Machine Config
runDateToString dayNum monthNum yearNum = do
  dayInt <- expectIntegerNamed "day" dayNum
  monthInt <- expectIntegerNamed "month" monthNum
  yearInt <- expectIntegerNamed "year" yearNum
  continueBackward $ ValString (formatDateParts yearInt monthInt dayInt)

expectIntegerNamed :: Text -> Rational -> Machine Integer
expectIntegerNamed label n =
  case isInteger n of
    Just i -> pure i
    Nothing ->
      internalException $ RuntimeTypeError $
        "Expected an integer " <> label <> " but got: " <> prettyRatio n

formatDateParts :: Integer -> Integer -> Integer -> Text
formatDateParts year month day =
  pad 4 year <> "-" <> pad 2 month <> "-" <> pad 2 day
  where
    pad width v =
      let raw = Text.textShow v
      in if Text.length raw >= width
           then raw
           else Text.replicate (width - Text.length raw) "0" <> raw

runBuiltin :: WHNF -> UnaryBuiltinFun -> Maybe (Type' Resolved) -> Machine Config
runBuiltin es op mTy = do
  case op of
    UnaryJsonEncode -> do
      case es of
        ValCons headRef tailRef -> do
          -- Start frame-based evaluation for non-empty lists
          -- We're about to evaluate the head element (expectingTail = False)
          pushFrame (JsonEncodeListFrame [] tailRef False)
          continueRef headRef
        ValNil -> do
          -- Empty list is simple
          continueBackward $ ValString "[]"
        ValConstructor conRef []
          | nameToText (TypeCheck.getName conRef) == "NOTHING" ->
            -- NOTHING encodes to null
            continueBackward $ ValString "null"
          | nameToText (TypeCheck.getName conRef) == "TRUE" ->
            -- TRUE encodes to true
            continueBackward $ ValString "true"
          | nameToText (TypeCheck.getName conRef) == "FALSE" ->
            -- FALSE encodes to false
            continueBackward $ ValString "false"
        ValConstructor conRef [field]
          | nameToText (TypeCheck.getName conRef) == "JUST" -> do
            -- JUST wraps a single value, evaluate it and encode
            pushFrame (UnaryBuiltin0 UnaryJsonEncode Nothing)
            continueRef field
        ValConstructor conRef fields -> do
          -- Encode record constructors as JSON objects with field names
          entityInfo <- getEntityInfo
          case Map.lookup (getUnique conRef) entityInfo of
            Nothing ->
              internalException $ RuntimeTypeError $ "Cannot find constructor in entity info: " <> nameToText (TypeCheck.getName conRef)
            Just (_name, checkEntity) -> case checkEntity of
              TypeCheck.KnownTerm conType Constructor -> do
                -- Extract field names from the constructor's function type
                fieldNames <- extractFieldNames conType
                if length fieldNames /= length fields
                  then internalException $ RuntimeTypeError $
                    "Field count mismatch for constructor " <> nameToText (TypeCheck.getName conRef) <>
                    ": expected " <> Text.pack (show (length fieldNames)) <>
                    " but got " <> Text.pack (show (length fields))
                  else
                    let fieldPairs = zip fieldNames fields
                    in case fieldPairs of
                      [] -> do
                        -- Nullary constructor (no fields) - encode as JSON string with constructor name
                        -- This handles enum (ONE OF) values
                        let conName = nameToText (TypeCheck.getName conRef)
                        jsonStr <- encodeValueToJson (ValString conName)
                        continueBackward $ ValString jsonStr
                      ((fn, fr):rest) -> do
                        -- Start frame-based encoding of fields
                        -- The frame stores: accumulated pairs, current field name being encoded, remaining pairs
                        pushFrame (JsonEncodeConstructorFrame [] fn rest)
                        -- Push frame to encode the first field value
                        pushFrame (UnaryBuiltin0 UnaryJsonEncode Nothing)
                        -- Evaluate the first field
                        continueRef fr
              _ ->
                internalException $ RuntimeTypeError $
                  "Expected constructor term but got different entity type for: " <> nameToText (TypeCheck.getName conRef)
        _ -> do
          -- For non-list, non-constructor values, use direct encoding
          jsonStr <- encodeValueToJson es
          continueBackward $ ValString jsonStr
    UnaryJsonDecode -> do
      jsonStr <- expectString es
      result <- case mTy of
        Just ty -> do
          -- Extract inner type from EITHER if present
          -- JSONDECODE returns EITHER STRING α, so if we have EITHER STRING Person, extract Person
          let innerTy = case ty of
                TyApp _ eitherRef [_errorType, valueType]
                  | nameToText (TypeCheck.getName eitherRef) == "EITHER" -> valueType
                _ -> ty
          decodeJsonToValueTyped jsonStr innerTy
        Nothing ->
          decodeJsonToValue jsonStr
      continueBackward result
#ifdef HTTP_ENABLED
    UnaryFetch -> do
      safe <- getSafeMode
      if safe
        then internalException (RuntimeTypeError "FETCH is disabled in safe mode (no HTTP requests allowed)")
        else do
          url <- expectString es
          let (url', options) = Text.breakOn "?" url
              (protocol, _) = Text.breakOn "://" url'
          case protocol of
            "https" -> do
              let (hostname, path) = Text.breakOn "/" (Text.drop (Text.length "https://") url')
                  pathSegments = filter (not . Text.null) $ Text.splitOn "/" path
                  reqBase = Req.https hostname
                  reqWithPath = foldl (Req./:) reqBase pathSegments
                  params = if Text.null options then [] else Text.splitOn "&" (Text.drop 1 options)
                  req_options =
                    mconcat (map (\p -> let (k,v) = Text.breakOn "=" p in k =: Text.drop 1 v) params)
              res <- liftIO $ Req.runReq Req.defaultHttpConfig $ do
                Req.req Req.GET reqWithPath Req.NoReqBody Req.lbsResponse req_options
              continueBackward $ ValString (TE.decodeUtf8 . LBS.toStrict $ Req.responseBody res)
            _ -> internalException (RuntimeTypeError "FETCH only supports https")
#else
    UnaryFetch -> do
      internalException (RuntimeTypeError "FETCH is not available (HTTP support disabled at compile time)")
#endif
    UnaryEnv -> do
      varName <- expectString es
      maybeValue <- liftIO $ lookupEnv (Text.unpack varName)
      case maybeValue of
        Just value -> do
          -- Return JUST value
          valueRef <- allocateValue (ValString (Text.pack value))
          continueBackward (ValConstructor TypeCheck.justRef [valueRef])
        Nothing -> do
          -- Return NOTHING
          continueBackward (ValConstructor TypeCheck.nothingRef [])
    UnaryToString -> do
      coerceToString es
    UnaryToNumber -> do
      str <- expectString es
      case parseNumberText str of
        Just num -> do
          numRef <- allocateValue (ValNumber num)
          continueBackward $ ValConstructor TypeCheck.justRef [numRef]
        Nothing ->
          continueBackward $ ValConstructor TypeCheck.nothingRef []
    UnaryToDate -> do
      str <- expectString es
      case parseDateText str of
        Nothing ->
          continueBackward $ ValConstructor TypeCheck.nothingRef []
        Just parsedDay -> do
          maybeInner <- resolveMaybeInnerType mTy
          dateVal <- buildDateValue parsedDay maybeInner
          dateRef <- allocateValue dateVal
          continueBackward $ ValConstructor TypeCheck.justRef [dateRef]
    -- String unary operations
    UnaryStringLength -> do
      str <- expectString es
      continueBackward $ ValNumber (fromIntegral $ Text.length str)
    UnaryToUpper -> do
      str <- expectString es
      continueBackward $ ValString (Text.toUpper str)
    UnaryToLower -> do
      str <- expectString es
      continueBackward $ ValString (Text.toLower str)
    UnaryTrim -> do
      str <- expectString es
      continueBackward $ ValString (Text.strip str)
    UnaryDateValue -> do
      str <- expectString es
      case parseDateValueText str of
        Left err -> do
          errRef <- allocateValue (ValString err)
          continueBackward $ ValConstructor TypeCheck.leftRef [errRef]
        Right dayVal -> do
          valRef <- allocateValue (ValDate dayVal)
          continueBackward $ ValConstructor TypeCheck.rightRef [valRef]
    UnaryDateSerial -> do
      day <- expectDateValue es
      continueBackward $ ValNumber (fromIntegral (dayNumberFromDay day))
    UnaryDateFromSerial -> do
      serial <- expectNumber es
      continueBackward $ ValDate (Time.utctDay (serialToUTCTime serial))
    UnaryDateDay -> do
      day <- expectDateValue es
      let (_, _, d) = Time.toGregorian day
      continueBackward $ ValNumber (fromIntegral d)
    UnaryDateMonth -> do
      day <- expectDateValue es
      let (_, m, _) = Time.toGregorian day
      continueBackward $ ValNumber (fromIntegral m)
    UnaryDateYear -> do
      day <- expectDateValue es
      let (y, _, _) = Time.toGregorian day
      continueBackward $ ValNumber (fromIntegral y)
    UnaryTimeValue -> do
      str <- expectString es
      case parseTimeValueText str of
        Left err -> do
          errRef <- allocateValue (ValString err)
          continueBackward $ ValConstructor TypeCheck.leftRef [errRef]
        Right fraction -> do
          valRef <- allocateValue (ValNumber fraction)
          continueBackward $ ValConstructor TypeCheck.rightRef [valRef]
    -- TIME builtins
    UnaryTimeHour -> do
      tod <- expectTimeValue es
      continueBackward $ ValNumber (fromIntegral $ todHour tod)
    UnaryTimeMinute -> do
      tod <- expectTimeValue es
      continueBackward $ ValNumber (fromIntegral $ todMin tod)
    UnaryTimeSecond -> do
      tod <- expectTimeValue es
      continueBackward $ ValNumber (realToFrac $ todSec tod)
    UnaryTimeToSerial -> do
      tod <- expectTimeValue es
      let seconds = timeOfDayToTime tod
      continueBackward $ ValNumber (toRational seconds / toRational (86400 :: Pico))
    UnaryTimeFromSerial -> do
      serial <- expectNumber es
      let seconds = realToFrac (serial * 86400) :: Pico
      continueBackward $ ValTime (timeToTimeOfDay (realToFrac seconds))
    UnaryToTime -> do
      str <- expectString es
      case parseTimeText str of
        Just tod -> do
          todRef <- allocateValue (ValTime tod)
          continueBackward $ ValConstructor TypeCheck.justRef [todRef]
        Nothing ->
          continueBackward $ ValConstructor TypeCheck.nothingRef []
    -- DATETIME builtins
    UnaryDatetimeDate -> do
      (utc, tzName) <- expectDateTimeValue es
      case tryLoadTZPure tzName of
        Just tz -> do
          let localTime' = TZ.utcToLocalTimeTZ tz utc
          continueBackward $ ValDate (localDay localTime')
        Nothing ->
          userException $ UserError $ "Could not load timezone: " <> tzName
    UnaryDatetimeTime -> do
      (utc, tzName) <- expectDateTimeValue es
      case tryLoadTZPure tzName of
        Just tz -> do
          let localTime' = TZ.utcToLocalTimeTZ tz utc
          continueBackward $ ValTime (localTimeOfDay localTime')
        Nothing ->
          userException $ UserError $ "Could not load timezone: " <> tzName
    UnaryDatetimeSerial -> do
      (utc, _tzName) <- expectDateTimeValue es
      continueBackward $ ValNumber (utcDatestamp utc)
    UnaryDatetimeTzName -> do
      (_utc, tzName) <- expectDateTimeValue es
      continueBackward $ ValString tzName
    UnaryToDatetime -> do
      str <- expectString es
      case parseDatetimeText str of
        Just utc -> do
          -- Use document timezone as default for the stored tz name
          tc <- getTemporalContext
          let tzName = fromMaybe "Etc/UTC" tc.tcDocumentTimezone
          dtRef <- allocateValue (ValDateTime utc tzName)
          continueBackward $ ValConstructor TypeCheck.justRef [dtRef]
        Nothing ->
          continueBackward $ ValConstructor TypeCheck.nothingRef []
    -- Numeric unary operations (catch-all)
    _ -> do
      val :: Rational <- expectNumber es
      let valDouble :: Double
          valDouble = fromRational val
      case op of
        UnaryLn ->
          if val <= 0
            then userException $ UserError "LN expects input greater than 0"
            else continueBackward $ ValNumber (toRational (log valDouble))
        UnaryLog10 ->
          if val <= 0
            then userException $ UserError "LOG10 expects input greater than 0"
            else continueBackward $ ValNumber (toRational (logBase 10 valDouble))
        UnarySin ->
          continueBackward $ ValNumber (toRational (sin valDouble))
        UnaryCos ->
          continueBackward $ ValNumber (toRational (cos valDouble))
        UnaryTan ->
          continueBackward $ ValNumber (toRational (tan valDouble))
        UnaryAsin ->
          if val < (-1) || val > 1
            then userException $ UserError "ASIN expects input between -1 and 1"
            else continueBackward $ ValNumber (toRational (asin valDouble))
        UnaryAcos ->
          if val < (-1) || val > 1
            then userException $ UserError "ACOS expects input between -1 and 1"
            else continueBackward $ ValNumber (toRational (acos valDouble))
        UnaryAtan ->
          continueBackward $ ValNumber (toRational (atan valDouble))
        UnaryIsInteger -> continueBackward $ valBool $ isJust $ isInteger val
        UnaryRound -> continueBackward $ valInt $ round val
        UnaryCeiling -> continueBackward $ valInt $ ceiling val
        UnaryFloor -> continueBackward $ valInt $ floor val
        UnaryPercent -> continueBackward $ ValNumber (val / 100)
        UnarySqrt -> continueBackward $ ValNumber (toRational (sqrt valDouble))
  where
    valInt :: Integer -> WHNF
    valInt = ValNumber . toRational

runTernaryBuiltin :: TernaryBuiltinFun -> WHNF -> WHNF -> WHNF -> Machine Config
runTernaryBuiltin TernarySubstring val1 val2 val3 = do
  str <- expectString val1
  start <- expectNumber val2
  len <- expectNumber val3
  let startInt = floor start :: Int
      lenInt = floor len :: Int
  -- Use Text.take and Text.drop for substring
  continueBackward $ ValString (Text.take lenInt (Text.drop startInt str))
runTernaryBuiltin TernaryReplace val1 val2 val3 = do
  str <- expectString val1
  old <- expectString val2
  new <- expectString val3
  -- Use Text.replace: replace needle replacement haystack
  continueBackward $ ValString (Text.replace old new str)
runTernaryBuiltin TernaryPost val1 val2 val3 = runPost val1 val2 val3
runTernaryBuiltin TernaryDateFromDMY dVal mVal yVal = do
  dNum <- expectNumber dVal
  mNum <- expectNumber mVal
  yNum <- expectNumber yVal
  dInt <- expectWhole "DATE_FROM_DMY expects integer day" dNum
  mInt <- expectWhole "DATE_FROM_DMY expects integer month" mNum
  yInt <- expectWhole "DATE_FROM_DMY expects integer year" yNum
  case Time.fromGregorianValid yInt (fromInteger mInt) (fromInteger dInt) of
    Just day -> continueBackward (ValDate day)
    Nothing ->
      userException $ UserError $
        "DATE_FROM_DMY produced an invalid date from day="
        <> Text.pack (show dInt) <> ", month=" <> Text.pack (show mInt) <> ", year=" <> Text.pack (show yInt)
runTernaryBuiltin TernaryTimeFromHMS hVal mVal sVal = do
  hNum <- expectNumber hVal
  mNum <- expectNumber mVal
  sNum <- expectNumber sVal
  hInt <- expectWhole "TIME_FROM_HMS expects integer hour" hNum
  mInt <- expectWhole "TIME_FROM_HMS expects integer minute" mNum
  let sPico = realToFrac sNum :: Pico
  if hInt >= 0 && hInt < 24 && mInt >= 0 && mInt < 60 && sPico >= 0 && sPico < 60
    then continueBackward $ ValTime (TimeOfDay (fromInteger hInt) (fromInteger mInt) sPico)
    else userException $ UserError "TIME_FROM_HMS: values out of range (H: 0-23, M: 0-59, S: 0-59)"
runTernaryBuiltin TernaryDatetimeFromDTZ dateVal timeVal tzVal = do
  day <- expectDateValue dateVal
  tod <- expectTimeValue timeVal
  tzName <- expectString tzVal
  case tryLoadTZPure tzName of
    Just tz -> do
      let localTime = LocalTime day tod
          utc = TZ.localTimeToUTCTZ tz localTime
      continueBackward $ ValDateTime utc tzName
    Nothing ->
      userException $ UserError $ "Unknown timezone: '" <> tzName <> "'. Use an IANA timezone name like \"Asia/Singapore\" or \"America/New_York\"."
runTernaryBuiltin TernaryEverBetween startVal endVal predicate =
  startEverBetween startVal endVal predicate
runTernaryBuiltin TernaryAlwaysBetween startVal endVal predicate =
  startAlwaysBetween startVal endVal predicate

runBinOp :: BinOp -> WHNF -> WHNF -> Machine Config
runBinOp BinOpPlus   (ValNumber num1) (ValNumber num2)           = continueBackward $ ValNumber (num1 + num2)
runBinOp BinOpMinus  (ValNumber num1) (ValNumber num2)           = continueBackward $ ValNumber (num1 - num2)
runBinOp BinOpTimes  (ValNumber num1) (ValNumber num2)           = continueBackward $ ValNumber (num1 * num2)
runBinOp BinOpDividedBy (ValNumber num1) (ValNumber num2)        = do
  if num2 /= 0
    then continueBackward $ ValNumber (num1 / num2)
    else userException (DivisionByZero BinOpDividedBy)
runBinOp BinOpModulo    (ValNumber num1) (ValNumber num2)      = do
  n1 <- expectInteger BinOpModulo num1
  n2 <- expectInteger BinOpModulo num2
  if n2 /= 0
    then continueBackward $ ValNumber (toRational $ n1 `mod` n2)
    else userException (DivisionByZero BinOpModulo)
runBinOp BinOpExponent  (ValNumber base) (ValNumber exp_)   = continueBackward $ ValNumber (toRational ((fromRational base :: Double) ** (fromRational exp_ :: Double)))
runBinOp BinOpTrunc (ValNumber value) (ValNumber digits) =
  let digitsInt = round digits :: Integer
      scale k = (10 :: Rational) ^^ k
      truncated =
        if digitsInt >= 0
          then
            let factor = scale digitsInt
            in fromInteger (truncate (value * factor)) / factor
          else
            let factor = scale (abs digitsInt)
            in fromInteger (truncate (value / factor)) * factor
  in continueBackward $ ValNumber truncated
runBinOp BinOpEquals val1             val2                       = runBinOpEquals val1 val2
runBinOp BinOpLeq    (ValNumber num1) (ValNumber num2)           = continueBackward $ ValBool (num1 <= num2)
runBinOp BinOpLeq    (ValString str1) (ValString str2)           = continueBackward $ ValBool (str1 <= str2)
runBinOp BinOpLeq    (ValBool b1)     (ValBool b2)               = continueBackward $ ValBool (b1 <= b2)
runBinOp BinOpLeq    (ValDate d1)     (ValDate d2)               = continueBackward $ ValBool (d1 <= d2)
runBinOp BinOpLeq    (ValTime t1)     (ValTime t2)               = continueBackward $ ValBool (t1 <= t2)
runBinOp BinOpLeq    (ValDateTime u1 _) (ValDateTime u2 _)       = continueBackward $ ValBool (u1 <= u2)
runBinOp BinOpGeq    (ValNumber num1) (ValNumber num2)           = continueBackward $ ValBool (num1 >= num2)
runBinOp BinOpGeq    (ValString str1) (ValString str2)           = continueBackward $ ValBool (str1 >= str2)
runBinOp BinOpGeq    (ValBool b1)     (ValBool b2)               = continueBackward $ ValBool (b1 >= b2)
runBinOp BinOpGeq    (ValDate d1)     (ValDate d2)               = continueBackward $ ValBool (d1 >= d2)
runBinOp BinOpGeq    (ValTime t1)     (ValTime t2)               = continueBackward $ ValBool (t1 >= t2)
runBinOp BinOpGeq    (ValDateTime u1 _) (ValDateTime u2 _)       = continueBackward $ ValBool (u1 >= u2)
runBinOp BinOpLt     (ValNumber num1) (ValNumber num2)           = continueBackward $ ValBool (num1 < num2)
runBinOp BinOpLt     (ValString str1) (ValString str2)           = continueBackward $ ValBool (str1 < str2)
runBinOp BinOpLt     (ValBool b1)     (ValBool b2)               = continueBackward $ ValBool (b1 < b2)
runBinOp BinOpLt     (ValDate d1)     (ValDate d2)               = continueBackward $ ValBool (d1 < d2)
runBinOp BinOpLt     (ValTime t1)     (ValTime t2)               = continueBackward $ ValBool (t1 < t2)
runBinOp BinOpLt     (ValDateTime u1 _) (ValDateTime u2 _)       = continueBackward $ ValBool (u1 < u2)
runBinOp BinOpGt     (ValNumber num1) (ValNumber num2)           = continueBackward $ ValBool (num1 > num2)
runBinOp BinOpGt     (ValString str1) (ValString str2)           = continueBackward $ ValBool (str1 > str2)
runBinOp BinOpGt     (ValBool b1)     (ValBool b2)               = continueBackward $ ValBool (b1 > b2)
runBinOp BinOpGt     (ValDate d1)     (ValDate d2)               = continueBackward $ ValBool (d1 > d2)
runBinOp BinOpGt     (ValTime t1)     (ValTime t2)               = continueBackward $ ValBool (t1 > t2)
runBinOp BinOpGt     (ValDateTime u1 _) (ValDateTime u2 _)       = continueBackward $ ValBool (u1 > u2)
-- String binary operations
runBinOp BinOpContains   (ValString haystack) (ValString needle) = continueBackward $ ValBool (needle `Text.isInfixOf` haystack)
runBinOp BinOpStartsWith (ValString text) (ValString prefix)     = continueBackward $ ValBool (prefix `Text.isPrefixOf` text)
runBinOp BinOpEndsWith   (ValString text) (ValString suffix)     = continueBackward $ ValBool (suffix `Text.isSuffixOf` text)
runBinOp BinOpIndexOf    (ValString haystack) (ValString needle)
  | Text.null needle = continueBackward $ ValNumber 0  -- empty string found at position 0
  | otherwise =
    let (before, match) = Text.breakOn needle haystack
    in if Text.null match
       then continueBackward $ ValNumber (-1)  -- not found
       else continueBackward $ ValNumber (fromIntegral $ Text.length before)
-- SPLIT: STRING → STRING → LIST OF STRING
runBinOp BinOpSplit      (ValString text) (ValString delim) = do
  -- Text.splitOn returns [Text], convert to ValCons/ValNil list with proper allocation
  let parts = Text.splitOn delim text
  listVal <- textListToWHNF parts
  continueBackward listVal
-- CHARAT: STRING → NUMBER → STRING
runBinOp BinOpCharAt     (ValString text) (ValNumber idx) =
  let i = floor idx :: Int
  in if i < 0 || i >= Text.length text
     then continueBackward $ ValString ""  -- Out of bounds returns empty string
     else continueBackward $ ValString (Text.singleton (Text.index text i))
runBinOp BinOpWhenLast startVal predicateVal = startWhenLast startVal predicateVal
runBinOp BinOpWhenNext startVal predicateVal = startWhenNext startVal predicateVal
runBinOp BinOpValueAt dateVal attrVal = startValueAt dateVal attrVal
runBinOp _op         (ValAssumed r) _e2                          = stuckOnAssumed r
runBinOp _op         _e1 (ValAssumed r)                          = stuckOnAssumed r
runBinOp _           _                _                          = internalException (RuntimeTypeError "running bin op with invalid operation / value combination")

runBinOpEquals :: WHNF -> WHNF -> Machine Config
runBinOpEquals (ValNumber num1)        (ValNumber num2) = continueBackward $ valBool $ num1 == num2
runBinOpEquals (ValString str1)        (ValString str2) = continueBackward $ valBool $ str1 == str2
runBinOpEquals (ValDate d1)            (ValDate d2) = continueBackward $ valBool $ d1 == d2
runBinOpEquals (ValTime t1)            (ValTime t2) = continueBackward $ valBool $ t1 == t2
runBinOpEquals (ValDateTime u1 _)      (ValDateTime u2 _) = continueBackward $ valBool $ u1 == u2
runBinOpEquals ValNil                  ValNil           = continueBackward $ valBool True
runBinOpEquals (ValCons r1 rs1)        (ValCons r2 rs2) = do
  pushFrame (EqConstructor1 r2 [(rs1, rs2)])
  continueRef r1
runBinOpEquals ValNil                  (ValCons _ _)   = continueBackward $ ValBool False
runBinOpEquals (ValCons _ _)           ValNil           = continueBackward $ ValBool False
runBinOpEquals (ValConstructor n1 rs1) (ValConstructor n2 rs2)
  | sameResolved n1 n2 && length rs1 == length rs2 =
    let
      pairs = zip rs1 rs2
    in
      case pairs of
        [] -> continueBackward $ ValBool True
        ((r1, r2) : rss) -> do
          pushFrame (EqConstructor1 r2 rss)
          continueRef r1
  | otherwise                                           = continueBackward $ ValBool False
-- TODO: we probably also want to check ValObligations for equality
runBinOpEquals (ValAssumed r)          _                = stuckOnAssumed r
runBinOpEquals v1                       v2              = userException (EqualityOnUnsupportedType v1 v2)

infinityDay :: Time.Day
infinityDay = Time.fromGregorian 9999 12 31

applyDatePredicate :: WHNF -> Time.Day -> Machine Config
applyDatePredicate predicate day = do
  argRef <- allocateValue (ValDate day)
  pushFrame (App1 [argRef] Nothing)
  continueBackward predicate

startEverBetween :: WHNF -> WHNF -> WHNF -> Machine Config
startEverBetween startVal endVal predicate = do
  startDay <- expectDateValue startVal
  endDay <- expectDateValue endVal
  case compare startDay endDay of
    GT -> continueBackward (valBool False)
    _ -> do
      originalCtx <- getTemporalContext
      let step = if startDay <= endDay then 1 else -1
          ctxForDay = applyEvalClauses [UnderValidTime startDay, UnderRulesEffectiveAt startDay] originalCtx
      putTemporalContext ctxForDay
      pushFrame (EverBetweenFrame originalCtx predicate endDay startDay step)
      applyDatePredicate predicate startDay

startAlwaysBetween :: WHNF -> WHNF -> WHNF -> Machine Config
startAlwaysBetween startVal endVal predicate = do
  startDay <- expectDateValue startVal
  endDay <- expectDateValue endVal
  case compare startDay endDay of
    GT -> continueBackward (valBool True)
    _ -> do
      originalCtx <- getTemporalContext
      let step = if startDay <= endDay then 1 else -1
          ctxForDay = applyEvalClauses [UnderValidTime startDay, UnderRulesEffectiveAt startDay] originalCtx
      putTemporalContext ctxForDay
      pushFrame (AlwaysBetweenFrame originalCtx predicate endDay startDay step)
      applyDatePredicate predicate startDay

startWhenLast :: WHNF -> WHNF -> Machine Config
startWhenLast startVal predicate = do
  startDay <- expectDateValue startVal
  originalCtx <- getTemporalContext
  let ctxForDay = applyEvalClauses [UnderValidTime startDay, UnderRulesEffectiveAt startDay] originalCtx
  putTemporalContext ctxForDay
  pushFrame (WhenLastFrame originalCtx predicate startDay)
  applyDatePredicate predicate startDay

startWhenNext :: WHNF -> WHNF -> Machine Config
startWhenNext startVal predicate = do
  startDay <- expectDateValue startVal
  originalCtx <- getTemporalContext
  let ctxForDay = applyEvalClauses [UnderValidTime startDay, UnderRulesEffectiveAt startDay] originalCtx
  putTemporalContext ctxForDay
  pushFrame (WhenNextFrame originalCtx predicate startDay infinityDay)
  applyDatePredicate predicate startDay

startValueAt :: WHNF -> WHNF -> Machine Config
startValueAt dateVal attrVal = do
  day <- expectDateValue dateVal
  originalCtx <- getTemporalContext
  let ctxForDay = applyEvalClauses [UnderValidTime day, UnderRulesEffectiveAt day] originalCtx
  putTemporalContext ctxForDay
  pushFrame (ValueAtFrame originalCtx)
  applyDatePredicate attrVal day

pattern ValFulfilled :: Value a
pattern ValFulfilled <- (fulfilView -> True)
  where
    ValFulfilled = ValConstructor TypeCheck.fulfilRef []

pattern ValEvent :: a -> a -> a -> Value a
pattern ValEvent p a t <- (eventCView -> Just (p, a, t))
  where
    ValEvent p a t = ValConstructor TypeCheck.eventCRef [p, a, t]

eventCView :: Value a -> Maybe (a, a, a)
eventCView (ValConstructor n [p, a, t]) | n `sameResolved` TypeCheck.eventCRef = pure (p, a, t)
eventCView _ = Nothing

fulfilView :: Value a -> Bool
fulfilView v
  | ValConstructor r [] <- v = r `sameResolved` TypeCheck.fulfilRef
  | otherwise = False

pattern ValBool :: Bool -> Value a
pattern ValBool b <- (boolView -> Just b)
  where
    ValBool b = valBool b

valBool :: Bool -> Value a
valBool False = falseVal
valBool True  = trueVal

-- | Checks if a value is a Boolean constructor.
boolView :: Value a -> Maybe Bool
boolView val =
  case val of
    ValConstructor n []
      | sameResolved n TypeCheck.trueRef  -> Just True
      | sameResolved n TypeCheck.falseRef -> Just False
    _ -> Nothing

sameResolved :: Resolved -> Resolved -> Bool
sameResolved r1 r2 =
  getUnique r1 == getUnique r2

def :: Name -> Machine Resolved
def n = do
  u <- newUnique
  pure (Def u n)

ref :: Name -> Resolved -> Machine Resolved
ref n a =
  let
    (u, o) = getUniqueName a
  in
    pure (Ref n u o)


lookupTerm :: Environment -> Resolved -> Maybe Reference
lookupTerm env r =
  Map.lookup (getUnique r) env

expectTerm :: Environment -> Resolved -> Machine Reference
expectTerm env r =
  case lookupTerm env r of
    Nothing -> internalException (RuntimeScopeError r)
    Just rf -> pure rf

updateThunk :: Reference -> Thunk -> Machine ()
updateThunk rf !thunk = pokeThunk rf \_ _ -> (thunk, ())

updateThunkToWHNF :: Reference -> WHNF -> Machine ()
updateThunkToWHNF rf v =
  updateThunk rf (WHNF v)

-- NOTE: Once we start evaluating a thunk, we store the (Haskell) thread
-- that does so. If we encounter a thunk with such an entry created by
-- ourselves, we treat it as a blackhole: we tried to evaluate the thunk
-- again recursively, which means we're in a loop. If the evaluation was
-- triggered by a different thread though (in our case, this basically
-- means from a different module, then it's not necessarily a loop. We could
-- just wait (which is what GHC does), but we just try to evaluate it as
-- well, which should be benign.
evalRef :: Reference -> Machine Config
evalRef rf = do
  -- Fast path: thunk updates are monotonic (Unevaluated -> Unevaluated with
  -- more blackhole marks, or Unevaluated -> WHNF, never back), so a thunk
  -- observed in WHNF is final and can be returned from a plain read, without
  -- the atomic read-modify-write and the 'myThreadId' call. Only genuinely
  -- unevaluated thunks take the atomic path below (once each).
  thunk0 <- readThunk rf
  case thunk0 of
    WHNF val -> whnfConfig val
    Unevaluated{} ->
      join $ pokeThunk rf \tid -> \ case
        thunk@(WHNF val) ->
          -- Another thread finished it between our read and the atomic poke.
          (thunk, whnfConfig val)
        thunk@(Unevaluated tids e env)
          | tid `Set.member` tids ->  (thunk, userException (BlackholeForced e))
          | otherwise -> (Unevaluated (Set.insert tid tids) e env, pushFrame (UpdateThunk rf) *> continueExpr env e)
  where
    whnfConfig :: WHNF -> Machine Config
    whnfConfig val =
      case val of
        ValNullaryBuiltinFun fn -> do
          -- Don't cache nullary builtins (e.g. TIMEZONE, TODAY, NOW) because
          -- their results depend on mutable state (TemporalContext) that can
          -- change between evaluations while the thunk IORef persists in
          -- cached import environments.
          evaluated <- evalNullaryBuiltin fn
          continueBackward evaluated
        _ ->
          continueBackward val

-- | Recursive pre-allocation, used for mutually recursive let-bindings / declarations.
preAllocate :: [Resolved] -> Machine Environment
preAllocate ns = do
  pairs <- traverse preAllocateRef ns
  pure (Map.fromList pairs)

allocate_ :: Expr Resolved -> Environment -> Machine Reference
allocate_ (Var _ann n) env = do
  -- special case where we do not actually need to allocate
  expectTerm env n
allocate_ expr env =
  fst <$> allocateRecursive expr (const env)

-----------------------------------------------------------------------------
-- Prescanning and evaluation of modules
-----------------------------------------------------------------------------

evalModule :: Environment -> Module Resolved -> Machine (Environment, [EvalDirective])
evalModule env (MkModule _ann _uri section) = do
  names <- scanSection section
  env' <- preAllocate names
  let combinedEnv = Map.union env' env
  directives <- evalSection combinedEnv section
  pure (env', directives)

-- | Doesn't do any actual evaluation, just performs allocations
-- and returns the environment containing all the new bindings.
evalRecLocalDecls :: Environment -> [LocalDecl Resolved] -> Machine Environment
evalRecLocalDecls env decls = do
  names <- concat <$> traverse scanLocalDecl decls
  env' <- preAllocate names
  let combinedEnv = Map.union env' env
  traverse_ (evalLocalDecl combinedEnv) decls
  pure env'

-- | Just collect all defined names; could plausibly be done generically.
scanSection :: Section Resolved -> Machine [Resolved]
scanSection (MkSection _ann _mn _maka topdecls) =
  concat <$> traverse scanTopDecl topdecls

-- | Just collect all defined names; could plausibly be done generically.
scanTopDecl :: TopDecl Resolved -> Machine [Resolved]
scanTopDecl (Declare _ann declare) =
  scanDeclare declare
scanTopDecl (Decide _ann decide) =
  scanDecide decide
scanTopDecl (Assume _ann assume) =
  scanAssume assume
scanTopDecl (Section _ann section) =
  scanSection section
scanTopDecl (Directive _ann _directive) =
  pure []
scanTopDecl (Import _ann _import_) =
  pure []
scanTopDecl (Timezone _ann _expr) =
  pure []

-- | Just collect all defined names; could plausibly be done generically.
scanLocalDecl :: LocalDecl Resolved -> Machine [Resolved]
scanLocalDecl (LocalDecide _ann decide) =
  scanDecide decide
scanLocalDecl (LocalAssume _ann assume) =
  scanAssume assume

scanDecide :: Decide Resolved -> Machine [Resolved]
scanDecide (MkDecide _ann _tysig (MkAppForm _ n _ _) _expr) = pure [n]

scanAssume :: Assume Resolved -> Machine [Resolved]
scanAssume (MkAssume _ann _tysig (MkAppForm _ n _ _) _t) = pure [n]

-- | The only run-time names a type declaration brings into scope are constructors and selectors.
scanDeclare :: Declare Resolved -> Machine [Resolved]
scanDeclare (MkDeclare _ann _tysig _appFormAka t) = scanTypeDecl t

scanTypeDecl :: TypeDecl Resolved -> Machine [Resolved]
scanTypeDecl (EnumDecl _ann conDecls) =
  concat <$> traverse scanConDecl conDecls
scanTypeDecl (RecordDecl _ann mcon tns) =
  concat <$> traverse (\ c -> scanConDecl (MkConDecl emptyAnno c tns)) (toList mcon)
scanTypeDecl (SynonymDecl _ann _t) =
  pure []

scanConDecl :: ConDecl Resolved -> Machine [Resolved]
scanConDecl (MkConDecl _ann n [])  = pure [n]
scanConDecl (MkConDecl _ann n tns) = pure (n : ((\ (MkTypedName _ n' _ _) -> n') <$> tns))

evalSection :: Environment -> Section Resolved -> Machine [EvalDirective]
evalSection env (MkSection _ann _mn _maka topdecls) =
  concat <$> traverse (evalTopDecl env) topdecls

evalTopDecl :: Environment -> TopDecl Resolved -> Machine [EvalDirective]
evalTopDecl env (Declare _ann declare) =
  [] <$ evalDeclare env declare
evalTopDecl env (Decide _ann decide) =
  [] <$ evalDecide env decide
evalTopDecl env (Assume _ann assume) =
  [] <$ evalAssume env assume
evalTopDecl env (Directive _ann directive) =
  evalDirective env directive
evalTopDecl env (Section _ann section) =
  evalSection env section
evalTopDecl _env (Import _ann _import_) =
  pure []
evalTopDecl env (Timezone _ann expr) = do
  -- Extract timezone string from the expression and set it in TemporalContext.
  -- We store the timezone name without eagerly validating it here, because a
  -- userException at module level aborts ALL evaluation (including unrelated
  -- #EVAL directives).  Validation happens lazily when TODAY / CURRENTTIME is
  -- actually used, where the error is caught per-directive and surfaced as a
  -- proper diagnostic.
  mTzName <- extractTimezoneString env expr
  case mTzName of
    Just tzName -> do
      tc <- getTemporalContext
      putTemporalContext tc { tcDocumentTimezone = Just tzName }
    Nothing ->
      userException $ UserError
        "TIMEZONE IS must be a string literal or a simple identifier that resolves to a string."
  pure []

evalDirective :: Environment -> Directive Resolved -> Machine [EvalDirective]
evalDirective env (LazyEval ann expr) = do
  tracePolicy <- getTracePolicy
  let shouldTrace = case tracePolicy.evalDirectiveTrace of
        TracePolicy.NoTrace -> False
        TracePolicy.CollectTrace _ -> True
  pure [MkEvalDirective (rangeOf ann) shouldTrace False expr env]
evalDirective env (LazyEvalTrace ann expr) = do
  tracePolicy <- getTracePolicy
  let shouldTrace = case tracePolicy.evaltraceDirectiveTrace of
        TracePolicy.NoTrace -> False
        TracePolicy.CollectTrace _ -> True
  pure [MkEvalDirective (rangeOf ann) shouldTrace False expr env]
evalDirective _env (Check _ann _expr) =
  pure []
evalDirective env (Contract ann expr t evs) =
  evalDirective env . LazyEval ann =<< contractToEvalDirective expr t evs
evalDirective env (Assert ann expr) = do
  tracePolicy <- getTracePolicy
  let shouldTrace = case tracePolicy.evalDirectiveTrace of
        TracePolicy.NoTrace -> False
        TracePolicy.CollectTrace _ -> True
  pure [MkEvalDirective (rangeOf ann) shouldTrace True expr env]

contractToEvalDirective :: Expr Resolved -> Expr Resolved -> [Expr Resolved] -> Machine (Expr Resolved)
contractToEvalDirective contract t evs = do
  pure $ App emptyAnno TypeCheck.evalContractRef [contract, t, evListExpr]
  where
  evListExpr = List emptyAnno $ map eventExpr evs

eventExpr :: Expr Resolved -> Expr Resolved
eventExpr (Event _ann ev) = desugarEvent ev
eventExpr o = o -- NOTE: we must assume that the event is already desugared

desugarEvent :: Event Resolved -> Expr Resolved
desugarEvent (MkEvent ann party act timestamp _atFirst) = App ann TypeCheck.eventCRef [party, act, timestamp]

evalLocalDecl :: Environment -> LocalDecl Resolved -> Machine ()
evalLocalDecl env (LocalDecide _ann decide) =
  evalDecide env decide
evalLocalDecl env (LocalAssume _ann assume) =
  evalAssume env assume

-- We are assuming that the environment already contains an entry with an address for us.
evalAssume :: Environment -> Assume Resolved -> Machine ()
evalAssume env (MkAssume _ann _tysig (MkAppForm _ n []   _maka) _) =
  updateTerm env n (WHNF (ValAssumed n))
evalAssume env (MkAssume _ann _tysig (MkAppForm _ n _args _maka) _) = do
  -- TODO: we should create a given here yielding an assumed, but we currently cannot do that easily,
  -- because we do not have Assumed as an expression, and we also cannot embed values into expressions.
  updateTerm env n (WHNF (ValAssumed n))

updateTerm :: Environment -> Resolved -> Thunk -> Machine ()
updateTerm env n thunk = do
  rf <- expectTerm env n
  updateThunk rf thunk

-- We are assuming that the environment already contains an entry with an address for us.
evalDecide :: Environment -> Decide Resolved -> Machine ()
evalDecide env (MkDecide _ann _tysig (MkAppForm _ n []   _maka) expr) =
  updateTerm env n (Unevaluated Set.empty expr env)
evalDecide env (MkDecide _ann _tysig (MkAppForm _ n args _maka) expr) = do
  let
    v = ValClosure (MkGivenSig emptyAnno ((\ r -> MkOptionallyTypedName emptyAnno r Nothing) <$> args)) expr env
  updateTerm env n (WHNF v)

-- We are assuming that the environment already contains an entry with an address for us.
evalDeclare :: Environment -> Declare Resolved -> Machine ()
evalDeclare env (MkDeclare _ann _tysig _appFormAka t) =
  evalTypeDecl env t

evalTypeDecl :: Environment -> TypeDecl Resolved -> Machine ()
evalTypeDecl env (EnumDecl _ann conDecls) =
  traverse_ (evalConDecl env) conDecls
evalTypeDecl env (RecordDecl _ann mcon tns) =
  traverse_ (\ c -> evalConDecl env (MkConDecl emptyAnno c tns)) mcon
evalTypeDecl _env (SynonymDecl _ann _t) =
  pure ()

evalConDecl :: Environment -> ConDecl Resolved -> Machine ()
evalConDecl env (MkConDecl _ann n []) =
  updateTerm env n (WHNF (ValConstructor n []))
evalConDecl env (MkConDecl _ann n tns) = do
  -- constructor
  updateTerm env n (WHNF (ValUnappliedConstructor n))
  conRef <- ref (TypeCheck.getName n) n
  -- selectors (we need to create fresh names for the lambda abstractions so that every binder is unique)
  traverse_ (\ (i, MkTypedName _ sn _t _) -> do
    arg    <- def (TypeCheck.getName n)
    argRef <- ref (TypeCheck.getName n) arg
    args <- traverse (def . TypeCheck.getName) tns
    body <- ref (TypeCheck.getName sn) (args !! i)
    let
      sel =
        ValClosure
          (MkGivenSig emptyAnno [MkOptionallyTypedName emptyAnno arg Nothing])      -- \ x ->
          (Consider emptyAnno (App emptyAnno argRef [])                             -- case x of
            [ MkBranch emptyAnno (When emptyAnno (PatApp emptyAnno conRef (PatVar emptyAnno <$> args)))  --   Con y_1 ... y_n ->
                (App emptyAnno body [])                                             --     y_i
            ]
          )
          emptyEnvironment
    updateTerm env sn (WHNF sel)
    ) (zip [0 ..] tns)

-----------------------------------------------------------------------------
-- Premade expressions and values
-----------------------------------------------------------------------------

falseExpr :: Expr Resolved
falseExpr = App emptyAnno TypeCheck.falseRef []

falseVal :: Value a
falseVal = ValConstructor TypeCheck.falseRef []

trueExpr :: Expr Resolved
trueExpr = App emptyAnno TypeCheck.trueRef []

trueVal :: Value a
trueVal = ValConstructor TypeCheck.trueRef []

fulfilExpr :: Expr Resolved
fulfilExpr = App emptyAnno TypeCheck.fulfilRef []

-- \a b c. a b c
evalContractVal :: Machine (Value a)
evalContractVal = do
  let mn = MkName emptyAnno . NormalName
      (na, nb, nc) = (mn "a", mn "b", mn "c")
  ad <- def na
  bd <- def nb
  cd <- def nc
  ar <- ref na ad
  br <- ref nb bd
  cr <- ref nc cd

  pure $ ValClosure
    (MkGivenSig emptyAnno
      [ MkOptionallyTypedName emptyAnno ad Nothing
      , MkOptionallyTypedName emptyAnno bd Nothing
      , MkOptionallyTypedName emptyAnno cd Nothing
      ]
    )
    (App emptyAnno ar [App emptyAnno br [], App emptyAnno cr []])
    emptyEnvironment

waitUntilVal :: Reference -> Reference -> Reference -> Machine (Value a)
waitUntilVal eventcRef neverMatchesPartyRef neverMatchesActRef = do
  let an = MkName emptyAnno $ NormalName "a"
  ad <- def an
  ar <- ref an ad
  pure $ ValClosure
    (MkGivenSig emptyAnno [MkOptionallyTypedName emptyAnno ad Nothing])
    (App emptyAnno TypeCheck.eventCRef [neverMatchesPartyExpr, neverMatchesActExpr, Var emptyAnno ar])
    (Map.fromList
      [ (TypeCheck.eventCUnique, eventcRef)
      , (TypeCheck.neverMatchesPartyUnique, neverMatchesPartyRef)
      , (TypeCheck.neverMatchesActUnique, neverMatchesActRef)
      ]
    )

pattern ValNeverMatchesParty, ValNeverMatchesAct :: Value a
pattern ValNeverMatchesParty <- (\case ValConstructor r [] -> r `sameResolved` TypeCheck.neverMatchesPartyRef; _ -> False -> True)
  where ValNeverMatchesParty = ValConstructor TypeCheck.neverMatchesPartyRef []
pattern ValNeverMatchesAct <- (\case ValConstructor r [] -> r `sameResolved` TypeCheck.neverMatchesActRef; _ -> False -> True)
  where ValNeverMatchesAct = ValConstructor TypeCheck.neverMatchesActRef []

neverMatchesPartyExpr, neverMatchesActExpr :: Expr Resolved
neverMatchesPartyExpr = App emptyAnno TypeCheck.neverMatchesPartyRef []
neverMatchesActExpr = App emptyAnno TypeCheck.neverMatchesActRef []

-- EVENT :: party -> act -> Number -> EVENT party act
eventCVal :: Value a
eventCVal = ValUnappliedConstructor TypeCheck.eventCRef

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

data EvalDirective =
  MkEvalDirective
    { range    :: Maybe SrcRange -- ^ of the (L)EVAL directive
    , trace    :: !Bool -- ^ whether a trace is wanted
    , isAssert :: !Bool -- ^ whether it is to be treated as an assertion
    , expr     :: !(Expr Resolved) -- ^ expression to evaluate
    , env      :: !Environment -- ^ environment to evaluate the expression in
    }
  deriving stock (Generic, Show)

-----------------------------------------------------------------------------
-- Prettyprinting of the EvalExceptions
-----------------------------------------------------------------------------

-- The initial environment has to be built by pre-allocation.
initialEnvironment :: Machine Environment
initialEnvironment = do
  falseRef <- allocateValue falseVal
  trueRef  <- allocateValue trueVal
  nilRef   <- allocateValue ValNil
  nothingRef <- allocateValue (ValConstructor TypeCheck.nothingRef [])
  justRef <- allocateValue (ValUnappliedConstructor TypeCheck.justRef)
  leftRef <- allocateValue (ValUnappliedConstructor TypeCheck.leftRef)
  rightRef <- allocateValue (ValUnappliedConstructor TypeCheck.rightRef)
  evalContractRef <- allocateValue =<< evalContractVal
  eventCRef <- allocateValue eventCVal
  isIntegerRef <- allocateValue (ValUnaryBuiltinFun UnaryIsInteger)
  roundRef <- allocateValue (ValUnaryBuiltinFun UnaryRound)
  ceilingRef <- allocateValue (ValUnaryBuiltinFun UnaryCeiling)
  floorRef <- allocateValue (ValUnaryBuiltinFun UnaryFloor)
  sqrtRef <- allocateValue (ValUnaryBuiltinFun UnarySqrt)
  lnRef <- allocateValue (ValUnaryBuiltinFun UnaryLn)
  log10Ref <- allocateValue (ValUnaryBuiltinFun UnaryLog10)
  sinRef <- allocateValue (ValUnaryBuiltinFun UnarySin)
  cosRef <- allocateValue (ValUnaryBuiltinFun UnaryCos)
  tanRef <- allocateValue (ValUnaryBuiltinFun UnaryTan)
  asinRef <- allocateValue (ValUnaryBuiltinFun UnaryAsin)
  acosRef <- allocateValue (ValUnaryBuiltinFun UnaryAcos)
  atanRef <- allocateValue (ValUnaryBuiltinFun UnaryAtan)
  -- String unary builtins
  stringLengthRef <- allocateValue (ValUnaryBuiltinFun UnaryStringLength)
  toUpperRef <- allocateValue (ValUnaryBuiltinFun UnaryToUpper)
  toLowerRef <- allocateValue (ValUnaryBuiltinFun UnaryToLower)
  trimRef <- allocateValue (ValUnaryBuiltinFun UnaryTrim)
  toStringRef <- allocateValue (ValUnaryBuiltinFun UnaryToString)
  toNumberRef <- allocateValue (ValUnaryBuiltinFun UnaryToNumber)
  toDateRef <- allocateValue (ValUnaryBuiltinFun UnaryToDate)
  -- TIME builtins
  timeHourRef <- allocateValue (ValUnaryBuiltinFun UnaryTimeHour)
  timeMinuteRef <- allocateValue (ValUnaryBuiltinFun UnaryTimeMinute)
  timeSecondRef <- allocateValue (ValUnaryBuiltinFun UnaryTimeSecond)
  timeToSerialRef <- allocateValue (ValUnaryBuiltinFun UnaryTimeToSerial)
  timeFromSerialRef <- allocateValue (ValUnaryBuiltinFun UnaryTimeFromSerial)
  timeFromHMSRef <- allocateValue (ValTernaryBuiltinFun TernaryTimeFromHMS)
  toTimeRef <- allocateValue (ValUnaryBuiltinFun UnaryToTime)
  -- DATETIME builtins
  datetimeDateRef <- allocateValue (ValUnaryBuiltinFun UnaryDatetimeDate)
  datetimeTimeRef <- allocateValue (ValUnaryBuiltinFun UnaryDatetimeTime)
  datetimeSerialRef <- allocateValue (ValUnaryBuiltinFun UnaryDatetimeSerial)
  datetimeTzNameRef <- allocateValue (ValUnaryBuiltinFun UnaryDatetimeTzName)
  datetimeFromDTZRef <- allocateValue (ValTernaryBuiltinFun TernaryDatetimeFromDTZ)
  toDatetimeRef <- allocateValue (ValUnaryBuiltinFun UnaryToDatetime)
  -- TIMEZONE nullary builtin
  timezoneRef <- allocateValue (ValNullaryBuiltinFun NullaryTimezone)
  -- Ternary string builtins
  substringRef <- allocateValue (ValTernaryBuiltinFun TernarySubstring)
  replaceRef <- allocateValue (ValTernaryBuiltinFun TernaryReplace)
  -- IO/JSON builtins from main
  fetchRef <- allocateValue (ValUnaryBuiltinFun UnaryFetch)
  envRef <- allocateValue (ValUnaryBuiltinFun UnaryEnv)
  jsonEncodeRef <- allocateValue (ValUnaryBuiltinFun UnaryJsonEncode)
  jsonDecodeRef <- allocateValue (ValUnaryBuiltinFun UnaryJsonDecode)
  todayRef <- allocateValue (ValNullaryBuiltinFun NullaryTodaySerial)
  nowRef <- allocateValue (ValNullaryBuiltinFun NullaryNowSerial)
  currentTimeRef <- allocateValue (ValNullaryBuiltinFun NullaryCurrentTime)
  dateFromTextRef <- allocateValue (ValUnaryBuiltinFun UnaryDateValue)
  dateSerialRef <- allocateValue (ValUnaryBuiltinFun UnaryDateSerial)
  dateFromSerialRef <- allocateValue (ValUnaryBuiltinFun UnaryDateFromSerial)
  dateFromDMYRef <- allocateValue (ValTernaryBuiltinFun TernaryDateFromDMY)
  dateDayRef <- allocateValue (ValUnaryBuiltinFun UnaryDateDay)
  dateMonthRef <- allocateValue (ValUnaryBuiltinFun UnaryDateMonth)
  dateYearRef <- allocateValue (ValUnaryBuiltinFun UnaryDateYear)
  timeValueRef <- allocateValue (ValUnaryBuiltinFun UnaryTimeValue)
  everBetweenRef <- allocateValue (ValTernaryBuiltinFun TernaryEverBetween)
  alwaysBetweenRef <- allocateValue (ValTernaryBuiltinFun TernaryAlwaysBetween)
  -- Temporal context switching entry (handled specially by the evaluator)
  evalAsOfSystemTimeRef <- allocateValue (ValAssumed TypeCheck.evalAsOfSystemTimeRef)
  evalUnderValidTimeRef <- allocateValue (ValAssumed TypeCheck.evalUnderValidTimeRef)
  evalUnderRulesEffectiveAtRef <- allocateValue (ValAssumed TypeCheck.evalUnderRulesEffectiveAtRef)
  evalUnderRulesEncodedAtRef <- allocateValue (ValAssumed TypeCheck.evalUnderRulesEncodedAtRef)
  fulfilRef <- allocateValue ValFulfilled
  neverMatchesPartyRef <- allocateValue ValNeverMatchesParty
  neverMatchesActRef <- allocateValue ValNeverMatchesAct
  waitUntilRef <- allocateValue =<< waitUntilVal eventCRef neverMatchesPartyRef neverMatchesActRef
  andRef <- allocateValue =<< andValClosure trueRef falseRef
  orRef <- allocateValue =<< orValClosure trueRef falseRef
  impliesRef <- allocateValue =<< impliesValClosure trueRef falseRef
  notRef <- allocateValue =<< notValClosure trueRef falseRef

  builtinBinOpRefs <-
    traverse
      (\(funVal, uniq) -> do
        r <- allocateValue $ ValBinaryBuiltinFun funVal
        pure (uniq, r)
      )
      builtinBinOps

  pure $
    Map.fromList $
      [ (TypeCheck.falseUnique, falseRef)
      , (TypeCheck.trueUnique,  trueRef)
      , (TypeCheck.emptyUnique, nilRef)
      , (TypeCheck.nothingUnique, nothingRef)
      , (TypeCheck.justUnique, justRef)
      , (TypeCheck.leftUnique, leftRef)
      , (TypeCheck.rightUnique, rightRef)
      , (TypeCheck.evalContractUnique, evalContractRef)
      , (TypeCheck.eventCUnique, eventCRef)
      , (TypeCheck.fulfilUnique, fulfilRef)
      , (TypeCheck.isIntegerUnique, isIntegerRef)
      , (TypeCheck.roundUnique, roundRef)
      , (TypeCheck.ceilingUnique, ceilingRef)
      , (TypeCheck.floorUnique, floorRef)
      , (TypeCheck.sqrtUnique, sqrtRef)
      , (TypeCheck.lnUnique, lnRef)
      , (TypeCheck.log10Unique, log10Ref)
      , (TypeCheck.sinUnique, sinRef)
      , (TypeCheck.cosUnique, cosRef)
      , (TypeCheck.tanUnique, tanRef)
      , (TypeCheck.asinUnique, asinRef)
      , (TypeCheck.acosUnique, acosRef)
      , (TypeCheck.atanUnique, atanRef)
      , (TypeCheck.fetchUnique, fetchRef)
      , (TypeCheck.envUnique, envRef)
      , (TypeCheck.jsonEncodeUnique, jsonEncodeRef)
      , (TypeCheck.jsonDecodeUnique, jsonDecodeRef)
      , (TypeCheck.todaySerialUnique, todayRef)
      , (TypeCheck.nowSerialUnique, nowRef)
      , (TypeCheck.currentTimeUnique, currentTimeRef)
      , (TypeCheck.dateFromTextUnique, dateFromTextRef)
      , (TypeCheck.dateSerialUnique, dateSerialRef)
      , (TypeCheck.dateFromSerialUnique, dateFromSerialRef)
      , (TypeCheck.dateFromDMYUnique, dateFromDMYRef)
      , (TypeCheck.dateDayUnique, dateDayRef)
      , (TypeCheck.dateMonthUnique, dateMonthRef)
      , (TypeCheck.dateYearUnique, dateYearRef)
      , (TypeCheck.timeValueFractionUnique, timeValueRef)
      , (TypeCheck.everBetweenUnique, everBetweenRef)
      , (TypeCheck.alwaysBetweenUnique, alwaysBetweenRef)
      , (TypeCheck.evalAsOfSystemTimeUnique, evalAsOfSystemTimeRef)
      , (TypeCheck.evalUnderValidTimeUnique, evalUnderValidTimeRef)
      , (TypeCheck.evalUnderRulesEffectiveAtUnique, evalUnderRulesEffectiveAtRef)
      , (TypeCheck.evalUnderRulesEncodedAtUnique, evalUnderRulesEncodedAtRef)
      , (TypeCheck.waitUntilUnique, waitUntilRef)
      , (TypeCheck.andUnique, andRef)
      , (TypeCheck.orUnique, orRef)
      , (TypeCheck.impliesUnique, impliesRef)
      , (TypeCheck.notUnique, notRef)
      -- String unary builtins
      , (TypeCheck.stringLengthUnique, stringLengthRef)
      , (TypeCheck.toUpperUnique, toUpperRef)
      , (TypeCheck.toLowerUnique, toLowerRef)
      , (TypeCheck.trimUnique, trimRef)
      , (TypeCheck.toStringUnique, toStringRef)
      , (TypeCheck.toNumberUnique, toNumberRef)
      , (TypeCheck.toDateUnique, toDateRef)
      -- TIME builtins
      , (TypeCheck.timeHourUnique, timeHourRef)
      , (TypeCheck.timeMinuteUnique, timeMinuteRef)
      , (TypeCheck.timeSecondUnique, timeSecondRef)
      , (TypeCheck.timeToSerialUnique, timeToSerialRef)
      , (TypeCheck.timeFromSerialUnique, timeFromSerialRef)
      , (TypeCheck.timeFromHMSUnique, timeFromHMSRef)
      , (TypeCheck.toTimeUnique, toTimeRef)
      -- DATETIME builtins
      , (TypeCheck.datetimeDateUnique, datetimeDateRef)
      , (TypeCheck.datetimeTimeUnique, datetimeTimeRef)
      , (TypeCheck.datetimeSerialUnique, datetimeSerialRef)
      , (TypeCheck.datetimeTzNameUnique, datetimeTzNameRef)
      , (TypeCheck.datetimeFromDTZUnique, datetimeFromDTZRef)
      , (TypeCheck.toDatetimeUnique, toDatetimeRef)
      -- TIMEZONE
      , (TypeCheck.timezoneUnique, timezoneRef)
      -- Ternary string functions
      , (TypeCheck.substringUnique, substringRef)
      , (TypeCheck.replaceUnique, replaceRef)
      ]
      <> builtinBinOpRefs

builtinBinOps :: [(BinOp, Unique)]
builtinBinOps =
  [ (val, unique)
  | (val, uniques) <-
      [ (BinOpLt,        TypeCheck.ltUniques)
      , (BinOpLeq,       TypeCheck.leqUniques)
      , (BinOpGt,        TypeCheck.gtUniques)
      , (BinOpGeq,       TypeCheck.geqUniques)
      , (BinOpPlus,      [TypeCheck.plusUnique])
      , (BinOpMinus,     [TypeCheck.minusUnique])
      , (BinOpTimes,     [TypeCheck.timesUnique])
      , (BinOpDividedBy, [TypeCheck.divideUnique])
      , (BinOpModulo,    [TypeCheck.moduloUnique])
      , (BinOpExponent,  [TypeCheck.exponentUnique])
      , (BinOpTrunc,     [TypeCheck.truncUnique])
      , (BinOpCons,      [TypeCheck.consUnique])
      , (BinOpEquals,    [TypeCheck.equalsUnique])
      -- String binary operations
      , (BinOpContains,   [TypeCheck.containsUnique])
      , (BinOpStartsWith, [TypeCheck.startsWithUnique])
      , (BinOpEndsWith,   [TypeCheck.endsWithUnique])
      , (BinOpIndexOf,    [TypeCheck.indexOfUnique])
      , (BinOpSplit,      [TypeCheck.splitUnique])
      , (BinOpCharAt,     [TypeCheck.charAtUnique])
      , (BinOpWhenLast,   [TypeCheck.whenLastUnique])
      , (BinOpWhenNext,   [TypeCheck.whenNextUnique])
      , (BinOpValueAt,    [TypeCheck.valueAtUnique])
      ]
  , unique <- uniques
  ]

----------------------------------------------------------------------------
-- Clock & parsing utilities
----------------------------------------------------------------------------

evalNullaryBuiltin :: NullaryBuiltinFun -> Machine WHNF
evalNullaryBuiltin = \case
  NullaryTodaySerial -> do
    tc <- getTemporalContext
    case tc.tcDocumentTimezone of
      Just tzName -> do
        mTz <- liftIO $ tryLoadTZ (Text.unpack tzName)
        case mTz of
          Just tz -> do
            let localTime = TZ.utcToLocalTimeTZ tz tc.tcSystemTime
                todayDay = localDay localTime
            pure $ ValDate todayDay
          Nothing ->
            userException $ UserError $
              "Could not load timezone '" <> tzName <> "' for TODAY."
      Nothing ->
        userException $ UserError
          "TIMEZONE is not declared. TODAY requires 'TIMEZONE IS \"<IANA timezone>\"' in your document."
  NullaryNowSerial -> do
    tc <- getTemporalContext
    let tzName = fromMaybe "Etc/UTC" tc.tcDocumentTimezone
    pure $ ValDateTime tc.tcSystemTime tzName
  NullaryTimezone -> do
    tc <- getTemporalContext
    case tc.tcDocumentTimezone of
      Just tzName -> pure $ ValString tzName
      Nothing ->
        userException $ UserError
          "TIMEZONE is not declared. Add 'TIMEZONE IS \"<IANA timezone>\"' to your document."
  NullaryCurrentTime -> do
    tc <- getTemporalContext
    case tc.tcDocumentTimezone of
      Just tzName -> do
        mTz <- liftIO $ tryLoadTZ (Text.unpack tzName)
        case mTz of
          Just tz -> do
            let localTime = TZ.utcToLocalTimeTZ tz tc.tcSystemTime
                tod = localTimeOfDay localTime
            pure $ ValTime tod
          Nothing ->
            userException $ UserError $
              "Could not load timezone '" <> tzName <> "' for CURRENTTIME."
      Nothing ->
        userException $ UserError
          "TIMEZONE is not declared. CURRENTTIME requires 'TIMEZONE IS \"<IANA timezone>\"' in your document."

utcDatestamp :: Time.UTCTime -> Rational
utcDatestamp time =
  fromIntegral (dayNumberFromDay (Time.utctDay time))
    + diffTimeFraction (Time.utctDayTime time)

diffTimeFraction :: Time.DiffTime -> Rational
diffTimeFraction dt =
  let seconds :: Pico
      seconds = realToFrac dt
  in toRational seconds / secondsPerDay

dayNumberFromDay :: Time.Day -> Integer
dayNumberFromDay day =
  Time.diffDays day l4EpochDay - 1

l4EpochDay :: Time.Day
l4EpochDay = Time.fromGregorian 0 1 1

secondsPerDay :: Rational
secondsPerDay = 86400

serialToUTCTime :: Rational -> Time.UTCTime
serialToUTCTime serial =
  let (wholeDays, fraction) = properFraction serial :: (Integer, Rational)
      day = Time.addDays (wholeDays + 1) l4EpochDay
      seconds :: Pico
      seconds = realToFrac (fraction * secondsPerDay)
  in Time.UTCTime day (realToFrac seconds)

dateFormats :: [String]
dateFormats =
  [ "%Y-%m-%d"
  , "%Y/%m/%d"
  , "%Y.%m.%d"
  , "%m/%d/%Y"
  , "%m-%d-%Y"
  , "%m/%d/%y"
  , "%m-%d-%y"
  , "%d/%m/%Y"
  , "%d-%m-%Y"
  , "%d.%m.%Y"
  , "%d-%b-%Y"
  , "%b %d, %Y"
  , "%d %b %Y"
  ]

toDateFormats :: [String]
toDateFormats =
  [ "%Y-%m-%d"
  , "%Y/%m/%d"
  , "%d-%b-%Y"
  , "%d/%m/%Y"
  , "%b %e, %Y"
  ]

parseNumberText :: Text -> Maybe Rational
parseNumberText raw =
  let trimmed = Text.strip raw
  in if Text.null trimmed
       then Nothing
       else do
         sci :: Sci.Scientific <- readMaybe (Text.unpack trimmed)
         pure (toRational sci)

parseDateText :: Text -> Maybe Time.Day
parseDateText raw =
  let trimmed = Text.strip raw
      variants = [trimmed, Text.toUpper trimmed, Text.toLower trimmed]
  in listToMaybe
       [ day
       | candidate <- variants
       , fmt <- toDateFormats
       , Just day <- [TimeFormat.parseTimeM True TimeFormat.defaultTimeLocale fmt (Text.unpack candidate)]
       , let (year, _, _) = Time.toGregorian day
       , year >= 1
       , year <= 9999
       ]

resolveMaybeInnerType :: Maybe (Type' Resolved) -> Machine (Maybe (Type' Resolved))
resolveMaybeInnerType Nothing = pure Nothing
resolveMaybeInnerType (Just ty) =
  case ty of
    TyApp _ maybeRef [inner]
      | nameToText (TypeCheck.getName maybeRef) == "MAYBE" -> pure (Just inner)
    _ -> pure Nothing

-- | Build a DATE value from a Time.Day. Since DATE is now a builtin type,
-- we simply wrap the day in ValDate.
buildDateValue :: Time.Day -> Maybe (Type' Resolved) -> Machine WHNF
buildDateValue day _mInner = pure $ ValDate day

parseDateValueText :: Text -> Either Text Time.Day
parseDateValueText rawInput =
  let trimmed = Text.strip rawInput
  in if Text.null trimmed
       then Left "DATEVALUE: input is empty."
        else
          case firstSuccessful trimmed of
            Nothing -> Left "DATEVALUE: could not parse date string."
            Just day -> Right day
  where
    firstSuccessful :: Text -> Maybe Time.Day
    firstSuccessful txt = go dateFormats
      where
        str = Text.unpack txt
        go = \case
          [] -> Nothing
          fmt : rest ->
            case TimeFormat.parseTimeM True TimeFormat.defaultTimeLocale fmt str of
              Just day -> Just day
              Nothing -> go rest

data AmPm = AM | PM

parseTimeValueText :: Text -> Either Text Rational
parseTimeValueText rawInput =
  let trimmed = Text.strip rawInput
  in if Text.null trimmed
        then Left "TIMEVALUE: input is empty."
        else do
          (timePortion, mSuffix) <- extractSuffix trimmed
          let pieces = Text.splitOn ":" timePortion
          case pieces of
            [hTxt, mTxt] -> buildTime hTxt mTxt "0" mSuffix
            [hTxt, mTxt, sTxt] -> buildTime hTxt mTxt sTxt mSuffix
            _ -> Left "TIMEVALUE: expected HH:MM or HH:MM:SS."

buildTime :: Text -> Text -> Text -> Maybe AmPm -> Either Text Rational
buildTime hTxt mTxt sTxt mSuffix = do
  hourRaw <- parseNatBound "hour" 0 99 hTxt
  minute <- parseNatBound "minute" 0 59 mTxt
  secondsVal <- parseSecondsPart sTxt
  hour24 <- case mSuffix of
    Nothing -> if hourRaw >= 0 && hourRaw < 24
                  then Right hourRaw
                  else Left "TIMEVALUE: hour must be between 0 and 23."
    Just AM ->
      if hourRaw >= 1 && hourRaw <= 12
        then Right (if hourRaw == 12 then 0 else hourRaw)
        else Left "TIMEVALUE: hour must be between 1 and 12 for AM."
    Just PM ->
      if hourRaw >= 1 && hourRaw <= 12
        then Right (if hourRaw == 12 then 12 else hourRaw + 12)
        else Left "TIMEVALUE: hour must be between 1 and 12 for PM."
  let totalSeconds =
        fromIntegral hour24 * 3600
          + fromIntegral minute * 60
          + secondsVal
  if totalSeconds < 0 || totalSeconds >= fromRational secondsPerDay
    then Left "TIMEVALUE: time must be less than 24 hours."
    else Right (toRational totalSeconds / secondsPerDay)

extractSuffix :: Text -> Either Text (Text, Maybe AmPm)
extractSuffix input =
  let trimmed = Text.dropWhileEnd Char.isSpace input
      letters = Text.takeWhileEnd Char.isLetter trimmed
      rest = Text.dropWhileEnd Char.isLetter trimmed
      base = Text.stripEnd rest
  in if Text.null letters
        then Right (base, Nothing)
        else case Text.toCaseFold letters of
          "am" -> Right (base, Just AM)
          "pm" -> Right (base, Just PM)
          _ -> Left "TIMEVALUE: unknown suffix; only AM/PM are supported."

parseNatBound :: Text -> Int -> Int -> Text -> Either Text Int
parseNatBound label lo hi txt =
  case TR.decimal txt of
    Right (value, rest)
      | Text.null rest && value >= lo && value <= hi -> Right value
      | Text.null rest -> Left (label <> " out of range.")
    _ -> Left ("TIMEVALUE: " <> label <> " must be numeric.")

parseSecondsPart :: Text -> Either Text Rational
parseSecondsPart txt =
  let (wholePart, fractionalPart) = Text.breakOn "." txt
  in do
    sec <- parseNatBound "second" 0 59 wholePart
    frac <-
      if Text.null fractionalPart
        then Right 0
        else do
          let digits = Text.drop 1 fractionalPart
          when (Text.null digits) $
            Left "TIMEVALUE: fractional seconds must have digits."
          fracInt <- parseDigits digits
          let denom = 10 ^ Text.length digits :: Integer
          pure (toRational fracInt / toRational denom)
    pure (fromIntegral sec + frac)

parseDigits :: Text -> Either Text Integer
parseDigits txt =
  if Text.all Char.isDigit txt
    then Right (Text.foldl' (\acc ch -> acc * 10 + toInteger (Char.digitToInt ch)) 0 txt)
    else Left "TIMEVALUE: expected only digits in fractional part."

----------------------------------------------------------------------------
-- Timezone utilities
----------------------------------------------------------------------------

-- | Try to load a TZ from the IANA database. Returns Nothing on failure.
tryLoadTZ :: String -> IO (Maybe TZ.TZ)
tryLoadTZ name =
  (Just <$> TZ.loadTZFromDB name) `catch` \(_ :: SomeException) ->
    -- Fall back to the embedded timezone database.  The system DB may be
    -- unavailable when the LSP runs inside VS Code or other sandboxed
    -- environments on macOS.
    pure $ TZAll.tzByLabel <$> TZAll.fromTZName (TE.encodeUtf8 (Text.pack name))

-- | Extract a timezone string from a TIMEZONE IS expression.
-- Handles string literals directly and simple identifiers by peeking at thunks.
extractTimezoneString :: Environment -> Expr Resolved -> Machine (Maybe Text)
extractTimezoneString _env (Lit _ (StringLit _ s)) = pure (Just s)
extractTimezoneString env (App _ nameRef []) = do
  case Map.lookup (getUnique nameRef) env of
    Just refId ->
      pokeThunk refId $ \_ thunk -> case thunk of
        WHNF (ValString s) -> (thunk, Just s)
        Unevaluated _ (Lit _ (StringLit _ s)) _ -> (thunk, Just s)
        _ -> (thunk, Nothing)
    Nothing -> pure Nothing
extractTimezoneString _ _ = pure Nothing

-- | Parse a time string in HH:MM:SS or HH:MM format to TimeOfDay
parseTimeText :: Text -> Maybe TimeOfDay
parseTimeText raw =
  let trimmed = Text.strip raw
      formats = ["%H:%M:%S", "%H:%M:%S%Q", "%H:%M"]
  in listToMaybe
       [ tod
       | fmt <- formats
       , Just tod <- [TimeFormat.parseTimeM True TimeFormat.defaultTimeLocale fmt (Text.unpack trimmed)]
       ]

-- | Parse an ISO-8601 datetime string to UTCTime
parseDatetimeText :: Text -> Maybe UTCTime
parseDatetimeText raw =
  let trimmed = Text.strip raw
      formats =
        [ "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%dT%H:%M:%S%Q%Z"
        , "%Y-%m-%dT%H:%M:%SZ"
        , "%Y-%m-%dT%H:%M:%S%QZ"
        , "%Y-%m-%dT%H:%M:%S%z"
        , "%Y-%m-%dT%H:%M:%S%Q%z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%z"
        ]
  in listToMaybe
       [ utc
       | fmt <- formats
       , Just utc <- [TimeFormat.parseTimeM True TimeFormat.defaultTimeLocale fmt (Text.unpack trimmed)]
       ]

-- | Format a TimeOfDay as HH:MM:SS
formatTimeOfDay :: TimeOfDay -> Text
formatTimeOfDay tod =
  Text.pack $ TimeFormat.formatTime TimeFormat.defaultTimeLocale "%H:%M:%S" tod

-- | Format a UTCTime with timezone offset as ISO-8601
formatDateTimeIso :: UTCTime -> Text -> Text
formatDateTimeIso utc tzName =
  case tryLoadTZPure tzName of
    Just tz ->
      let localTime' = TZ.utcToLocalTimeTZ tz utc
          offset = TZ.timeZoneForUTCTime tz utc
          offsetStr = TimeFormat.formatTime TimeFormat.defaultTimeLocale "%z" offset
      in Text.pack (TimeFormat.formatTime TimeFormat.defaultTimeLocale "%Y-%m-%dT%H:%M:%S" localTime') <> Text.pack offsetStr
    Nothing ->
      -- Fallback: format as UTC
      Text.pack $ TimeFormat.formatTime TimeFormat.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" utc

-- | Pure-ish wrapper for TZ loading (uses unsafePerformIO since TZ data is static)
tryLoadTZPure :: Text -> Maybe TZ.TZ
tryLoadTZPure name = unsafePerformIO $ tryLoadTZ (Text.unpack name)
{-# NOINLINE tryLoadTZPure #-}

-- | Extract TimeOfDay from a WHNF value
expectTimeValue :: WHNF -> Machine TimeOfDay
expectTimeValue (ValTime tod) = pure tod
expectTimeValue val = internalException $ RuntimeTypeError $
  "Expected TIME value but got: " <> prettyLayout val

-- | Extract UTCTime and timezone from a WHNF value
expectDateTimeValue :: WHNF -> Machine (UTCTime, Text)
expectDateTimeValue (ValDateTime utc tz) = pure (utc, tz)
expectDateTimeValue val = internalException $ RuntimeTypeError $
  "Expected DATETIME value but got: " <> prettyLayout val

boolBinOpClosure :: Reference -> Reference -> (Resolved -> Resolved -> Expr Resolved) -> Machine (Value a)
boolBinOpClosure true false buildExpr = do
  let
    mkName = MkName emptyAnno . NormalName
    na = mkName "a"
    nb = mkName "b"
  aDef <- def na
  bDef <- def nb
  aRef <- ref na aDef
  bRef <- ref nb bDef
  pure $ ValClosure
    (MkGivenSig emptyAnno
      [ MkOptionallyTypedName emptyAnno aDef (Just TypeCheck.boolean)
      , MkOptionallyTypedName emptyAnno bDef (Just TypeCheck.boolean)
      ])
    (buildExpr aRef bRef)
    ( Map.fromList
      [ (TypeCheck.trueUnique, true)
      , (TypeCheck.falseUnique, false)
      ]
    )

boolUnaryOpClosure :: Reference -> Reference -> (Resolved -> Expr Resolved) -> Machine (Value a)
boolUnaryOpClosure true false buildExpr = do
  let
    mkName = MkName emptyAnno . NormalName
    na = mkName "a"
  aDef <- def na
  aRef <- ref na aDef
  pure $ ValClosure
    (MkGivenSig emptyAnno
      [ MkOptionallyTypedName emptyAnno aDef (Just TypeCheck.boolean)
      ])
    (buildExpr aRef)
    ( Map.fromList
      [ (TypeCheck.trueUnique, true)
      , (TypeCheck.falseUnique, false)
      ]
    )

andValClosure :: Reference -> Reference -> Machine (Value a)
andValClosure true false =
  boolBinOpClosure true false
    (\aRef bRef ->
      IfThenElse emptyAnno
        (Var emptyAnno aRef)
        (Var emptyAnno bRef)
        falseExpr
    )

notValClosure :: Reference -> Reference -> Machine (Value a)
notValClosure true false =
  boolUnaryOpClosure true false
    (\aRef ->
      IfThenElse emptyAnno
        (Var emptyAnno aRef)
        falseExpr
        trueExpr
    )

orValClosure :: Reference -> Reference -> Machine (Value a)
orValClosure true false =
  boolBinOpClosure true false
    (\aRef bRef ->
      IfThenElse emptyAnno
        (Var emptyAnno aRef)
        trueExpr
        (Var emptyAnno bRef)
    )

impliesValClosure :: Reference -> Reference -> Machine (Value a)
impliesValClosure true false =
  boolBinOpClosure true false
    (\aRef bRef ->
      IfThenElse emptyAnno
        (Var emptyAnno aRef)
        (Var emptyAnno bRef)
        trueExpr
    )

----------------------------------------------------------------------------
-- JSON to Environment conversion for batch processing
----------------------------------------------------------------------------

-- | Write JSON values into existing References in the environment.
-- This should be called after preAllocate has created References for ASSUME'd variables.
-- The function looks up ASSUME'd variables by name from EntityInfo,
-- finds their References in the provided environment, and writes JSON values into them.
writeJSONToReferences :: Aeson.Value -> Environment -> Machine ()
writeJSONToReferences json env = case json of
  Aeson.Object obj -> do
    entityInfo <- getEntityInfo
    let assumedVars =
          [ (u, n, ty)
          | (u, (n, TypeCheck.KnownTerm ty Assumed)) <- Map.toList entityInfo
          ]
    forM_ assumedVars $ \(unique, name, ty) -> do
      let key = nameToText (TypeCheck.getName name)
      case KeyMap.lookup (Key.fromText key) obj of
        Nothing -> pure ()  -- No JSON value for this variable
        Just val -> do
          -- Look up the existing Reference for this variable
          case Map.lookup unique env of
            Nothing -> pure ()  -- No Reference found (shouldn't happen after preAllocate)
            Just existingRef -> do
              -- Convert JSON to WHNF and write into the existing Reference
              whnf <- jsonValueToWHNFTyped val ty
              updateThunkToWHNF existingRef whnf
  _ -> pure ()
