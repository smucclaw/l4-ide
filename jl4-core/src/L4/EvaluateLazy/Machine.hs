{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module L4.EvaluateLazy.Machine
( Frame
, EvalException (..)
, UserEvalException (..)
, Machine (Allocate, AllocateValue, PreAllocate, ..)
, Allocation (..)
, Config (..)
, forwardExpr
, backward
, EvalDirective (..)
, evalModule
, maximumStackSize
, initialEnvironment
, evalRef
, emptyEnvironment
, prettyEvalException
)
where

import Base
import qualified Base.Text as Text
import qualified Base.Map as Map
import qualified Base.Set as Set
import Control.Concurrent
import L4.Annotation
import L4.Evaluate.Operators
import L4.Evaluate.ValueLazy
import L4.Parser.SrcSpan (SrcRange)
import L4.Print
import L4.Syntax
import qualified L4.TypeCheck as TypeCheck
import L4.EvaluateLazy.ContractFrame
import L4.Utils.Ratio

data Frame =
    BinOp1 BinOp {- -} (Expr Resolved) Environment
  | BinOp2 BinOp WHNF {- -}
  | App1 {- -} [Reference]
  | IfThenElse1 {- -} (Expr Resolved) (Expr Resolved) Environment
  | ConsiderWhen1 Reference {- -} (Expr Resolved) [Branch Resolved] Environment
  | PatNil0
  | PatCons0 (Pattern Resolved) (Pattern Resolved)
  | PatCons1 {- -} Reference (Pattern Resolved)
  | PatCons2 Environment {- -}
  | PatApp0 Resolved [Pattern Resolved]
  | PatApp1 [Environment] {- -} [(Reference, Pattern Resolved)]
  | EqConstructor1 {- -} Reference [(Reference, Reference)]
  | EqConstructor2 WHNF {- -} [(Reference, Reference)]
  | EqConstructor3 {- -} [(Reference, Reference)]
  | UnaryBuiltin0 UnaryBuiltinFun
  | UpdateThunk Reference
  | ContractFrame ContractFrame
  deriving stock Show

data EvalException =
    InternalEvalException InternalEvalException
  | UserEvalException UserEvalException
  deriving stock (Generic, Show)
  deriving anyclass NFData

data InternalEvalException =
    RuntimeScopeError Resolved -- internal
  | RuntimeTypeError Text -- internal
  | PrematureGC -- internal
  | DanglingPointer -- internal
  | UnhandledPatternMatch -- internal
  deriving stock (Generic, Show)
  deriving anyclass NFData

data UserEvalException =
    BlackholeForced (Expr Resolved)
  | EqualityOnUnsupportedType
  | NonExhaustivePatterns Reference -- we could try to warn statically
  | StackOverflow
  | DivisionByZero BinOp
  | NotAnInteger BinOp Rational
  | Stuck Resolved -- ^ stores the term we got stuck on
  deriving stock (Generic, Show)
  deriving anyclass NFData

data Machine a where
  Config :: a -> Machine a
  Exception :: EvalException -> Machine a
  WithPoppedFrame :: (Maybe Frame -> Machine a) -> Machine a
  PushFrame :: Frame -> Machine ()
  Allocate' :: Allocation t -> Machine t
  NewUnique :: Machine Unique
  PokeThunk :: Reference
    -> (ThreadId -> Thunk -> (Thunk, a))
    -> Machine a
  Bind :: Machine a -> (a -> Machine b) -> Machine b

data Allocation t where
  Recursive :: Expr Resolved -> (Reference -> Environment) -> Allocation (Reference, Environment)
  Value :: WHNF -> Allocation Reference
  PreAllocation :: Resolved -> Allocation (Unique, Reference)

instance Functor Machine where
  fmap = liftM

instance Applicative Machine where
  (<*>) = ap
  pure = Config

instance Monad Machine where
  (>>=) = Bind

pattern Allocate :: Expr Resolved -> (Reference -> Environment) -> Machine (Reference, Environment)
pattern Allocate expr k = Allocate' (Recursive expr k)

pattern AllocateValue :: WHNF -> Machine Reference
pattern AllocateValue whnf = Allocate' (Value whnf)

pattern PreAllocate ::Resolved -> Machine (Unique, Reference)
pattern PreAllocate r = Allocate' (PreAllocation r)

data Config
  = ForwardMachine Environment (Expr Resolved)
  | BackwardMachine WHNF
  | EvalRefMachine Reference
  | DoneMachine WHNF

pattern ForwardExpr :: Environment -> Expr Resolved -> Machine Config
pattern ForwardExpr env e = Config (ForwardMachine env e)

pattern Backward :: WHNF -> Machine Config
pattern Backward whnf = Config (BackwardMachine whnf)

pattern EvalRef :: Reference -> Machine Config
pattern EvalRef ref = Config (EvalRefMachine ref)

pattern InternalException :: InternalEvalException -> Machine a
pattern InternalException e = Exception (InternalEvalException e)

pattern UserException :: UserEvalException -> Machine a
pattern UserException e = Exception (UserEvalException e)

pattern Done :: WHNF -> Machine Config
pattern Done whnf = Config (DoneMachine whnf)

forwardExpr :: Environment -> Expr Resolved -> Machine Config
forwardExpr env (And _ann e1 e2) =
  ForwardExpr env (IfThenElse emptyAnno e1 e2 falseExpr)
forwardExpr env (Or _ann e1 e2) =
  ForwardExpr env (IfThenElse emptyAnno e1 trueExpr e2)
forwardExpr env (Implies _ann e1 e2) =
  ForwardExpr env (IfThenElse emptyAnno e1 e2 trueExpr)
forwardExpr env (Not _ann e) =
  ForwardExpr env (IfThenElse emptyAnno e falseExpr trueExpr)
forwardExpr env (Equals _ann e1 e2) = do
  PushFrame (BinOp1 BinOpEquals e2 env)
  ForwardExpr env e1
forwardExpr env (Plus _ann e1 e2) = do
  PushFrame (BinOp1 BinOpPlus e2 env)
  ForwardExpr env e1
forwardExpr env (Minus _ann e1 e2) = do
  PushFrame (BinOp1 BinOpMinus e2 env)
  ForwardExpr env e1
forwardExpr env (Times _ann e1 e2) = do
  PushFrame (BinOp1 BinOpTimes e2 env)
  ForwardExpr env e1
forwardExpr env (DividedBy _ann e1 e2) = do
  PushFrame (BinOp1 BinOpDividedBy e2 env)
  ForwardExpr env e1
forwardExpr env (Modulo _ann e1 e2) = do
  PushFrame (BinOp1 BinOpModulo e2 env)
  ForwardExpr env e1
forwardExpr env (Leq _ann e1 e2) = do
  PushFrame (BinOp1 BinOpLeq e2 env)
  ForwardExpr env e1
forwardExpr env (Geq _ann e1 e2) = do
  PushFrame (BinOp1 BinOpGeq e2 env)
  ForwardExpr env e1
forwardExpr env (Lt _ann e1 e2) = do
  PushFrame (BinOp1 BinOpLt e2 env)
  ForwardExpr env e1
forwardExpr env (Gt _ann e1 e2) = do
  PushFrame (BinOp1 BinOpGt e2 env)
  ForwardExpr env e1
forwardExpr env (Proj _ann e l) =
  ForwardExpr env (App emptyAnno l [e]) -- we desugar projection to plain function application
forwardExpr env (Var _ann n) = -- still problematic: similarity / overlap between this and App with no args
  expectTerm env n >>= EvalRef
forwardExpr env (Cons _ann e1 e2) = do
  rf1 <- allocate_ e1 env
  rf2 <- allocate_ e2 env
  Backward (ValCons rf1 rf2)
forwardExpr env (Lam _ann givens e) =
  Backward (ValClosure givens e env)
forwardExpr env (App _ann n []) =
  expectTerm env n >>= EvalRef
forwardExpr env (App _ann n es@(_ : _)) = do
  rs <- traverse (`allocate_` env) es
  PushFrame (App1 rs)
  ForwardExpr env (Var emptyAnno n)
forwardExpr env (AppNamed ann n [] _) =
  ForwardExpr env (App ann n [])
forwardExpr _env (AppNamed _ann _n _nes Nothing) =
  InternalException $ RuntimeTypeError
    "named application where the order of arguments is not resolved"
forwardExpr env (AppNamed ann n nes (Just order)) =
  let
    -- move expressions into order, drop names
    es = (\ (MkNamedExpr _ _ e) -> e) . snd <$> sortOn fst (zip order nes)
  in
    ForwardExpr env (App ann n es)
forwardExpr env (IfThenElse _ann e1 e2 e3) = do
  PushFrame (IfThenElse1 e2 e3 env)
  ForwardExpr env e1
forwardExpr env (Consider _ann e branches) = do
  rf <- allocate_ e env
  matchBranches rf env branches
forwardExpr _env (Lit _ann lit) = do
  rval <- runLit lit
  Backward rval
forwardExpr _env (List _ann []) =
  Backward ValNil
forwardExpr env (List _ann (e : es)) =
  ForwardExpr env (Cons emptyAnno e (List emptyAnno es))
forwardExpr env (Where _ann e ds) = do
  env' <- evalRecLocalDecls env ds
  let combinedEnv = Map.union env' env
  ForwardExpr combinedEnv e
forwardExpr env (Regulative _ann (MkObligation _ party action due followup)) = do
  Backward (ValObligation env (Right party) (Right action) due (fromMaybe fulfilExpr followup))
forwardExpr env (Event _ann ev) =
  ForwardExpr env (desugarEvent ev)

backward :: WHNF -> Machine Config
backward val = WithPoppedFrame $ \ case
  Nothing -> Done val
  Just (BinOp1 binOp e2 env) -> do
    PushFrame (BinOp2 binOp val)
    ForwardExpr env e2
  Just (BinOp2 binOp val1) -> do
    runBinOp binOp val1 val
  Just f@(App1 rs) -> do
    case val of
      ValClosure givens e env' -> do
        env'' <- matchGivens givens f rs
        ForwardExpr (Map.union env'' env') e
      ValUnappliedConstructor r -> do
        Backward (ValConstructor r rs)
      ValObligation env party act due followup -> do
        (events, time) <- case rs of
          [r, t] -> pure (r, t)
          rs' -> InternalException $ RuntimeTypeError $
            "expected a list of events, and a time stamp but found: " <> foldMap prettyLayout rs'
        PushFrame (ContractFrame (Contract1 ScrutEvents {..}))
        EvalRef events
      v@(ValConstructor r []) | r `sameResolved` TypeCheck.fulfilRef -> backward v
      ValUnaryBuiltinFun fn -> do
        r <- expect1 rs
        PushFrame (UnaryBuiltin0 fn)
        EvalRef r
      ValAssumed r ->
        stuckOnAssumed r -- TODO: we can do better here
      res -> InternalException (RuntimeTypeError $ "expected a function but found: " <> prettyLayout res)
  Just (IfThenElse1 e2 e3 env) ->
    case boolView val of
      Just True ->
        ForwardExpr env e2
      Just False ->
        ForwardExpr env e3
      Nothing | ValAssumed r <- val ->
        stuckOnAssumed r
      Nothing ->
        InternalException $ RuntimeTypeError $
          "expected a BOOLEAN but found: " <> prettyLayout val <> " when evaluating IF-THEN-ELSE"
  Just (ConsiderWhen1 _scrutinee e _branches env) -> do
    case val of
      ValEnvironment env' ->
        ForwardExpr (Map.union env' env) e
      _ ->
        InternalException $ RuntimeTypeError $
          "expected an environment but found: " <> prettyLayout val <> " when evaluating WHEN"
  Just PatNil0 -> do
    case val of
      ValNil ->
        Backward (ValEnvironment Map.empty)
      _ ->
        patternMatchFailure
  Just (PatCons0 p1 p2) -> do
    case val of
      ValCons rf1 rf2 -> do
        PushFrame (PatCons1 rf2 p2)
        matchPattern rf1 p1
      _ ->
        patternMatchFailure
  Just (PatCons1 rf2 p2) -> do
    case val of
      ValEnvironment env1 -> do
        PushFrame (PatCons2 env1)
        matchPattern rf2 p2
      _ ->
        InternalException $ RuntimeTypeError $
          "expected an environment but found: " <> prettyLayout val <> " when matching FOLLOWED BY"
  Just (PatCons2 env1) ->
    case val of
      ValEnvironment env2 ->
        Backward (ValEnvironment (Map.union env2 env1))
      _ -> InternalException $ RuntimeTypeError $
        "expected an environment but found: " <> prettyLayout val <> " when matching FOLLOWED BY"
  Just (PatApp0 n ps) ->
    case val of
      ValConstructor n' rfs
        | sameResolved n n' ->
          if length rfs == length ps
            then
              let
                pairs = zip rfs ps
              in
                case pairs of
                  []             -> backward (ValEnvironment Map.empty)
                  ((r, p) : rps) -> do
                    PushFrame (PatApp1 [] rps)
                    matchPattern r p
            else InternalException $ RuntimeTypeError
              "pattern for constructor has the wrong number of arguments"
      _ ->
        patternMatchFailure
  Just (PatApp1 envs rps) ->
    case val of
      ValEnvironment env ->
        case rps of
          []              -> Backward (ValEnvironment (Map.unions (env : envs)))
          ((r, p) : rps') -> do
            PushFrame (PatApp1 (env : envs) rps')
            matchPattern r p
      _ -> InternalException $ RuntimeTypeError $
        "expected an environment but found: " <> prettyLayout val <> " when matching constructor"
  Just (EqConstructor1 rf rfs) -> do
    PushFrame (EqConstructor2 val rfs)
    EvalRef rf
  Just (EqConstructor2 val1 rfs) -> do
    PushFrame (EqConstructor3 rfs)
    runBinOpEquals val1 val
  Just (EqConstructor3 rfs) ->
    case boolView val of
      Just False -> Backward $ valBool False
      Just True ->
        case rfs of
          [] -> Backward $ valBool True
          ((r1, r2) : rfs') -> do
            PushFrame (EqConstructor1 r2 rfs')
            EvalRef r1
      Nothing -> InternalException $ RuntimeTypeError $
        "expected a BOOLEAN but found: " <> prettyLayout val <> " when testing equality"
  Just (UnaryBuiltin0 fn) -> do
    runBuiltin val fn
  Just (UpdateThunk rf) -> do
    updateThunkToWHNF rf val
    Backward val
  Just (ContractFrame cFrame) -> backwardContractFrame val cFrame

backwardContractFrame :: Value Reference -> ContractFrame -> Machine Config
backwardContractFrame val = \case
  Contract1 ScrutEvents {..} -> do
    case val of
      ValCons e es -> do
        pushCFrame (Contract2 ScrutEvent {events = es, ..})
        EvalRef e
      ValNil -> Backward (ValObligation env party act due followup)
      _ -> InternalException $ RuntimeTypeError $
        "expected LIST EVENT but found: " <> prettyLayout val <> " when scrutinizing regulative events"
  Contract2 ScrutEvent {..} -> do
    (ev'party, ev'act, ev'time) <- case val of
      ValConstructor n [p, a, t]  | n `sameResolved` TypeCheck.eventCRef -> pure (p, a, t)
      _ -> InternalException $ RuntimeTypeError $
        "expected an EVENT but found: " <> prettyLayout val <> " when scrutinizing a regulative event"
    pushCFrame (Contract3 PartyWHNF {..})
    maybeEvaluate env party
  Contract3 PartyWHNF {..} -> do
    pushCFrame (Contract3' PartyEqual {party = val, ..})
    EvalRef ev'party
  Contract3' PartyEqual {..} -> do
    pushCFrame (Contract4 ScrutParty {ev'party = val, ..})
    runBinOpEquals party val
  Contract4 ScrutParty {..} -> do
    case boolView val of
      Nothing -> InternalException $ RuntimeTypeError $
        "expected BOOLEAN but found: " <> prettyLayout val
      Just True -> do
        pushCFrame (Contract5 ActWHNF {..})
        maybeEvaluate env act
      Just False -> continueWithNextEvent ScrutEvents {party = Left party, ..} events
  Contract5 ActWHNF {..} -> do
    pushCFrame (Contract5' ActEqual {act = val, ..})
    EvalRef ev'act
  Contract5' ActEqual {..} -> do
    pushCFrame (Contract6 ScrutAct {ev'act = val, ..})
    runBinOpEquals act val
  Contract6 ScrutAct {..} -> do
    case boolView val of
      Nothing -> InternalException $ RuntimeTypeError $
        "expected BOOLEAN but found: " <> prettyLayout val
      Just True -> case due of
        Just due' -> do
          pushCFrame (Contract7 StampWHNF {..})
          forwardExpr env due'
        Nothing -> continueWithFollowup env followup events ev'time
      Just False -> continueWithNextEvent ScrutEvents {party = Left party, act = Left act, ..} events
  Contract7 StampWHNF {..} -> do
    pushCFrame (Contract8 CurTimeWHNF {due = val, ..})
    EvalRef ev'time
  Contract8 CurTimeWHNF {..} -> do
    pushCFrame (Contract9 ScrutTime {ev'time = val, ..})
    EvalRef time
  Contract9 ScrutTime {..} -> do
    let assertTime = \case
          ValNumber i -> pure i
          v -> InternalException $ RuntimeTypeError $
            "expected a NUMBER but got: " <> prettyLayout v
    stamp <- assertTime ev'time
    due' <- assertTime due
    time <- assertTime val
    let deadline = time + due'
    if stamp > deadline
      then backward (ValBreached (DeadlineMissed ev'party ev'act ev'time deadline))
      else do
        -- NOTE: this is not too nice, but not wanting this would require to change `App1` to take MaybeEvaluated's
        timeR <- AllocateValue ev'time
        continueWithFollowup env followup events timeR
  where
    continueWithNextEvent :: ScrutEvents -> Reference -> Machine Config
    continueWithNextEvent frame events = do
      pushCFrame (Contract1 frame)
      EvalRef events

    pushCFrame = PushFrame . ContractFrame

    continueWithFollowup :: Environment -> RExpr -> Reference -> Reference -> Machine Config
    continueWithFollowup env followup events time = do
      PushFrame (App1 [events, time])
      ForwardExpr env followup

maybeEvaluate :: Environment -> MaybeEvaluated -> Machine Config
maybeEvaluate env = either Backward (ForwardExpr env)

matchGivens :: GivenSig Resolved -> Frame -> [Reference] -> Machine Environment
matchGivens (MkGivenSig _ann otns) f es = do
  let others = foldMap (either (const []) (\x -> [fst x]) . TypeCheck.isQuantifier) otns
  matchGivens' others f es

matchGivens' :: [Resolved] -> Frame -> [Reference] -> Machine (Map Unique Reference)
matchGivens' ns f rs = do
  if length ns == length rs
    then do
      pure $ Map.fromList (zipWith (\ r v -> (getUnique r, v)) ns rs)
    else do
      PushFrame f -- provides better error context
      InternalException $
        RuntimeTypeError "given signatures' values' lengths do not match"

matchBranches :: Reference -> Environment -> [Branch Resolved] -> Machine Config
matchBranches scrutinee _env [] =
  UserException (NonExhaustivePatterns scrutinee)
matchBranches _scrutinee env (Otherwise _ann e : _) =
  ForwardExpr env e
matchBranches scrutinee env (When _ann pat e : branches) = do
  PushFrame (ConsiderWhen1 scrutinee e branches env)
  matchPattern scrutinee pat

matchPattern :: Reference -> Pattern Resolved -> Machine Config
matchPattern scrutinee (PatVar _ann n) = do
  Backward (ValEnvironment (Map.singleton (getUnique n) scrutinee))
matchPattern scrutinee (PatApp _ann n [])
  | getUnique n == TypeCheck.emptyUnique = do -- pattern for the empty list
  PushFrame PatNil0
  EvalRef scrutinee
matchPattern scrutinee (PatCons _ann p1 p2) = do
  PushFrame (PatCons0 p1 p2 )
  EvalRef scrutinee
matchPattern scrutinee (PatApp _ann n ps) = do
  PushFrame (PatApp0 n ps )
  EvalRef scrutinee

-- | This unwinds the stack until it finds the enclosing pattern match and then resumes.
patternMatchFailure :: Machine Config
patternMatchFailure = WithPoppedFrame $ \ case
  Nothing ->
    InternalException UnhandledPatternMatch
  Just (ConsiderWhen1 scrutinee _ branches env) ->
    matchBranches scrutinee env branches
  Just _ ->
    patternMatchFailure

runLit :: Lit -> Machine WHNF
runLit (NumericLit _ann num) = pure (ValNumber num)
runLit (StringLit _ann str)  = pure (ValString str)

expect1 :: [a] -> Machine a
expect1 = \case
  [x] -> pure x
  xs -> InternalException (RuntimeTypeError $ "Expected 1 argument, but got " <> Text.show (length xs))

expectNumber :: WHNF -> Machine Rational
expectNumber = \case
  ValNumber f -> pure f
  _ -> InternalException (RuntimeTypeError "Expected number.")

expectInteger :: BinOp -> Rational -> Machine Integer
expectInteger op n = do
  case isInteger n of
    Nothing -> UserException (NotAnInteger op n)
    Just i -> pure i

runBuiltin :: WHNF -> UnaryBuiltinFun -> Machine Config
runBuiltin es op = do
  val :: Rational <- expectNumber es
  Backward case op of
    UnaryIsInteger -> valBool $ isJust $ isInteger val
    UnaryRound -> valInt $ round val
    UnaryCeiling -> valInt $ ceiling val
    UnaryFloor -> valInt $ floor val
  where
    valInt :: Integer -> WHNF
    valInt = ValNumber . toRational

runBinOp :: BinOp -> WHNF -> WHNF -> Machine Config
runBinOp BinOpPlus   (ValNumber num1) (ValNumber num2)           = Backward $ ValNumber (num1 + num2)
runBinOp BinOpMinus  (ValNumber num1) (ValNumber num2)           = Backward $ ValNumber (num1 - num2)
runBinOp BinOpTimes  (ValNumber num1) (ValNumber num2)           = Backward $ ValNumber (num1 * num2)
runBinOp BinOpDividedBy (ValNumber num1) (ValNumber num2)        = do
  if num2 /= 0
    then Backward $ ValNumber (num1 / num2)
    else UserException (DivisionByZero BinOpDividedBy)
runBinOp BinOpModulo    (ValNumber num1) (ValNumber num2)      = do
  n1 <- expectInteger BinOpModulo num1
  n2 <- expectInteger BinOpModulo num2
  if n2 /= 0
    then Backward $ ValNumber (toRational $ n1 `mod` n2)
    else UserException (DivisionByZero BinOpModulo)
runBinOp BinOpEquals val1             val2                       = runBinOpEquals val1 val2
runBinOp BinOpLeq    (ValNumber num1) (ValNumber num2)           = Backward $ ValBool (num1 <= num2)
runBinOp BinOpLeq    (ValString str1) (ValString str2)           = Backward $ ValBool (str1 <= str2)
runBinOp BinOpLeq    (ValBool b1)     (ValBool b2)               = Backward $ ValBool (b1 <= b2)
runBinOp BinOpGeq    (ValNumber num1) (ValNumber num2)           = Backward $ ValBool (num1 >= num2)
runBinOp BinOpGeq    (ValString str1) (ValString str2)           = Backward $ ValBool (str1 >= str2)
runBinOp BinOpGeq    (ValBool b1)     (ValBool b2)               = Backward $ ValBool (b1 >= b2)
runBinOp BinOpLt     (ValNumber num1) (ValNumber num2)           = Backward $ ValBool (num1 < num2)
runBinOp BinOpLt     (ValString str1) (ValString str2)           = Backward $ ValBool (str1 < str2)
runBinOp BinOpLt     (ValBool b1)     (ValBool b2)               = Backward $ ValBool (b1 < b2)
runBinOp BinOpGt     (ValNumber num1) (ValNumber num2)           = Backward $ ValBool (num1 > num2)
runBinOp BinOpGt     (ValString str1) (ValString str2)           = Backward $ ValBool (str1 > str2)
runBinOp BinOpGt     (ValBool b1)     (ValBool b2)               = Backward $ ValBool (b1 > b2)
runBinOp _op         (ValAssumed r) _e2                          = stuckOnAssumed r
runBinOp _op         _e1 (ValAssumed r)                          = stuckOnAssumed r
runBinOp _           _                _                          = InternalException (RuntimeTypeError "running bin op with invalid operation / value combination")

runBinOpEquals :: WHNF -> WHNF -> Machine Config
runBinOpEquals (ValNumber num1)        (ValNumber num2) = Backward $ valBool $ num1 == num2
runBinOpEquals (ValString str1)        (ValString str2) = Backward $ valBool $ str1 == str2
runBinOpEquals ValNil                  ValNil           = Backward $ valBool True
runBinOpEquals (ValCons r1 rs1)        (ValCons r2 rs2) = do
  PushFrame (EqConstructor1 r2 [(rs1, rs2)])
  EvalRef r1
runBinOpEquals (ValConstructor n1 rs1) (ValConstructor n2 rs2)
  | sameResolved n1 n2 && length rs1 == length rs2 =
    let
      pairs = zip rs1 rs2
    in
      case pairs of
        [] -> backward $ ValBool True
        ((r1, r2) : rss) -> do
          PushFrame (EqConstructor1 r2 rss)
          EvalRef r1
  | otherwise                                           = Backward $ ValBool False
-- TODO: we probably also want to check ValObligations for equality
runBinOpEquals _                       _                = UserException EqualityOnUnsupportedType

pattern ValBool :: Bool -> WHNF
pattern ValBool b <- (boolView -> Just b)
  where
    ValBool b = valBool b

valBool :: Bool -> WHNF
valBool False = falseVal
valBool True  = trueVal

-- | Checks if a value is a Boolean constructor.
boolView :: WHNF -> Maybe Bool
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
  u <- NewUnique
  pure (Def u n)

ref :: Name -> Resolved -> Machine Resolved
ref n a =
  let
    (u, o) = getUniqueName a
  in
    pure (Ref n u o)


stuckOnAssumed :: Resolved -> Machine b
stuckOnAssumed assumedResolved = UserException (Stuck assumedResolved)

lookupTerm :: Environment -> Resolved -> Maybe Reference
lookupTerm env r =
  Map.lookup (getUnique r) env

expectTerm :: Environment -> Resolved -> Machine Reference
expectTerm env r =
  case lookupTerm env r of
    Nothing -> InternalException (RuntimeScopeError r)
    Just rf -> pure rf

updateThunk :: Reference -> Thunk -> Machine ()
updateThunk rf !thunk = PokeThunk rf \_ _ -> (thunk, ())

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
evalRef rf =
  join $ PokeThunk rf \tid -> \case
    thunk@(WHNF val) -> (thunk, backward val)
    thunk@(Unevaluated tids e env)
      | tid `Set.member` tids ->  (thunk, UserException (BlackholeForced e))
      | otherwise -> (Unevaluated (Set.insert tid tids) e env, PushFrame (UpdateThunk rf) *> ForwardExpr env e)

-- | Recursive pre-allocation, used for mutually recursive let-bindings / declarations.
preAllocate :: [Resolved] -> Machine Environment
preAllocate ns = do
  pairs <- traverse PreAllocate ns
  pure (Map.fromList pairs)

allocate_ :: Expr Resolved -> Environment -> Machine Reference
allocate_ (Var _ann n) env = do
  -- special case where we do not actually need to allocate
  expectTerm env n
allocate_ expr env =
  fst <$> Allocate expr (const env)

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
scanConDecl (MkConDecl _ann n tns) = pure (n : ((\ (MkTypedName _ n' _) -> n') <$> tns))

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

evalDirective :: Environment -> Directive Resolved -> Machine [EvalDirective]
evalDirective env (LazyEval ann expr) =
  pure ((\ r -> MkEvalDirective r expr env) <$> toList (rangeOf ann))
evalDirective _env (StrictEval _ann _expr) =
  pure []
evalDirective _env (Check _ann _expr) =
  pure []
evalDirective env (Contract ann expr t evs) =
  evalDirective env . LazyEval ann =<< contractToEvalDirective expr t evs

contractToEvalDirective :: Expr Resolved -> Expr Resolved -> [Expr Resolved] -> Machine (Expr Resolved)
contractToEvalDirective contract t evs = do
  evs' <- evListExpr
  pure $ App emptyAnno TypeCheck.evalContractRef [contract, evs', t]
  where
  evListExpr = List emptyAnno <$> traverse eventExpr evs

eventExpr :: Expr Resolved -> Machine (Expr Resolved)
eventExpr (Event _ann ev) = pure $ desugarEvent ev
eventExpr o = InternalException $ RuntimeTypeError $ "expected an EVENT, but got " <> prettyLayout o

desugarEvent :: Event Resolved -> Expr Resolved
desugarEvent (MkEvent ann party act timestamp) = App ann TypeCheck.eventCRef [party, act, timestamp]

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
  traverse_ (\ (i, MkTypedName _ sn _t) -> do
    arg    <- def (TypeCheck.getName n)
    argRef <- ref (TypeCheck.getName n) arg
    args <- traverse (def . TypeCheck.getName) tns
    body <- ref (TypeCheck.getName sn) (args !! i)
    let
      sel =
        ValClosure
          (MkGivenSig emptyAnno [MkOptionallyTypedName emptyAnno arg Nothing])      -- \ x ->
          (Consider emptyAnno (App emptyAnno argRef [])                             -- case x of
            [ When emptyAnno (PatApp emptyAnno conRef (PatVar emptyAnno <$> args))  --   Con y_1 ... y_n ->
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

falseVal :: WHNF
falseVal = ValConstructor TypeCheck.falseRef []

trueExpr :: Expr Resolved
trueExpr = App emptyAnno TypeCheck.trueRef []

trueVal :: WHNF
trueVal = ValConstructor TypeCheck.trueRef []

fulfilExpr :: Expr Resolved
fulfilExpr = App emptyAnno TypeCheck.fulfilRef []

fulfilVal :: Value a
fulfilVal = ValConstructor TypeCheck.fulfilRef []

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

-- EVENT :: party -> act -> Number -> EVENT party act
eventCVal :: Value a
eventCVal = ValUnappliedConstructor TypeCheck.eventCRef

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

data EvalDirective =
  MkEvalDirective
    { range :: !SrcRange -- ^ of the (L)EVAL directive
    , expr  :: !(Expr Resolved) -- ^ expression to evaluate
    , env   :: !Environment -- ^ environment to evaluate the expression in
    }
  deriving stock (Generic, Show)

-----------------------------------------------------------------------------
-- Prettyprinting of the EvalExceptions
-----------------------------------------------------------------------------

prettyEvalException :: EvalException -> [Text]
prettyEvalException (InternalEvalException exc) = wrapInternal (prettyInternalEvalException exc)
  where
    wrapInternal :: [Text] -> [Text]
    wrapInternal msgs = [ "Internal error:" ] <> msgs <> [ "Please report this as a bug." ]
prettyEvalException (UserEvalException exc)     = prettyUserEvalException exc

prettyInternalEvalException :: InternalEvalException -> [Text]
prettyInternalEvalException = \case
  RuntimeScopeError r ->
    indentMany r
    <> [ "is not in scope." ]
  RuntimeTypeError err ->
    [ "I encountered a type error during evaluation:" ]
    <> [ indentSingle err ]
  PrematureGC ->
    [ "Trying to access an address that has already been garbage-collected." ]
  DanglingPointer ->
    [ "Trying to access an address that is not on the abstract machine heap." ]
  UnhandledPatternMatch ->
    [ "Unhandled pattern match failure." ]

indentSingle :: Text -> Text
indentSingle = ("  " <>)

indentMany :: LayoutPrinter a => a -> [Text]
indentMany = map ind . Text.lines .  prettyLayout
  where
    ind = ("  " <>)

prettyUserEvalException :: UserEvalException -> [Text]
prettyUserEvalException = \case
  BlackholeForced expr ->
    [ "Infinite loop detected while trying to evaluate:"
    , prettyLayout expr ]
  EqualityOnUnsupportedType -> ["Trying to check equality on types that do not support it."]
  NonExhaustivePatterns val ->
    [ "Value" ]
    <> indentMany val
    <> [ "has no corresponding pattern." ]
  StackOverflow ->
    [ "Stack overflow: "
    , "Recursion depth of " <> Text.show maximumStackSize
    , "exceeded." ]
  DivisionByZero op ->
    [ "Division by zero in the operation:"
    , prettyLayout op
    ]
  NotAnInteger op num ->
    [ "Expected an Integer but got the fractional number: " ]
    <> [ prettyRatio num ]
    <> [ "During the evaluation of the operation:"
       , prettyLayout op
       ]
  Stuck r ->
    [ "I could not continue evaluating, because I needed to know the value of" ]
    <> indentMany r
    <> [ "but it is an assumed term." ]

maximumStackSize :: Int
maximumStackSize = 200

-- The initial environment has to be built by pre-allocation.
initialEnvironment :: Machine Environment
initialEnvironment = do
  falseRef <- AllocateValue falseVal
  trueRef  <- AllocateValue trueVal
  nilRef   <- AllocateValue ValNil
  evalContractRef <- AllocateValue =<< evalContractVal
  eventCRef <- AllocateValue eventCVal
  fulfilRef <- AllocateValue fulfilVal
  isIntegerRef <- AllocateValue (ValUnaryBuiltinFun UnaryIsInteger)
  roundRef <- AllocateValue (ValUnaryBuiltinFun UnaryRound)
  ceilingRef <- AllocateValue (ValUnaryBuiltinFun UnaryCeiling)
  floorRef <- AllocateValue (ValUnaryBuiltinFun UnaryFloor)
  pure $
    Map.fromList
      [ (TypeCheck.falseUnique, falseRef)
      , (TypeCheck.trueUnique,  trueRef)
      , (TypeCheck.emptyUnique, nilRef)
      , (TypeCheck.evalContractUnique, evalContractRef)
      , (TypeCheck.eventCUnique, eventCRef)
      , (TypeCheck.fulfilUnique, fulfilRef)
      , (TypeCheck.isIntegerUnique, isIntegerRef)
      , (TypeCheck.roundUnique, roundRef)
      , (TypeCheck.ceilingUnique, ceilingRef)
      , (TypeCheck.floorUnique, floorRef)
      ]
