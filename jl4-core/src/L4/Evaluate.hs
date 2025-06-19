{-# LANGUAGE ViewPatterns #-}
module L4.Evaluate
  ( EvalDirectiveResult (..)
  , EvalState (..)
  , EvalTrace (..)
  , doEvalModule
  , buildModuleEnvironment
  , execEvalModuleWithEnv
  , unionEnvironments
  , prettyEvalException
  )
  where

import Base
import qualified Base.Text as Text
import Optics ((%))
import qualified Base.Map as Map
import L4.Annotation
import L4.Evaluate.Operators
import L4.Evaluate.Value
import L4.Evaluate.ValueLazy (UnaryBuiltinFun(..))
import qualified L4.EvaluateLazy.Machine as Lazy
import L4.Parser.SrcSpan (SrcRange)
import L4.Print
import L4.Syntax
import qualified L4.TypeCheck as TypeCheck
import L4.Utils.RevList
import L4.Utils.Ratio

import Data.Either

newtype Eval a = MkEval (EvalState -> EvalEnv -> (Either EvalException a, EvalState))
  deriving (Functor, Applicative, Monad, MonadError EvalException, MonadState EvalState, MonadReader EvalEnv)
    via ExceptT EvalException (StateT EvalState (Reader EvalEnv))

data EvalDirectiveResult =
  MkEvalDirectiveResult
    { range  :: !SrcRange -- ^ of the EVAL directive
    , result :: Either EvalException Value -- ^ of the EVAL directive
    , trace  :: EvalTrace
    }
  deriving stock Generic
  deriving anyclass NFData

newtype EvalEnv
  = MkEvalEnv
  { moduleUri :: NormalizedUri }

data EvalState =
  MkEvalState
    { environment      :: !Environment
    , directiveResults :: [EvalDirectiveResult]
    , supply           :: !Int
    , evalActions      :: !(RevList EvalAction)
    }
  deriving stock Generic
  deriving anyclass NFData

data EvalAction =
    Enter (Expr Resolved)
  | Exit Value
  | Exception EvalException
  deriving stock (Show, Generic)
  deriving anyclass NFData

pushFrame :: Expr Resolved -> Eval ()
pushFrame e = do
  modifying' #evalActions (pushRevList enter)
  where
    enter = Enter e

popFrame :: Value -> Eval ()
popFrame val =
  modifying' #evalActions (pushRevList pop)
  where
    pop = Exit val

unwindStack :: EvalException -> Eval ()
unwindStack e =
  modifying' #evalActions (pushRevList exc)
  where
    exc = Exception e

data EvalTrace =
  Trace (Expr Resolved) [EvalTrace] (Either EvalException Value)
  deriving stock (Show, Generic)
  deriving anyclass NFData

data EvalException =
    RuntimeScopeError Resolved -- internal
  | RuntimeTypeError Text -- internal
  | InvariantViolated Text -- internal
  | EqualityOnUnsupportedType
  | NonExhaustivePatterns Value -- we could try to warn statically
  | StackOverflow
  | DivisionByZero
  | NotAnInteger Rational
  | Stuck
    (Expr Resolved)
    -- ^ the expression we were evaluating when getting stuck
    Resolved
    -- ^ the term we got stuck on
  deriving stock (Generic, Show)
  deriving anyclass NFData

prettyEvalException :: EvalException -> [Text]
prettyEvalException = \ case
  RuntimeScopeError r ->
    [ "Internal error:" ]
    <> prepare r
    <> [ "is not in scope."
    , "Please report this as a bug." ]
  RuntimeTypeError err ->
    [ "Internal error:"
    , ind err
    , "is not in scope."
    , "Please report this as a bug." ]
  InvariantViolated inv ->
    [ "Invariant violated:"
    , ind inv
    , "Please report this as a bug." ]
  EqualityOnUnsupportedType -> ["Trying to check equality on types that do not support it."]
  NonExhaustivePatterns val ->
    [ "Value" ]
    <> prepare val
    <> [ "has no corresponding pattern." ]
  StackOverflow ->
    [ "Stack overflow: "
    , "Recursion depth of " <> Text.show maximumStackSize
    , "exceeded." ]
  DivisionByZero ->
    [ "Division by zero"
    ]
  NotAnInteger num ->
    [ "Expected an Integer but got the fractional number: " ]
    <> [ prettyRatio num ]
  Stuck expr r ->
    [ "Expression stuck while evaluating:" ]
    <> prepare expr
    <> [ "This happened because" ]
    <> prepare r
    <> [ "is assumed" ]
  where
    ind = ("  " <>)

    prepare :: LayoutPrinter a => a -> [Text]
    prepare = map ind . Text.lines .  prettyLayout


emptyEnvironment :: Environment
emptyEnvironment = Map.empty

-- We reproduce some of the name handling functions from the type checker here, only
-- that we produce separate uniques.

step :: Eval Int
step = do
  current <- use #supply
  let next = current + 1
  assign #supply next
  pure current

newUnique :: Eval Unique
newUnique = do
  i <- step
  u <- asks (.moduleUri)
  pure (MkUnique 'e' i u)

def :: Name -> Eval Resolved
def n = do
  u <- newUnique
  pure (Def u n)

ref :: Name -> Resolved -> Eval Resolved
ref n a =
  let
    (u, o) = getUniqueName a
  in
    pure (Ref n u o)

scope :: Eval a -> Eval a
scope m = do
  savedEnv <- use #environment
  a <- m
  assign #environment savedEnv
  pure a

withEnvironment :: (Environment -> Eval a) -> Eval a
withEnvironment f = do
  env <- use #environment
  f env

usingEnvironment :: Environment -> Eval () -> Eval Environment
usingEnvironment env m = scope $ do
  assign #environment env
  m
  use #environment

makeKnown :: Resolved -> Value -> Eval ()
makeKnown r val =
  modifying #environment (Map.insert (getUnique r) val)

data Stack =
    BinOp1 BinOp {- -} (Expr Resolved) Environment Stack
  | BinOp2 BinOp Value {- -} Stack
  -- | Proj1 {- -} Resolved Stack
  | App1 Resolved [Value] {- -} [Expr Resolved] Environment Stack -- values in reverse order
  | IfThenElse1 {- -} (Expr Resolved) (Expr Resolved) Environment Stack
  | Consider1 {- -} [Branch Resolved] Environment Stack
  | List1 [Value] {- -} [Expr Resolved] Environment Stack -- values in reverse order
  | Empty
  deriving stock Show

falseExpr :: Expr Resolved
falseExpr = App emptyAnno TypeCheck.falseRef []

falseVal :: Value
falseVal = ValConstructor TypeCheck.falseRef []

trueExpr :: Expr Resolved
trueExpr = App emptyAnno TypeCheck.trueRef []

trueVal :: Value
trueVal = ValConstructor TypeCheck.trueRef []

initialEnvironment :: Environment
initialEnvironment =
  let
    binOps =
      [ (unique, ValBinaryBuiltinFun binOp)
      | (binOp, unique) <- Lazy.builtinBinOps
      ]
  in
  Map.fromList $
    [ (TypeCheck.falseUnique, falseVal)
    , (TypeCheck.trueUnique,  trueVal)
    , (TypeCheck.emptyUnique, ValList [])
    , (TypeCheck.isIntegerUnique, ValUnaryBuiltinFun UnaryIsInteger)
    , (TypeCheck.roundUnique, ValUnaryBuiltinFun UnaryRound)
    , (TypeCheck.ceilingUnique, ValUnaryBuiltinFun UnaryCeiling)
    , (TypeCheck.floorUnique, ValUnaryBuiltinFun UnaryFloor)
    , (TypeCheck.andUnique,     andClosureVal trueVal falseVal)
    , (TypeCheck.orUnique,      orClosureVal trueVal falseVal)
    , (TypeCheck.impliesUnique, impliesClosureVal trueVal falseVal)
    , (TypeCheck.notUnique,     notClosureVal trueVal falseVal)
    ]
    <> binOps

evalModule :: Module Resolved -> Eval ()
evalModule (MkModule _ann _uri sec) =
  evalSection sec

evalSection :: Section Resolved -> Eval ()
evalSection (MkSection _ann _mn _maka topdecls) =
  traverse_ evalTopDecl topdecls

evalTopDecl :: TopDecl Resolved -> Eval ()
evalTopDecl (Declare _ann declare) =
  evalDeclare declare
evalTopDecl (Decide _ann decide) =
  evalDecide decide
evalTopDecl (Assume _ann assume) =
  evalAssume assume
evalTopDecl (Directive _ann directive) =
  evalDirective directive
evalTopDecl (Import _ann _import_) =
  pure ()
evalTopDecl (Section _ann sec) =
  evalSection sec

evalLocalDecl :: LocalDecl Resolved -> Eval ()
evalLocalDecl (LocalDecide _ann decide) =
  evalDecide decide
evalLocalDecl (LocalAssume _ann assume) =
  evalAssume assume

evalDeclare :: Declare Resolved -> Eval ()
evalDeclare (MkDeclare _ann _tysig _appFormAka t) =
  evalTypeDecl t

evalTypeDecl :: TypeDecl Resolved -> Eval ()
evalTypeDecl (EnumDecl _ann conDecls) =
  traverse_ evalConDecl conDecls
evalTypeDecl (RecordDecl _ann mcon tns) =
  traverse_ (\ c -> evalConDecl (MkConDecl emptyAnno c tns)) mcon
evalTypeDecl (SynonymDecl _ann _t) =
  pure ()

evalConDecl :: ConDecl Resolved -> Eval ()
evalConDecl (MkConDecl _ann n []) =
  makeKnown n (ValConstructor n [])
evalConDecl (MkConDecl _ann n tns) = do
  -- constructor
  makeKnown n (ValUnappliedConstructor n)
  conRef <- ref (TypeCheck.getName n) n
  -- selectors (we need to create fresh names for the lambda abstractions so that every binder is unique)
  traverse_ (\ (i, MkTypedName _ sn _t) -> do
    arg    <- def (TypeCheck.getName n)
    argRef <- ref (TypeCheck.getName n) arg
    args <- traverse def (TypeCheck.getName <$> tns)
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
    makeKnown sn sel
    ) (zip [0 ..] tns)

-- NOTE: We currently disallow recursive declarations that look like values.
-- We could allow these by being more sophisticated.
evalDecide :: Decide Resolved -> Eval ()
evalDecide (MkDecide _ann _tysig (MkAppForm _ n [] _maka) expr) = do
  mr <- evalExpr expr
  case mr of
    Left _ -> pure () -- do something better?
    Right v -> makeKnown n v
evalDecide (MkDecide _ann _tysig (MkAppForm _ n args _maka) expr) =
  withEnvironment $ \ env -> do
    let
      v = ValClosure (MkGivenSig emptyAnno ((\ r -> MkOptionallyTypedName emptyAnno r Nothing) <$> args)) expr env'
      env' = Map.insert (getUnique n) v env
    makeKnown n v

evalAssume :: Assume Resolved -> Eval ()
evalAssume (MkAssume _ann _tysig (MkAppForm _ n [] _maka) _) =
  makeKnown n (ValAssumed n)
evalAssume (MkAssume _ann _tysig (MkAppForm _ n _args _maka) _) =
  makeKnown n (ValAssumed n) -- TODO: we should create a given here yielding an assumed, but we currently cannot do that easily

evalExpr :: Expr Resolved -> Eval (Either EvalException Value)
evalExpr expr =
  tryError (withEnvironment $ \ env ->
    pushExprFrame env 0 Empty expr
    )

evalDirective :: Directive Resolved -> Eval ()
evalDirective (LazyEval _ann _expr) = pure ()
evalDirective (LazyEvalTrace _ann _expr) = pure ()
evalDirective (Check _ _) = pure ()
evalDirective (Contract {}) = pure ()

maximumStackSize :: Int
maximumStackSize = 50

-- | Push a stack frame for the given expression.
-- Makes sure, it is shown in the stack trace in its entirety.
--
-- Does not increase the current stack size.
pushExprFrame :: Environment -> Int -> Stack -> Expr Resolved -> Eval Value
pushExprFrame env ss stack e = do
  pushFrame e
  v <- forwardExpr env ss stack e
  popFrame v
  pure v

-- | Push the evaluation of expression to the stack.
-- Increases the stack size
pushEvalFrame :: Environment -> Int -> Stack -> Expr Resolved -> Eval Value
pushEvalFrame env ss stack e = do
  pushFrame e
  pushExprFrame env (ss + 1) stack e

-- | Pop the given value from the stack evaluation and start evaluation
-- of the next 'Expr Resolved'.
--
-- Does not modify the current stack size.
popAndPushFrame :: Value -> Environment -> Int -> Stack -> Expr Resolved -> Eval Value
popAndPushFrame val env ss stack e = do
  popFrame val
  pushFrame e
  forwardExpr env ss stack e

forwardExpr :: Environment -> Int -> Stack -> Expr Resolved -> Eval Value
forwardExpr _env ss stack _e
  | ss > maximumStackSize =
    exception StackOverflow stack
forwardExpr env !ss stack (And _ann e1 e2) = do
  pushExprFrame env ss stack (IfThenElse emptyAnno e1 e2 falseExpr)
forwardExpr env !ss stack (Or _ann e1 e2) = do
  pushExprFrame env ss stack (IfThenElse emptyAnno e1 trueExpr e2)
forwardExpr _env !_ss stack (RAnd {}) = exception (RuntimeTypeError "strict evaluation of contracts is currently not supported") stack
forwardExpr _env !_ss stack (ROr {}) = exception (RuntimeTypeError "strict evaluation of contracts is currently not supported") stack
forwardExpr env !ss stack (Implies _ann e1 e2) = do
  pushExprFrame env ss stack (IfThenElse emptyAnno e1 e2 trueExpr)
forwardExpr env !ss stack (Not _ann e) = do
  pushExprFrame env ss stack (IfThenElse emptyAnno e falseExpr trueExpr)
forwardExpr env !ss stack (Equals _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpEquals e2 env stack) e1
forwardExpr env !ss stack (Plus _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpPlus e2 env stack) e1
forwardExpr env !ss stack (Minus _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpMinus e2 env stack) e1
forwardExpr env !ss stack (Times _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpTimes e2 env stack) e1
forwardExpr env !ss stack (DividedBy _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpDividedBy e2 env stack) e1
forwardExpr env !ss stack (Modulo _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpModulo e2 env stack) e1
forwardExpr env !ss stack (Cons _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpCons e2 env stack) e1
forwardExpr env !ss stack (Leq _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpLeq e2 env stack) e1
forwardExpr env !ss stack (Geq _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpGeq e2 env stack) e1
forwardExpr env !ss stack (Lt _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpLt e2 env stack) e1
forwardExpr env !ss stack (Gt _ann e1 e2) = do
  pushEvalFrame env ss (BinOp1 BinOpGt e2 env stack) e1
forwardExpr env !ss stack (Proj _ann e l) =
  forwardExpr env ss stack (App emptyAnno l [e]) -- we desugar projection to plain function application
forwardExpr env !ss stack (Var _ann n) = do
  case lookupTerm env n of
    Nothing -> exception (RuntimeScopeError n) stack
    Just val -> do
      backwardExpr ss stack val
forwardExpr env !ss stack (Lam _ann givens e) =
  backwardExpr ss stack (ValClosure givens e env)
forwardExpr env !ss stack (App _ann n []) =
  case lookupTerm env n of
    Nothing -> exception (RuntimeScopeError n) stack
    Just val -> backwardExpr ss stack val
forwardExpr env !ss stack (App _ann n (e : es)) = do
  pushEvalFrame env ss (App1 n [] es env stack) e
forwardExpr env !ss stack (AppNamed ann n [] _) =
  forwardExpr env ss stack (App ann n [])
forwardExpr _env !_ss stack (AppNamed _ann _n _nes Nothing) =
  exception (RuntimeTypeError "named application where the order of arguments is not resolved") stack
forwardExpr env !ss stack (AppNamed ann n nes (Just order)) =
  let
    -- move expressions into order, drop names
    es = (\ (MkNamedExpr _ _ e) -> e) . snd <$> sortOn fst (zip order nes)
  in
    pushExprFrame env ss stack (App ann n es)
forwardExpr env !ss stack (IfThenElse _ann e1 e2 e3) = do
  pushEvalFrame env ss (IfThenElse1 e2 e3 env stack) e1
forwardExpr _env !_ss stack MultiWayIf {} = exception (RuntimeTypeError "strict evaluation of multiway if is not supported") stack
forwardExpr _env !_ss stack Regulative {} = exception (RuntimeTypeError "strict evaluation of contracts is currently not supported") stack
forwardExpr _env !_ss stack Event {} = exception (RuntimeTypeError "strict evaluation of events is currently not supported") stack
forwardExpr env !ss stack (Consider _ann e branches) = do
  pushEvalFrame env ss (Consider1 branches env stack) e
forwardExpr _env !ss stack (Lit _ann lit) = do
  rval <- runLit lit
  backwardExpr ss stack rval
forwardExpr env !ss stack (Percent _ann expr) = do
  pushExprFrame env ss stack (DividedBy _ann expr (Lit emptyAnno (NumericLit emptyAnno 100)))
forwardExpr _env !ss stack (List _ann []) = do
  backwardExpr ss stack (ValList [])
forwardExpr env !ss stack (List _ann (e : es)) = do
  pushEvalFrame env ss (List1 [] es env stack) e
forwardExpr env !ss stack (Where _ann e ds) = do
  -- TODO: I don't like the weird mix between abstract machine style
  -- and direct style. We should extend the abstract machine style to
  -- cover declarations.
  env' <- usingEnvironment env (traverse_ evalLocalDecl ds)
  pushExprFrame env' ss stack e

backwardExpr :: Int -> Stack -> Value -> Eval Value
backwardExpr !ss (BinOp1 binOp e2 env stack) val1 = do
  popAndPushFrame val1 env ss (BinOp2 binOp val1 stack) e2
backwardExpr !ss (BinOp2 binOp val1 stack) val2 = do
  popFrame val2
  rval <- runBinOp binOp val1 val2 stack
  backwardExpr (ss - 1) stack rval
-- backwardExpr stack0@(Proj1 l stack) val =
--   case val of
--     ValConstructor n args -> _
--     _ -> exception RuntimeTypeError stack0
backwardExpr !ss stack0@(App1 n vals [] env stack) val = do
  case lookupTerm env n of
    Just (ValClosure givens e env') -> do
      popFrame val
      env'' <- matchGivens givens (reverse (val : vals)) stack0
      pushExprFrame (Map.union env'' env') (ss - 1) stack e
    Just (ValUnaryBuiltinFun builtin) -> do
      popFrame val
      runUnaryBuiltin stack0 (reverse (val : vals)) builtin
    Just (ValBinaryBuiltinFun builtin) -> do
      popFrame val
      runBinaryBuiltin stack0 (reverse (val : vals)) builtin
    Just (ValUnappliedConstructor r) -> do
      popFrame val
      backwardExpr (ss - 1) stack (ValConstructor r (reverse (val : vals)))
    Just (ValAssumed r) -> stuckOnAssumed r stack0
    res -> exception (RuntimeTypeError $ "unexpected value when looking up term: " <> Text.show res) stack0
backwardExpr !ss (App1 n vals (e : es) env stack) val = do
  popAndPushFrame val env ss (App1 n (val : vals) es env stack) e
backwardExpr !ss stack0@(IfThenElse1 e2 e3 env stack) val1 = do
  popFrame val1
  case boolView val1 of
    Just True  -> do
      pushExprFrame env (ss - 1) stack e2
    Just False -> do
      pushExprFrame env (ss - 1) stack e3
    Nothing | ValAssumed r <- val1 -> stuckOnAssumed r stack0
    Nothing -> exception (RuntimeTypeError "expected a bool frame when checking if then else") stack0
backwardExpr !ss stack0@(Consider1 branches env stack) val = do
  popFrame val
  matchBranches val branches env stack0 ss stack
backwardExpr !ss (List1 vals [] _env stack) val = do
  popFrame val
  backwardExpr (ss - 1) stack (ValList (reverse (val : vals)))
backwardExpr !ss (List1 vals (e : es) env stack) val = do
  popFrame val
  pushExprFrame env ss (List1 (val : vals) es env stack) e
backwardExpr _ss Empty val =
  pure val

-- | Checks if a value is a Boolean constructor.
boolView :: Value -> Maybe Bool
boolView val =
  case val of
    ValConstructor n []
      | sameResolved n TypeCheck.trueRef  -> Just True
      | sameResolved n TypeCheck.falseRef -> Just False
    _ -> Nothing

matchGivens :: GivenSig Resolved -> [Value] -> Stack -> Eval Environment
matchGivens (MkGivenSig _ann otns) vals stack0 = do
  let
    (_tyvars, others) = partitionEithers (TypeCheck.isQuantifier <$> otns)
  if length others == length vals
    then pure $ Map.fromList (zipWith (\ (r, _) v -> (getUnique r, v)) others vals)
    else exception (RuntimeTypeError "given signatures' values' lengths do not match") stack0

matchBranches :: Value -> [Branch Resolved] -> Environment -> Stack -> Int -> Stack -> Eval Value
matchBranches val [] _env stack0 _ss _stack =
  exception (NonExhaustivePatterns val) stack0
matchBranches _val (Otherwise _ann e : _) env _stack0 !ss stack = do
  pushExprFrame env ss stack e
matchBranches val (When _ann pat e : branches) env stack0 !ss stack = do
  menv' <- matchPattern stack0 val pat
  case menv' of
    Nothing   -> matchBranches val branches env stack0 ss stack
    Just env' -> do
      pushExprFrame (Map.union env' env) ss stack e

matchPattern :: Stack -> Value -> Pattern Resolved -> Eval (Maybe Environment)
matchPattern _stack0 val (PatVar _ann n) =
  pure (Just (Map.singleton (getUnique n) val))
matchPattern _stack0 (ValList vs) (PatApp _ann n [])
  | getUnique n == TypeCheck.emptyUnique =
    case vs of
      []      -> pure (Just Map.empty)
      (_ : _) -> pure Nothing
matchPattern stack0 (ValList vs) (PatCons _ann p1 p2) =
  case vs of
    []       -> pure Nothing
    (v1 : v2) -> (fmap Map.unions . sequence) <$> sequence (zipWith (matchPattern stack0) [v1, ValList v2] [p1, p2])
matchPattern stack0 (ValConstructor n vals) (PatApp _ann n' pats)
  | sameResolved n n' =
    if length vals == length pats
      then (fmap Map.unions . sequence) <$> sequence (zipWith (matchPattern stack0) vals pats)
      else exception (RuntimeTypeError "matching patterns with different amounts of patterns and values") stack0
  | otherwise         = pure Nothing
matchPattern stack0 (ValAssumed r) _ = stuckOnAssumed r stack0
matchPattern stack0 _ _ =
  exception (RuntimeTypeError "matching patterns with malformed value constructor and pattern application") stack0

sameResolved :: Resolved -> Resolved -> Bool
sameResolved r1 r2 =
  getUnique r1 == getUnique r2

-- | For the time being, exceptions are always fatal. But we could
-- in principle have exception we can recover from ...
exception :: EvalException -> Stack -> Eval a
exception exc _stack = do
  unwindStack exc
  throwError exc

runBinOp :: BinOp -> Value -> Value -> Stack -> Eval Value
runBinOp BinOpPlus      (ValNumber num1) (ValNumber num2) _stack = pure $ ValNumber (num1 + num2)
runBinOp BinOpMinus     (ValNumber num1) (ValNumber num2) _stack = pure $ ValNumber (num1 - num2)
runBinOp BinOpTimes     (ValNumber num1) (ValNumber num2) _stack = pure $ ValNumber (num1 * num2)
runBinOp BinOpDividedBy (ValNumber num1) (ValNumber num2)  stack
  | num2 /= 0 = pure $ ValNumber (num1 / num2)
  | otherwise = exception DivisionByZero stack
runBinOp BinOpModulo    (ValNumber num1) (ValNumber num2) stack  = do
  n1 <- expectInteger stack num1
  n2 <- expectInteger stack num2
  if n2 /= 0
    then pure $ ValNumber (toRational $ n1 `mod` n2)
    else exception DivisionByZero stack
runBinOp BinOpCons   val1             (ValList val2)   _stack = pure $ ValList (val1 : val2)
runBinOp BinOpEquals val1             val2             stack  = runBinOpEquals val1 val2 stack
runBinOp BinOpLeq    (ValNumber num1) (ValNumber num2) _stack = pure $ valBool (num1 <= num2)
runBinOp BinOpLeq    (ValString str1) (ValString str2) _stack = pure $ valBool (str1 <= str2)
runBinOp BinOpLeq    (boolView -> Just b1) (boolView -> Just b2) _stack = pure $ valBool (b1 <= b2)
runBinOp BinOpGeq    (ValNumber num1) (ValNumber num2) _stack = pure $ valBool (num1 >= num2)
runBinOp BinOpGeq    (ValString str1) (ValString str2) _stack = pure $ valBool (str1 >= str2)
runBinOp BinOpGeq    (boolView -> Just b1) (boolView -> Just b2) _stack = pure $ valBool (b1 >= b2)
runBinOp BinOpLt     (ValNumber num1) (ValNumber num2) _stack = pure $ valBool (num1 < num2)
runBinOp BinOpLt     (ValString str1) (ValString str2) _stack = pure $ valBool (str1 < str2)
runBinOp BinOpLt     (boolView -> Just b1) (boolView -> Just b2) _stack = pure $ valBool (b1 < b2)
runBinOp BinOpGt     (ValNumber num1) (ValNumber num2) _stack = pure $ valBool (num1 > num2)
runBinOp BinOpGt     (ValString str1) (ValString str2) _stack = pure $ valBool (str1 > str2)
runBinOp BinOpGt     (boolView -> Just b1) (boolView -> Just b2) _stack = pure $ valBool (b1 > b2)
runBinOp _op         (ValAssumed r) _e2 stack = stuckOnAssumed r stack
runBinOp _op         _e1 (ValAssumed r) stack = stuckOnAssumed r stack
runBinOp _           _                _                 stack = exception (RuntimeTypeError "running bin op with invalid operation / value combination") stack

stuckOnAssumed :: Resolved -> Stack -> Eval b
stuckOnAssumed assumedResolved stack = do
  exprs <- use (#evalActions % to unRevList)
  case exprs of
    Enter expr : _ -> exception (Stuck expr assumedResolved) stack
    _ -> exception (InvariantViolated "no corresponding Enter stackframe for expression") stack

runBinOpEquals :: Value -> Value -> Stack -> Eval Value
runBinOpEquals val1 val2 stack =
  case computeEquals val1 val2 of
    Just b  -> pure $ valBool b
    Nothing -> exception EqualityOnUnsupportedType stack

computeEquals :: Value -> Value -> Maybe Bool
computeEquals (ValNumber num1) (ValNumber num2) = Just $ num1 == num2
computeEquals (ValString str1) (ValString str2) = Just $ str1 == str2
computeEquals (ValList vs1)    (ValList vs2)
  | length vs1 == length vs2                    = do
      bs <- zipWithM computeEquals vs1 vs2
      pure (and bs)
  | otherwise                                   = Just False
computeEquals (ValConstructor r1 vs1) (ValConstructor r2 vs2)
  | sameResolved r1 r2 && length vs1 == length vs2 = do
      bs <- zipWithM computeEquals vs1 vs2
      pure (and bs)
  | otherwise                                   = Just False
computeEquals _                _                = Nothing

valBool :: Bool -> Value
valBool False = falseVal
valBool True  = trueVal

valInt :: Integer -> Value
valInt = ValNumber . toRational

runLit :: Lit -> Eval Value
runLit (NumericLit _ann num) = pure (ValNumber num)
runLit (StringLit _ann str)  = pure (ValString str)

runUnaryBuiltin :: Stack -> [Value] -> UnaryBuiltinFun -> Eval Value
runUnaryBuiltin s vals op = do
  val :: Rational <- expect1Number s vals
  pure case op of
    UnaryIsInteger -> valBool $ isJust $ isInteger val
    UnaryRound -> valInt $ round val
    UnaryCeiling -> valInt $ ceiling val
    UnaryFloor -> valInt $ floor val
    UnaryPercent -> ValNumber $ val / 100

runBinaryBuiltin :: Stack -> [Value] -> BinOp -> Eval Value
runBinaryBuiltin s vals op = do
  (a, b) <- expect2 s vals
  runBinOp op a b s

expect1 :: Stack -> [a] -> Eval a
expect1 s = \ case
  [x] -> pure x
  xs ->
    exception (RuntimeTypeError $ "Expected 1 argument but got " <> Text.show (length xs)) s

expect2 :: Stack -> [a] -> Eval (a, a)
expect2 s = \ case
  [x, y] -> pure (x, y)
  xs ->
    exception (RuntimeTypeError $ "Expected 2 arguments but got " <> Text.show (length xs)) s

expectNumber :: Stack -> Value -> Eval Rational
expectNumber s = \ case
  ValNumber f -> pure f
  _ -> exception (RuntimeTypeError "Expected number.") s

expect1Number :: Stack -> [Value] -> Eval Rational
expect1Number s vs = do
  v <- expect1 s vs
  expectNumber s v

expectInteger :: Stack -> Rational -> Eval Integer
expectInteger stack n = do
  case isInteger n of
    Nothing -> exception (NotAnInteger n) stack
    Just i -> pure i

lookupTerm :: Environment -> Resolved -> Maybe Value
lookupTerm env r =
  Map.lookup (getUnique r) env

execEvalModuleWithEnv :: Environment -> Module Resolved -> EvalState
execEvalModuleWithEnv env m@(MkModule _ moduleUri _) =
  case evalModule m of
    MkEval f ->
      case f (MkEvalState env [] 0 emptyRevList) MkEvalEnv {moduleUri} of
        (_, s) -> s

execEvalModule :: Module Resolved -> EvalState
execEvalModule = execEvalModuleWithEnv initialEnvironment

doEvalModule :: Module Resolved -> [EvalDirectiveResult]
doEvalModule m = (execEvalModule m).directiveResults

buildModuleEnvironment :: Environment -> Module Resolved -> Environment
buildModuleEnvironment initial m = (execEvalModuleWithEnv initial m).environment

unionEnvironments :: Foldable f => f (Map Unique Value) -> Environment
unionEnvironments m = if null m then initialEnvironment else Map.unions m

boolBinOpClosure :: Value -> Value -> (Resolved -> Resolved -> Expr Resolved) -> Value
boolBinOpClosure true false buildExpr = do
  ValClosure
    (MkGivenSig emptyAnno
      [ MkOptionallyTypedName emptyAnno TypeCheck.aDef (Just TypeCheck.boolean)
      , MkOptionallyTypedName emptyAnno TypeCheck.bDef (Just TypeCheck.boolean)
      ])
    (buildExpr TypeCheck.aRef TypeCheck.bRef)
    ( Map.fromList
      [ (TypeCheck.trueUnique, true)
      , (TypeCheck.falseUnique, false)
      ]
    )

boolUnaryOpClosure :: Value -> Value -> (Resolved -> Expr Resolved) -> Value
boolUnaryOpClosure true false buildExpr = do
  ValClosure
    (MkGivenSig emptyAnno
      [ MkOptionallyTypedName emptyAnno TypeCheck.aDef (Just TypeCheck.boolean)
      ])
    (buildExpr TypeCheck.aRef)
    ( Map.fromList
      [ (TypeCheck.trueUnique, true)
      , (TypeCheck.falseUnique, false)
      ]
    )

andClosureVal :: Value -> Value -> Value
andClosureVal true false =
  boolBinOpClosure true false
    (\aRef bRef ->
      IfThenElse emptyAnno
        (Var emptyAnno aRef)
        (Var emptyAnno bRef)
        falseExpr
    )

notClosureVal :: Value -> Value -> Value
notClosureVal true false =
  boolUnaryOpClosure true false
    (\aRef ->
      IfThenElse emptyAnno
        (Var emptyAnno aRef)
        falseExpr
        trueExpr
    )

orClosureVal :: Value -> Value -> Value
orClosureVal true false =
  boolBinOpClosure true false
    (\aRef bRef ->
      IfThenElse emptyAnno
        (Var emptyAnno aRef)
        trueExpr
        (Var emptyAnno bRef)
    )

impliesClosureVal :: Value -> Value -> Value
impliesClosureVal true false =
  boolBinOpClosure true false
    (\aRef bRef ->
      IfThenElse emptyAnno
        (Var emptyAnno aRef)
        (Var emptyAnno bRef)
        trueExpr
    )
