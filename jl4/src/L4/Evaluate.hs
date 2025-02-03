module L4.Evaluate where

import Base
import qualified Base.Map as Map
import qualified Base.Text as Text
import L4.ExactPrint (rangeOfNode, ToConcreteNodes)
import L4.Lexer (SrcRange, PosToken)
import L4.Syntax
import qualified L4.TypeCheck as TypeCheck

import Control.DeepSeq
import Data.Either

newtype Eval a = MkEval (EvalState -> (Either EvalException a, EvalState))
  deriving (Functor, Applicative, Monad, MonadError EvalException, MonadState EvalState)
    via ExceptT EvalException (StateT EvalState Identity)

type EvalResult = (SrcRange, Either EvalException Value)

data EvalState =
  MkEvalState
    { environment :: !Environment
    , results     :: [EvalResult]
    -- , supply      :: !Int
    }
  deriving Generic

data EvalException =
    RuntimeScopeError Resolved -- internal
  | RuntimeTypeError -- internal
  | NonExhaustivePatterns -- we could try to warn statically
  | StackOverflow
  | Unimplemented
  deriving stock (Generic, Show)
  deriving anyclass NFData

type Environment = Map Unique Value

withEnvironment :: (Environment -> Eval a) -> Eval a
withEnvironment f = do
  env <- use #environment
  f env

makeKnown :: Resolved -> Value -> Eval ()
makeKnown r val =
  modifying #environment (Map.insert (getUnique r) val)

addEvalResult :: ToConcreteNodes PosToken a => a -> Either EvalException Value -> Eval ()
addEvalResult a val = 
  let
    res = (, val) <$> rangeOfNode a
  in
    maybe (pure ()) (modifying #results . (:)) res

data Value =
    ValNumber Int -- for now
  | ValString Text
  | ValList [Value]
  | ValClosure (GivenSig Resolved) (Expr Resolved) Environment
  | ValUnappliedConstructor Resolved
  | ValConstructor Resolved [Value]
  | ValAssumed Resolved

-- | This is a non-standard instance because environments can be recursive, hence we must
-- not actually force the environments ...
--
instance NFData Value where
  rnf :: Value -> ()
  rnf (ValNumber i)               = rnf i
  rnf (ValString t)               = rnf t
  rnf (ValList vs)                = rnf vs
  rnf (ValClosure given expr env) = env `seq` rnf given `seq` rnf expr
  rnf (ValUnappliedConstructor r) = rnf r
  rnf (ValConstructor r vs)       = rnf r `seq` rnf vs
  rnf (ValAssumed r)              = rnf r

renderValue :: Value -> Text
renderValue (ValNumber i) = Text.pack (show i)
renderValue (ValString txt) = Text.pack (show txt)
renderValue (ValList vs) = Text.intercalate ", " (renderValue <$> vs)
renderValue (ValClosure _ _ _) = "<function>"
renderValue (ValUnappliedConstructor _) = "<unapplied constructor>"
renderValue (ValConstructor r []) = TypeCheck.simpleprint r
renderValue (ValConstructor r vs) = "(" <> TypeCheck.simpleprint r <> " OF " <> Text.intercalate ", " (renderValue <$> vs) <> ")"
renderValue (ValAssumed _) = "<assumed>"

data BinOp =
    BinOpPlus
  | BinOpMinus
  | BinOpTimes
  | BinOpDividedBy
  | BinOpModulo
  | BinOpCons
  | BinOpEquals
  | BinOpLeq
  | BinOpGeq

data Stack =
    BinOp1 BinOp {- -} (Expr Resolved) Environment Stack
  | BinOp2 BinOp Value {- -} Stack
  -- | Proj1 {- -} Resolved Stack
  | App1 Resolved [Value] {- -} [Expr Resolved] Environment Stack -- values in reverse order
  | IfThenElse1 {- -} (Expr Resolved) (Expr Resolved) Environment Stack
  | Consider1 {- -} [Branch Resolved] Environment Stack
  | List1 [Value] {- -} [Expr Resolved] Environment Stack -- values in reverse order
  | Empty

falseExpr :: Expr Resolved
falseExpr = App mempty TypeCheck.falseRef []

falseVal :: Value
falseVal = ValConstructor TypeCheck.falseRef []

trueExpr :: Expr Resolved
trueExpr = App mempty TypeCheck.trueRef []

trueVal :: Value
trueVal = ValConstructor TypeCheck.trueRef []

initialEnvironment :: Environment
initialEnvironment =
  Map.fromList
    [ (TypeCheck.falseUnique, falseVal)
    , (TypeCheck.trueUnique,  trueVal)
    , (TypeCheck.emptyUnique, ValList [])
    ]

evalProgram :: Program Resolved -> Eval ()
evalProgram (MkProgram _ann sections) =
  traverse_ evalSection sections

evalSection :: Section Resolved -> Eval ()
evalSection (MkSection _ann _lvl _mn topdecls) =
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

evalDeclare :: Declare Resolved -> Eval ()
evalDeclare (MkDeclare _ann _tysig appForm t) =
  evalTypeDecl (TypeCheck.appFormHead appForm) t

evalTypeDecl :: Resolved -> TypeDecl Resolved -> Eval ()
evalTypeDecl _ (EnumDecl _ann conDecls) =
  traverse_ evalConDecl conDecls
evalTypeDecl c (RecordDecl _ann tns) =
  evalConDecl (MkConDecl mempty c tns)

evalConDecl :: ConDecl Resolved -> Eval ()
evalConDecl (MkConDecl _ann n []) =
  makeKnown n (ValConstructor n [])
evalConDecl (MkConDecl _ann n _tns) =
  makeKnown n (ValUnappliedConstructor n)
  -- TODO: selector functions

-- NOTE: We currently disallow recursive declarations that look like values.
-- We could allow these by being more sophisticated.
evalDecide :: Decide Resolved -> Eval ()
evalDecide (MkDecide _ann _tysig (MkAppForm _ n []) expr) = do
  mr <- evalExpr expr
  case mr of
    Left _ -> pure () -- do something better?
    Right v -> makeKnown n v
evalDecide (MkDecide _ann _tysig (MkAppForm _ n args) expr) =
  withEnvironment $ \ env -> do
    let
      v = ValClosure (MkGivenSig mempty ((\ r -> MkOptionallyTypedName mempty r Nothing) <$> args)) expr env'
      env' = Map.insert (getUnique n) v env
    makeKnown n v

evalAssume :: Assume Resolved -> Eval ()
evalAssume (MkAssume _ann _tysig (MkAppForm _ n []) _) =
  makeKnown n (ValAssumed n)
evalAssume (MkAssume _ann _tysig (MkAppForm _ n _args) _) =
  makeKnown n (ValAssumed n) -- TODO: we should create a given here yielding an assumed, but we currently cannot do that easily

evalExpr :: Expr Resolved -> Eval (Either EvalException Value)
evalExpr expr =
  tryError (withEnvironment $ \ env -> forwardExpr env 0 Empty expr)

evalDirective :: Directive Resolved -> Eval ()
evalDirective (Eval _ann expr) = do
  v <- evalExpr expr
  addEvalResult expr v
evalDirective (Check _ _) = pure ()

forwardExpr :: Environment -> Int -> Stack -> Expr Resolved -> Eval Value
forwardExpr _env ss stack _e
  | ss >= 50 =
    exception StackOverflow stack
forwardExpr env !ss stack (And _ann e1 e2) =
  forwardExpr env ss stack (IfThenElse mempty e1 e2 falseExpr)
forwardExpr env !ss stack (Or _ann e1 e2) =
  forwardExpr env ss stack (IfThenElse mempty e1 trueExpr e2)
forwardExpr env !ss stack (Implies _ann e1 e2) =
  forwardExpr env ss stack (IfThenElse mempty e1 e2 trueExpr)
forwardExpr env !ss stack (Equals _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpEquals e2 env stack) e1
forwardExpr env !ss stack (Not _ann e) =
  forwardExpr env ss stack (IfThenElse mempty e falseExpr trueExpr)
forwardExpr env !ss stack (Plus _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpPlus e2 env stack) e1
forwardExpr env !ss stack (Minus _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpMinus e2 env stack) e1
forwardExpr env !ss stack (Times _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpTimes e2 env stack) e1
forwardExpr env !ss stack (DividedBy _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpDividedBy e2 env stack) e1
forwardExpr env !ss stack (Modulo _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpModulo e2 env stack) e1
forwardExpr env !ss stack (Cons _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpCons e2 env stack) e1
forwardExpr env !ss stack (Leq _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpLeq e2 env stack) e1
forwardExpr env !ss stack (Geq _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpGeq e2 env stack) e1
forwardExpr env !ss stack (Proj _ann e l) =
  forwardExpr env ss stack (App mempty l [e]) -- we desugar projection to plain function application
forwardExpr env !ss stack (Var _ann n) =
  case lookupTerm env n of
    Nothing -> exception (RuntimeScopeError n) stack
    Just val -> backwardExpr ss stack val
forwardExpr env !ss stack (Lam _ann givens e) =
  backwardExpr ss stack (ValClosure givens e env)
forwardExpr env !ss stack (App _ann n []) =
  case lookupTerm env n of
    Nothing -> exception (RuntimeScopeError n) stack
    Just val -> backwardExpr ss stack val
forwardExpr env !ss stack (App _ann n (e : es)) =
  forwardExpr env (ss + 1) (App1 n [] es env stack) e
forwardExpr _env _ss stack (AppNamed _ann _n _nes) =
  exception Unimplemented stack
forwardExpr env !ss stack (IfThenElse _ann e1 e2 e3) =
  forwardExpr env (ss + 1) (IfThenElse1 e2 e3 env stack) e1
forwardExpr env !ss stack (Consider _ann e branches) =
  forwardExpr env (ss + 1) (Consider1 branches env stack) e
forwardExpr _env !ss stack (Lit _ann lit) = do
  rval <- runLit lit
  backwardExpr ss stack rval
forwardExpr _env !ss stack (List _ann []) =
  backwardExpr ss stack (ValList [])
forwardExpr env !ss stack (List _ann (e : es)) =
  forwardExpr env (ss + 1) (List1 [] es env stack) e

backwardExpr :: Int -> Stack -> Value -> Eval Value
backwardExpr !ss (BinOp1 binOp e2 env stack) val1 =
  forwardExpr env ss (BinOp2 binOp val1 stack) e2
backwardExpr !ss (BinOp2 binOp val1 stack) val2 = do
  rval <- runBinOp binOp val1 val2 stack
  backwardExpr (ss - 1) stack rval
-- backwardExpr stack0@(Proj1 l stack) val =
--   case val of
--     ValConstructor n args -> _
--     _ -> exception RuntimeTypeError stack0
backwardExpr !ss stack0@(App1 n vals [] env stack) val = do
  case lookupTerm env n of
    Just (ValClosure givens e env') -> do
      env'' <- matchGivens givens (reverse (val : vals)) stack0
      forwardExpr (Map.union env'' env') (ss - 1) stack e
    Just (ValUnappliedConstructor r) -> do
      backwardExpr (ss - 1) stack (ValConstructor r (reverse (val : vals)))
    _                       -> exception RuntimeTypeError stack0
backwardExpr !ss (App1 n vals (e : es) env stack) val =
  forwardExpr env ss (App1 n (val : vals) es env stack) e
backwardExpr !ss stack0@(IfThenElse1 e2 e3 env stack) val1 =
  case val1 of
    ValConstructor n []
      | sameResolved n TypeCheck.trueRef  -> forwardExpr env (ss - 1) stack e2
      | sameResolved n TypeCheck.falseRef -> forwardExpr env (ss - 1) stack e3
    _                                     -> exception RuntimeTypeError stack0
backwardExpr !ss stack0@(Consider1 branches env stack) val =
    matchBranches val branches env stack0 ss stack
backwardExpr !ss (List1 vals [] _env stack) val =
  backwardExpr (ss - 1) stack (ValList (reverse (val : vals)))
backwardExpr !ss (List1 vals (e : es) env stack) val =
  forwardExpr env ss (List1 (val : vals) es env stack) e
backwardExpr _ss Empty val =
  pure val

matchGivens :: GivenSig Resolved -> [Value] -> Stack -> Eval Environment
matchGivens (MkGivenSig _ann otns) vals stack0 = do
  let
    (_tyvars, others) = partitionEithers (TypeCheck.isQuantifier <$> otns)
  if length others == length vals
    then pure $ Map.fromList (zipWith (\ (r, _) v -> (getUnique r, v)) others vals)
    else exception RuntimeTypeError stack0

matchBranches :: Value -> [Branch Resolved] -> Environment -> Stack -> Int -> Stack -> Eval Value
matchBranches _val [] _env stack0 _ss _stack =
  exception NonExhaustivePatterns stack0
matchBranches _val (Otherwise _ann e : _) env _stack0 !ss stack =
  forwardExpr env ss stack e
matchBranches val (When _ann pat e : branches) env stack0 !ss stack = do
  menv' <- matchPattern stack0 val pat
  case menv' of
    Nothing   -> matchBranches val branches env stack0 ss stack
    Just env' -> forwardExpr (Map.union env' env) ss stack e

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
      else exception RuntimeTypeError stack0
  | otherwise         = pure Nothing
matchPattern stack0 _ _ =
  exception RuntimeTypeError stack0

sameResolved :: Resolved -> Resolved -> Bool
sameResolved r1 r2 =
  getUnique r1 == getUnique r2

-- | For the time being, exceptions are always fatal. But we could
-- in principle have exception we can recover from ...
exception :: EvalException -> Stack -> Eval a
exception exc _stack = throwError exc

runBinOp :: BinOp -> Value -> Value -> Stack -> Eval Value
runBinOp BinOpPlus   (ValNumber num1) (ValNumber num2) _stack = pure $ ValNumber (num1 + num2)
runBinOp BinOpMinus  (ValNumber num1) (ValNumber num2) _stack = pure $ ValNumber (num1 - num2)
runBinOp BinOpTimes  (ValNumber num1) (ValNumber num2) _stack = pure $ ValNumber (num1 * num2)
runBinOp BinOpDividedBy (ValNumber num1) (ValNumber num2) _stack = pure $ ValNumber (num1 `div` num2)
runBinOp BinOpModulo    (ValNumber num1) (ValNumber num2) _stack = pure $ ValNumber (num1 `mod` num2)
runBinOp BinOpCons   val1             (ValList val2)   _stack = pure $ ValList (val1 : val2)
runBinOp BinOpEquals (ValNumber num1) (ValNumber num2) _stack = pure $ valBool (num1 == num2)
runBinOp BinOpLeq    (ValNumber num1) (ValNumber num2) _stack = pure $ valBool (num1 <= num2)
runBinOp BinOpGeq    (ValNumber num1) (ValNumber num2) _stack = pure $ valBool (num1 >= num2)
runBinOp _           _                _                 stack = exception RuntimeTypeError stack

valBool :: Bool -> Value
valBool False = falseVal
valBool True  = trueVal

runLit :: Lit -> Eval Value
runLit (NumericLit _ann num) = pure (ValNumber num)
runLit (StringLit _ann str)  = pure (ValString str)

lookupTerm :: Environment -> Resolved -> Maybe Value
lookupTerm env r =
  Map.lookup (getUnique r) env

doEvalProgram :: Program Resolved -> [EvalResult]
doEvalProgram prog =
  case evalProgram prog of
    MkEval m ->
      case m (MkEvalState initialEnvironment [] {- 0 -}) of
        (_, s) -> s.results
