{-# LANGUAGE ViewPatterns #-}
module L4.Evaluate where

import Base
import qualified Base.Map as Map
import L4.Annotation
import L4.Evaluate.Value
import L4.Parser.SrcSpan (SrcRange)
import L4.Syntax
import qualified L4.TypeCheck as TypeCheck

import Data.Either

newtype Eval a = MkEval (EvalState -> (Either EvalException a, EvalState))
  deriving (Functor, Applicative, Monad, MonadError EvalException, MonadState EvalState)
    via ExceptT EvalException (StateT EvalState Identity)

type EvalResult = (SrcRange, Either EvalException Value)

data EvalState =
  MkEvalState
    { environment :: !Environment
    , results     :: [EvalResult]
    , supply      :: !Int
    }
  deriving Generic

data EvalException =
    RuntimeScopeError Resolved -- internal
  | RuntimeTypeError -- internal
  | EqualityOnUnsupportedType
  | NonExhaustivePatterns -- we could try to warn statically
  | StackOverflow
  | Unimplemented
  deriving stock (Generic, Show)
  deriving anyclass NFData

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
  pure (MkUnique 'e' i)

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

addEvalResult :: HasSrcRange a => a -> Either EvalException Value -> Eval ()
addEvalResult a val =
  let
    res = (, val) <$> rangeOf a
  in
    maybe (pure ()) (modifying #results . (:)) res

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
  | BinOpLt
  | BinOpGt

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
falseExpr = App emptyAnno TypeCheck.falseRef []

falseVal :: Value
falseVal = ValConstructor TypeCheck.falseRef []

trueExpr :: Expr Resolved
trueExpr = App emptyAnno TypeCheck.trueRef []

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
evalSection (MkSection _ann _lvl _mn _maka topdecls) =
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
  tryError (withEnvironment $ \ env -> forwardExpr env 0 Empty expr)

evalDirective :: Directive Resolved -> Eval ()
evalDirective (Eval _ann expr) = do
  v <- evalExpr expr
  addEvalResult expr v
evalDirective (Check _ _) = pure ()

maximumStackSize :: Int
maximumStackSize = 50

forwardExpr :: Environment -> Int -> Stack -> Expr Resolved -> Eval Value
forwardExpr _env ss stack _e
  | ss > maximumStackSize =
    exception StackOverflow stack
forwardExpr env !ss stack (And _ann e1 e2) =
  forwardExpr env ss stack (IfThenElse emptyAnno e1 e2 falseExpr)
forwardExpr env !ss stack (Or _ann e1 e2) =
  forwardExpr env ss stack (IfThenElse emptyAnno e1 trueExpr e2)
forwardExpr env !ss stack (Implies _ann e1 e2) =
  forwardExpr env ss stack (IfThenElse emptyAnno e1 e2 trueExpr)
forwardExpr env !ss stack (Equals _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpEquals e2 env stack) e1
forwardExpr env !ss stack (Not _ann e) =
  forwardExpr env ss stack (IfThenElse emptyAnno e falseExpr trueExpr)
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
forwardExpr env !ss stack (Lt _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpLt e2 env stack) e1
forwardExpr env !ss stack (Gt _ann e1 e2) =
  forwardExpr env (ss + 1) (BinOp1 BinOpGt e2 env stack) e1
forwardExpr env !ss stack (Proj _ann e l) =
  forwardExpr env ss stack (App emptyAnno l [e]) -- we desugar projection to plain function application
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
forwardExpr env !ss stack (AppNamed ann n [] _) =
  forwardExpr env ss stack (App ann n [])
forwardExpr _env !_ss stack (AppNamed _ann _n _nes Nothing) =
  exception RuntimeTypeError stack
forwardExpr env !ss stack (AppNamed ann n nes (Just order)) =
  let
    -- move expressions into order, drop names
    es = (\ (MkNamedExpr _ _ e) -> e) . snd <$> sortOn fst (zip order nes)
  in
    forwardExpr env ss stack (App ann n es)
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
forwardExpr env !ss stack (Where _ann e ds) = do
  -- TODO: I don't like the weird mix between abstract machine style
  -- and direct style. We should extend the abstract machine style to
  -- cover declarations.
  env' <- usingEnvironment env (traverse_ evalLocalDecl ds)
  forwardExpr env' ss stack e

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
  case boolView val1 of
    Just True  -> forwardExpr env (ss - 1) stack e2
    Just False -> forwardExpr env (ss - 1) stack e3
    Nothing    -> exception RuntimeTypeError stack0
backwardExpr !ss stack0@(Consider1 branches env stack) val =
    matchBranches val branches env stack0 ss stack
backwardExpr !ss (List1 vals [] _env stack) val =
  backwardExpr (ss - 1) stack (ValList (reverse (val : vals)))
backwardExpr !ss (List1 vals (e : es) env stack) val =
  forwardExpr env ss (List1 (val : vals) es env stack) e
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
runBinOp _           _                _                 stack = exception RuntimeTypeError stack

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
      case m (MkEvalState initialEnvironment [] 0) of
        (_, s) -> s.results
