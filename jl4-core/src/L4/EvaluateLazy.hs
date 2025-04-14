{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module L4.EvaluateLazy
  ( EvalDirectiveResult(..)
  , execEvalModuleWithEnv
  , prettyEvalException
  )
  where

import Base
import qualified Base.Text as Text
import qualified Base.Map as Map
import L4.Annotation
import L4.Evaluate.Operators
import L4.Evaluate.ValueLazy
import L4.Parser.SrcSpan (SrcRange)
import L4.Print
import L4.Syntax
import qualified L4.TypeCheck as TypeCheck

import Data.Either

newtype Eval a = MkEval (EvalState -> IO (Either EvalException a))
  deriving (Functor, Applicative, Monad, MonadError EvalException, MonadReader EvalState, MonadIO)
    via ExceptT EvalException (ReaderT EvalState IO)

data EvalState =
  MkEvalState
    { moduleUri :: !NormalizedUri
    , stack     :: !(IORef Stack)
    , supply    :: !(IORef Int)   -- used for uniques and addresses
    }

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
    BlackholeForced
  | EqualityOnUnsupportedType
  | NonExhaustivePatterns Reference -- we could try to warn statically
  | StackOverflow
  | Stuck Resolved -- ^ stores the term we got stuck on
  deriving stock (Generic, Show)
  deriving anyclass NFData

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
  BlackholeForced ->
    [ "Infinite loop detected." ]
  EqualityOnUnsupportedType -> ["Trying to check equality on types that do not support it."]
  NonExhaustivePatterns val ->
    [ "Value" ]
    <> indentMany val
    <> [ "has no corresponding pattern." ]
  StackOverflow ->
    [ "Stack overflow: "
    , "Recursion depth of " <> Text.show maximumStackSize
    , "exceeded." ]
  Stuck r ->
    [ "I could not continue evaluating, because I needed to know the value of" ]
    <> indentMany r
    <> [ "but it is an assumed term." ]

emptyEnvironment :: Environment
emptyEnvironment = Map.empty

data Stack =
  MkStack
    { size   :: !Int
    , frames :: [Frame]
    }
  deriving stock (Generic, Show)

emptyStack :: Stack
emptyStack = MkStack 0 []

data Frame =
    BinOp1 BinOp {- -} (Expr Resolved) Environment
  | BinOp2 BinOp WHNF {- -}
  | App1 {- -} [Expr Resolved] Environment
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
  | UpdateThunk Reference
  deriving stock Show

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

readRef :: (EvalState -> IORef a) -> Eval a
readRef r = asks r >>= liftIO . readIORef

writeRef :: (EvalState -> IORef a) -> a -> Eval ()
writeRef r !x = asks r >>= liftIO . flip writeIORef x

pushFrame :: Frame -> Eval ()
pushFrame frame = do
  s <- readRef (.stack)
  if s.size == maximumStackSize
    then userException StackOverflow
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

internalException :: InternalEvalException -> Eval a
internalException =
  exception . InternalEvalException

userException :: UserEvalException -> Eval a
userException =
  exception . UserEvalException

-- | For the time being, exceptions are always fatal. But we could
-- in principle have exception we can recover from ...
exception :: EvalException -> Eval a
exception exc =
  withPoppedFrame $ \ case
    Nothing -> throwError exc
    Just _f -> exception exc

tryEval :: Eval a -> Eval (Either EvalException a)
tryEval m =
  tryError m

stuckOnAssumed :: Resolved -> Eval b
stuckOnAssumed assumedResolved =
  userException (Stuck assumedResolved)

maximumStackSize :: Int
maximumStackSize = 200

lookupTerm :: Environment -> Resolved -> Maybe Reference
lookupTerm env r =
  Map.lookup (getUnique r) env

expectTerm :: Environment -> Resolved -> Eval Reference
expectTerm env r =
  case lookupTerm env r of
    Nothing -> internalException (RuntimeScopeError r)
    Just rf -> pure rf

-- NOTE: Modifications of thunks should probably be atomic,
-- because they could be updated concurrently from different
-- modules.
lookupAndUpdateRef :: Reference -> (Thunk -> (Thunk, a)) -> Eval a
lookupAndUpdateRef rf f =
  liftIO $
    atomicModifyIORef' rf.pointer f

updateThunk :: Reference -> Thunk -> Eval ()
updateThunk rf thunk = do
  liftIO (atomicWriteIORef rf.pointer $! thunk)

updateThunkToWHNF :: Reference -> WHNF -> Eval ()
updateThunkToWHNF rf v =
  updateThunk rf (WHNF v)

evalRef :: Reference -> Eval WHNF
evalRef rf =
  join $ lookupAndUpdateRef rf $ \ case
    thunk@(WHNF val) -> (thunk, backward val)
    thunk@Blackhole -> (thunk, userException BlackholeForced)
    Unevaluated e env -> (Blackhole, pushFrame (UpdateThunk rf) >> forwardExpr env e)

newAddress :: Eval Address
newAddress = do
  i <- step
  u <- asks (.moduleUri)
  pure (MkAddress u i)

newReference :: Eval Reference
newReference = do
  address <- newAddress
  reference <- liftIO (newIORef Blackhole)
  pure (MkReference address reference)

-- | Recursive pre-allocation, used for mutually recursive let-bindings / declarations.
preAllocate :: [Resolved] -> Eval Environment
preAllocate ns = do
  pairs <- traverse (\ r -> do
      let u = getUnique r
      rf <- newReference
      pure (u, rf)
    ) ns
  pure (Map.fromList pairs)

allocateValue :: WHNF -> Eval Reference
allocateValue v = do
  rf <- newReference
  updateThunk rf (WHNF v)
  pure rf

-- | Recursive allocation.
allocate :: Expr Resolved -> (Reference -> Environment) -> Eval (Reference, Environment)
allocate expr env = do
  rf <- newReference
  let
    env' = env rf
  updateThunk rf (Unevaluated expr env')
  pure (rf, env')

allocate_ :: Expr Resolved -> Environment -> Eval Reference
allocate_ (Var _ann n) env = do
  -- special case where we do not actually need to allocate
  expectTerm env n
allocate_ expr env =
  fst <$> allocate expr (const env)

forwardExpr :: Environment -> Expr Resolved -> Eval WHNF
forwardExpr env (And _ann e1 e2) =
  forwardExpr env (IfThenElse emptyAnno e1 e2 falseExpr)
forwardExpr env (Or _ann e1 e2) =
  forwardExpr env (IfThenElse emptyAnno e1 trueExpr e2)
forwardExpr env (Implies _ann e1 e2) =
  forwardExpr env (IfThenElse emptyAnno e1 e2 trueExpr)
forwardExpr env (Not _ann e) =
  forwardExpr env (IfThenElse emptyAnno e falseExpr trueExpr)
forwardExpr env (Equals _ann e1 e2) = do
  pushFrame (BinOp1 BinOpEquals e2 env)
  forwardExpr env e1
forwardExpr env (Plus _ann e1 e2) = do
  pushFrame (BinOp1 BinOpPlus e2 env)
  forwardExpr env e1
forwardExpr env (Minus _ann e1 e2) = do
  pushFrame (BinOp1 BinOpMinus e2 env)
  forwardExpr env e1
forwardExpr env (Times _ann e1 e2) = do
  pushFrame (BinOp1 BinOpTimes e2 env)
  forwardExpr env e1
forwardExpr env (DividedBy _ann e1 e2) = do
  pushFrame (BinOp1 BinOpDividedBy e2 env)
  forwardExpr env e1
forwardExpr env (Modulo _ann e1 e2) = do
  pushFrame (BinOp1 BinOpModulo e2 env)
  forwardExpr env e1
forwardExpr env (Leq _ann e1 e2) = do
  pushFrame (BinOp1 BinOpLeq e2 env)
  forwardExpr env e1
forwardExpr env (Geq _ann e1 e2) = do
  pushFrame (BinOp1 BinOpGeq e2 env)
  forwardExpr env e1
forwardExpr env (Lt _ann e1 e2) = do
  pushFrame (BinOp1 BinOpLt e2 env)
  forwardExpr env e1
forwardExpr env (Gt _ann e1 e2) = do
  pushFrame (BinOp1 BinOpGt e2 env)
  forwardExpr env e1
forwardExpr env (Proj _ann e l) =
  forwardExpr env (App emptyAnno l [e]) -- we desugar projection to plain function application
forwardExpr env (Var _ann n) = -- still problematic: similarity / overlap between this and App with no args
  expectTerm env n >>= evalRef
forwardExpr env (Cons _ann e1 e2) = do
  rf1 <- allocate_ e1 env
  rf2 <- allocate_ e2 env
  backward (ValCons rf1 rf2)
forwardExpr env (Lam _ann givens e) =
  backward (ValClosure givens e env)
forwardExpr env (App _ann n []) =
  expectTerm env n >>= evalRef
forwardExpr env (App _ann n es@(_ : _)) = do
  pushFrame (App1 es env)
  forwardExpr env (Var emptyAnno n)
forwardExpr env (AppNamed ann n [] _) =
  forwardExpr env (App ann n [])
forwardExpr _env (AppNamed _ann _n _nes Nothing) =
  internalException (RuntimeTypeError "named application where the order of arguments is not resolved")
forwardExpr env (AppNamed ann n nes (Just order)) =
  let
    -- move expressions into order, drop names
    es = (\ (MkNamedExpr _ _ e) -> e) . snd <$> sortOn fst (zip order nes)
  in
    forwardExpr env (App ann n es)
forwardExpr env (IfThenElse _ann e1 e2 e3) = do
  pushFrame (IfThenElse1 e2 e3 env)
  forwardExpr env e1
forwardExpr env (Consider _ann e branches) = do
  rf <- allocate_ e env
  matchBranches rf env branches
forwardExpr _env (Lit _ann lit) = do
  rval <- runLit lit
  backward rval
forwardExpr _env (List _ann []) =
  backward ValNil
forwardExpr env (List _ann (e : es)) =
  forwardExpr env (Cons emptyAnno e (List emptyAnno es))
forwardExpr env (Where _ann e ds) = do
  env' <- evalRecLocalDecls env ds
  let combinedEnv = Map.union env' env
  forwardExpr combinedEnv e

backward :: WHNF -> Eval WHNF
backward val = withPoppedFrame $ \ case
  Nothing -> pure val
  Just (BinOp1 binOp e2 env) -> do
    pushFrame (BinOp2 binOp val)
    forwardExpr env e2
  Just (BinOp2 binOp val1) -> do
    runBinOp binOp val1 val
  Just f@(App1 es env) -> do
    case val of
      ValClosure givens e env' -> do
        env'' <- matchGivens givens f env es
        forwardExpr (Map.union env'' env') e
      ValUnappliedConstructor r -> do
        refs <- traverse (flip allocate_ env) es
        backward (ValConstructor r refs)
      ValAssumed r ->
        stuckOnAssumed r -- TODO: we can do better here
      res -> internalException (RuntimeTypeError $ "expected a function but found: " <> prettyLayout res)
  Just (IfThenElse1 e2 e3 env) ->
    case boolView val of
      Just True ->
        forwardExpr env e2
      Just False ->
        forwardExpr env e3
      Nothing | ValAssumed r <- val ->
        stuckOnAssumed r
      Nothing ->
        internalException (RuntimeTypeError $ "expected a BOOLEAN but found: " <> prettyLayout val <> " when evaluating IF-THEN-ELSE")
  Just (ConsiderWhen1 _scrutinee e _branches env) -> do
    case val of
      ValEnvironment env' ->
        forwardExpr (Map.union env' env) e
      _ ->
        internalException (RuntimeTypeError $ "expected an environment but found: " <> prettyLayout val <> " when evaluating WHEN")
  Just PatNil0 -> do
    case val of
      ValNil ->
        backward (ValEnvironment Map.empty)
      _ ->
        patternMatchFailure
  Just (PatCons0 p1 p2) -> do
    case val of
      ValCons rf1 rf2 -> do
        pushFrame (PatCons1 rf2 p2)
        matchPattern rf1 p1
      _ ->
        patternMatchFailure
  Just (PatCons1 rf2 p2) -> do
    case val of
      ValEnvironment env1 -> do
        pushFrame (PatCons2 env1)
        matchPattern rf2 p2
      _ ->
        internalException (RuntimeTypeError $ "expected an environment but found: " <> prettyLayout val <> " when matching FOLLOWED BY")
  Just (PatCons2 env1) ->
    case val of
      ValEnvironment env2 ->
        backward (ValEnvironment (Map.union env2 env1))
      _ ->
        internalException (RuntimeTypeError $ "expected an environment but found: " <> prettyLayout val <> " when matching FOLLOWED BY")
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
                    pushFrame (PatApp1 [] rps)
                    matchPattern r p
            else internalException (RuntimeTypeError $ "pattern for constructor has the wrong number of arguments")
      _ ->
        patternMatchFailure
  Just (PatApp1 envs rps) ->
    case val of
      ValEnvironment env ->
        case rps of
          []              -> backward (ValEnvironment (Map.unions (env : envs)))
          ((r, p) : rps') -> do
            pushFrame (PatApp1 (env : envs) rps')
            matchPattern r p
      _ -> internalException (RuntimeTypeError $ "expected an environment but found: " <> prettyLayout val <> " when matching constructor")
  Just (EqConstructor1 rf rfs) -> do
    pushFrame (EqConstructor2 val rfs)
    evalRef rf
  Just (EqConstructor2 val1 rfs) -> do
    pushFrame (EqConstructor3 rfs)
    runBinOpEquals val1 val
  Just (EqConstructor3 rfs) ->
    case boolView val of
      Just False -> backward $ valBool False
      Just True ->
        case rfs of
          [] -> backward $ valBool True
          ((r1, r2) : rfs') -> do
            pushFrame (EqConstructor1 r2 rfs')
            evalRef r1
      Nothing -> internalException (RuntimeTypeError $ "expected a BOOLEAN but found: " <> prettyLayout val <> " when testing equality")
  Just (UpdateThunk rf) -> do
    updateThunkToWHNF rf val
    backward val

matchGivens :: GivenSig Resolved -> Frame -> Environment -> [Expr Resolved] -> Eval Environment
matchGivens (MkGivenSig _ann otns) f env es = do
  let
    (_tyvars, others) = partitionEithers (TypeCheck.isQuantifier <$> otns)
  if length others == length es
    then do
      refs <- traverse (flip allocate_ env) es
      pure $ Map.fromList (zipWith (\ (r, _) v -> (getUnique r, v)) others refs)
    else do
      pushFrame f -- provides better error context
      internalException (RuntimeTypeError "given signatures' values' lengths do not match")

matchBranches :: Reference -> Environment -> [Branch Resolved] -> Eval WHNF
matchBranches scrutinee _env [] =
  userException (NonExhaustivePatterns scrutinee)
matchBranches _scrutinee env (Otherwise _ann e : _) =
  forwardExpr env e
matchBranches scrutinee env (When _ann pat e : branches) = do
  pushFrame (ConsiderWhen1 scrutinee e branches env)
  matchPattern scrutinee pat

matchPattern :: Reference -> Pattern Resolved -> Eval WHNF
matchPattern scrutinee (PatVar _ann n) = do
  backward (ValEnvironment (Map.singleton (getUnique n) scrutinee))
matchPattern scrutinee (PatApp _ann n [])
  | getUnique n == TypeCheck.emptyUnique = do -- pattern for the empty list
  pushFrame PatNil0
  evalRef scrutinee
matchPattern scrutinee (PatCons _ann p1 p2) = do
  pushFrame (PatCons0 p1 p2 )
  evalRef scrutinee
matchPattern scrutinee (PatApp _ann n ps) = do
  pushFrame (PatApp0 n ps )
  evalRef scrutinee

-- | This unwinds the stack until it finds the enclosing pattern match and then resumes.
patternMatchFailure :: Eval WHNF
patternMatchFailure = withPoppedFrame $ \ case
  Nothing ->
    internalException UnhandledPatternMatch
  Just (ConsiderWhen1 scrutinee _ branches env) ->
    matchBranches scrutinee env branches
  Just _ ->
    patternMatchFailure

runLit :: Lit -> Eval WHNF
runLit (NumericLit _ann num) = pure (ValNumber num)
runLit (StringLit _ann str)  = pure (ValString str)

runBinOp :: BinOp -> WHNF -> WHNF -> Eval WHNF
runBinOp BinOpPlus   (ValNumber num1) (ValNumber num2)           = backward $ ValNumber (num1 + num2)
runBinOp BinOpMinus  (ValNumber num1) (ValNumber num2)           = backward $ ValNumber (num1 - num2)
runBinOp BinOpTimes  (ValNumber num1) (ValNumber num2)           = backward $ ValNumber (num1 * num2)
runBinOp BinOpDividedBy (ValNumber num1) (ValNumber num2)        = backward $ ValNumber (num1 `div` num2)
runBinOp BinOpModulo    (ValNumber num1) (ValNumber num2)        = backward $ ValNumber (num1 `mod` num2)
runBinOp BinOpEquals val1             val2                       = runBinOpEquals val1 val2
runBinOp BinOpLeq    (ValNumber num1) (ValNumber num2)           = backward $ ValBool (num1 <= num2)
runBinOp BinOpLeq    (ValString str1) (ValString str2)           = backward $ ValBool (str1 <= str2)
runBinOp BinOpLeq    (ValBool b1)     (ValBool b2)               = backward $ ValBool (b1 <= b2)
runBinOp BinOpGeq    (ValNumber num1) (ValNumber num2)           = backward $ ValBool (num1 >= num2)
runBinOp BinOpGeq    (ValString str1) (ValString str2)           = backward $ ValBool (str1 >= str2)
runBinOp BinOpGeq    (ValBool b1)     (ValBool b2)               = backward $ ValBool (b1 >= b2)
runBinOp BinOpLt     (ValNumber num1) (ValNumber num2)           = backward $ ValBool (num1 < num2)
runBinOp BinOpLt     (ValString str1) (ValString str2)           = backward $ ValBool (str1 < str2)
runBinOp BinOpLt     (ValBool b1)     (ValBool b2)               = backward $ ValBool (b1 < b2)
runBinOp BinOpGt     (ValNumber num1) (ValNumber num2)           = backward $ ValBool (num1 > num2)
runBinOp BinOpGt     (ValString str1) (ValString str2)           = backward $ ValBool (str1 > str2)
runBinOp BinOpGt     (ValBool b1)     (ValBool b2)               = backward $ ValBool (b1 > b2)
runBinOp _op         (ValAssumed r) _e2                          = stuckOnAssumed r
runBinOp _op         _e1 (ValAssumed r)                          = stuckOnAssumed r
runBinOp _           _                _                          = internalException (RuntimeTypeError "running bin op with invalid operation / value combination")

runBinOpEquals :: WHNF -> WHNF -> Eval WHNF
runBinOpEquals (ValNumber num1)        (ValNumber num2) = backward $ valBool $ num1 == num2
runBinOpEquals (ValString str1)        (ValString str2) = backward $ valBool $ str1 == str2
runBinOpEquals ValNil                  ValNil           = backward $ valBool True
runBinOpEquals (ValCons r1 rs1)        (ValCons r2 rs2) = do
  pushFrame (EqConstructor1 r2 [(rs1, rs2)])
  evalRef r1
runBinOpEquals (ValConstructor n1 rs1) (ValConstructor n2 rs2)
  | sameResolved n1 n2 && length rs1 == length rs2 =
    let
      pairs = zip rs1 rs2
    in
      case pairs of
        [] -> backward $ ValBool True
        ((r1, r2) : rss) -> do
          pushFrame (EqConstructor1 r2 rss)
          evalRef r1
  | otherwise                                           = backward $ ValBool False
runBinOpEquals _                       _                = userException EqualityOnUnsupportedType

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

evalModule :: Environment -> Module Resolved -> Eval (Environment, [EvalDirective])
evalModule env (MkModule _ann _uri sections) = do
  names <- concat <$> traverse scanSection sections
  env' <- preAllocate names
  let combinedEnv = Map.union env' env
  directives <- concat <$> traverse (evalSection combinedEnv) sections
  pure (env', directives)

-- | Doesn't do any actual evaluation, just performs allocations
-- and returns the environment containing all the new bindings.
evalRecLocalDecls :: Environment -> [LocalDecl Resolved] -> Eval Environment
evalRecLocalDecls env decls = do
  names <- concat <$> traverse scanLocalDecl decls
  env' <- preAllocate names
  let combinedEnv = Map.union env' env
  traverse_ (evalLocalDecl combinedEnv) decls
  pure env'

-- | Just collect all defined names; could plausibly be done generically.
scanSection :: Section Resolved -> Eval [Resolved]
scanSection (MkSection _ann _lvl _mn _maka topdecls) =
  concat <$> traverse scanTopDecl topdecls

-- | Just collect all defined names; could plausibly be done generically.
scanTopDecl :: TopDecl Resolved -> Eval [Resolved]
scanTopDecl (Declare _ann declare) =
  scanDeclare declare
scanTopDecl (Decide _ann decide) =
  scanDecide decide
scanTopDecl (Assume _ann assume) =
  scanAssume assume
scanTopDecl (Directive _ann _directive) =
  pure []
scanTopDecl (Import _ann _import_) =
  pure []

-- | Just collect all defined names; could plausibly be done generically.
scanLocalDecl :: LocalDecl Resolved -> Eval [Resolved]
scanLocalDecl (LocalDecide _ann decide) =
  scanDecide decide
scanLocalDecl (LocalAssume _ann assume) =
  scanAssume assume

scanDecide :: Decide Resolved -> Eval [Resolved]
scanDecide (MkDecide _ann _tysig (MkAppForm _ n _ _) _expr) = pure [n]

scanAssume :: Assume Resolved -> Eval [Resolved]
scanAssume (MkAssume _ann _tysig (MkAppForm _ n _ _) _t) = pure [n]

-- | The only run-time names a type declaration brings into scope are constructors and selectors.
scanDeclare :: Declare Resolved -> Eval [Resolved]
scanDeclare (MkDeclare _ann _tysig _appFormAka t) = scanTypeDecl t

scanTypeDecl :: TypeDecl Resolved -> Eval [Resolved]
scanTypeDecl (EnumDecl _ann conDecls) =
  concat <$> traverse scanConDecl conDecls
scanTypeDecl (RecordDecl _ann mcon tns) =
  concat <$> traverse (\ c -> scanConDecl (MkConDecl emptyAnno c tns)) (toList mcon)
scanTypeDecl (SynonymDecl _ann _t) =
  pure []

scanConDecl :: ConDecl Resolved -> Eval [Resolved]
scanConDecl (MkConDecl _ann n [])  = pure [n]
scanConDecl (MkConDecl _ann n tns) = pure (n : ((\ (MkTypedName _ n' _) -> n') <$> tns))

evalSection :: Environment -> Section Resolved -> Eval [EvalDirective]
evalSection env (MkSection _ann _lvl _mn _maka topdecls) =
  concat <$> traverse (evalTopDecl env) topdecls

evalTopDecl :: Environment -> TopDecl Resolved -> Eval [EvalDirective]
evalTopDecl env (Declare _ann declare) =
  [] <$ evalDeclare env declare
evalTopDecl env (Decide _ann decide) =
  [] <$ evalDecide env decide
evalTopDecl env (Assume _ann assume) =
  [] <$ evalAssume env assume
evalTopDecl env (Directive _ann directive) =
  evalDirective env directive
evalTopDecl _env (Import _ann _import_) =
  pure []

evalDirective :: Environment -> Directive Resolved -> Eval [EvalDirective]
evalDirective env (LazyEval _ann expr) =
  pure ((\ r -> MkEvalDirective r expr env) <$> toList (rangeOf expr))
evalDirective _env (StrictEval _ann _expr) =
  pure []
evalDirective _env (Check _ann _expr) =
  pure []

evalLocalDecl :: Environment -> LocalDecl Resolved -> Eval ()
evalLocalDecl env (LocalDecide _ann decide) =
  evalDecide env decide
evalLocalDecl env (LocalAssume _ann assume) =
  evalAssume env assume

-- We are assuming that the environment already contains an entry with an address for us.
evalAssume :: Environment -> Assume Resolved -> Eval ()
evalAssume env (MkAssume _ann _tysig (MkAppForm _ n []   _maka) _) =
  updateTerm env n (WHNF (ValAssumed n))
evalAssume env (MkAssume _ann _tysig (MkAppForm _ n _args _maka) _) = do
  -- TODO: we should create a given here yielding an assumed, but we currently cannot do that easily,
  -- because we do not have Assumed as an expression, and we also cannot embed values into expressions.
  updateTerm env n (WHNF (ValAssumed n))

updateTerm :: Environment -> Resolved -> Thunk -> Eval ()
updateTerm env n thunk = do
  rf <- expectTerm env n
  updateThunk rf thunk

-- We are assuming that the environment already contains an entry with an address for us.
evalDecide :: Environment -> Decide Resolved -> Eval ()
evalDecide env (MkDecide _ann _tysig (MkAppForm _ n []   _maka) expr) =
  updateTerm env n (Unevaluated expr env)
evalDecide env (MkDecide _ann _tysig (MkAppForm _ n args _maka) expr) = do
  let
    v = ValClosure (MkGivenSig emptyAnno ((\ r -> MkOptionallyTypedName emptyAnno r Nothing) <$> args)) expr env
  updateTerm env n (WHNF v)

-- We are assuming that the environment already contains an entry with an address for us.
evalDeclare :: Environment -> Declare Resolved -> Eval ()
evalDeclare env (MkDeclare _ann _tysig _appFormAka t) =
  evalTypeDecl env t

evalTypeDecl :: Environment -> TypeDecl Resolved -> Eval ()
evalTypeDecl env (EnumDecl _ann conDecls) =
  traverse_ (evalConDecl env) conDecls
evalTypeDecl env (RecordDecl _ann mcon tns) =
  traverse_ (\ c -> evalConDecl env (MkConDecl emptyAnno c tns)) mcon
evalTypeDecl _env (SynonymDecl _ann _t) =
  pure ()

evalConDecl :: Environment -> ConDecl Resolved -> Eval ()
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
    updateTerm env sn (WHNF sel)
    ) (zip [0 ..] tns)

falseExpr :: Expr Resolved
falseExpr = App emptyAnno TypeCheck.falseRef []

falseVal :: WHNF
falseVal = ValConstructor TypeCheck.falseRef []

trueExpr :: Expr Resolved
trueExpr = App emptyAnno TypeCheck.trueRef []

trueVal :: WHNF
trueVal = ValConstructor TypeCheck.trueRef []

-- The initial environment has to be built by pre-allocation.
initialEnvironment :: Eval Environment
initialEnvironment = do
  falseRef <- allocateValue falseVal
  trueRef  <- allocateValue trueVal
  nilRef   <- allocateValue ValNil
  pure $
    Map.fromList
      [ (TypeCheck.falseUnique, falseRef)
      , (TypeCheck.trueUnique,  trueRef)
      , (TypeCheck.emptyUnique, nilRef)
      ]

-- TODO: This currently allocates the initial environment once per module.
-- This isn't a big deal, but can we somehow do this only once per program,
-- for example by passing this in from the outside?
evalModuleAndDirectives :: Environment -> Module Resolved -> Eval (Environment, [EvalDirectiveResult])
evalModuleAndDirectives env m = do
  ienv <- initialEnvironment
  (env', directives) <- evalModule (env <> ienv) m
  results <- traverse nfDirective directives
  -- NOTE: We are only returning the new definitions of this module, not any imports.
  -- Depending on future export semantics, this may have to change.
  pure (env', results)

-- | Evaluate an EVAL directive. For this, we evaluate to normal form,
-- not just WHNF.
--
nfDirective :: EvalDirective -> Eval EvalDirectiveResult
nfDirective (MkEvalDirective r expr env) = do
  v <- tryEval $ do
    whnf <- forwardExpr env expr
    nf whnf
  pure (MkEvalDirectiveResult r v)

-- | Evaluate WHNF to NF, with a cutoff (which possibly could be made configurable).
nf :: WHNF -> Eval NF
nf = nfAux maximumStackSize

nfAux :: Int -> WHNF -> Eval NF
nfAux  d _v | d < 0                  = pure ToDeep
nfAux _d (ValNumber i)               = pure (MkNF (ValNumber i))
nfAux _d (ValString s)               = pure (MkNF (ValString s))
nfAux _d ValNil                      = pure (MkNF ValNil)
nfAux  d (ValCons r1 r2)             = do
  v1 <- evalRef r1 >>= nfAux (d - 1)
  v2 <- evalRef r2 >>= nfAux (d - 1)
  pure (MkNF (ValCons v1 v2))
nfAux _d (ValClosure givens e env)   = pure (MkNF (ValClosure givens e env))
nfAux _d (ValUnappliedConstructor n) = pure (MkNF (ValUnappliedConstructor n))
nfAux  d (ValConstructor n rs)       = do
  vs <- traverse (\ r -> evalRef r >>= nfAux (d - 1)) rs
  pure (MkNF (ValConstructor n vs))
nfAux _d (ValAssumed n)              = pure (MkNF (ValAssumed n))
nfAux _d (ValEnvironment env)        = pure (MkNF (ValEnvironment env))

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
      initialStack  <- newIORef emptyStack
      initialSupply <- newIORef 0
      r <- f (MkEvalState moduleUri initialStack initialSupply)
      case r of
        Left _exc -> do
          -- exceptions at the top-level are unusual; after all, we don't actually
          -- force any evaluation here, and we catch exceptions for eval directives
          pure (emptyEnvironment, [])
        Right result -> do
          pure result

data EvalDirective =
  MkEvalDirective
    { range :: !SrcRange -- ^ of the (L)EVAL directive
    , expr  :: !(Expr Resolved) -- ^ expression to evaluate
    , env   :: !Environment -- ^ environment to evaluate the expression in
    }
  deriving stock (Generic, Show)

data EvalDirectiveResult =
  MkEvalDirectiveResult
    { range  :: !SrcRange -- ^ of the (L)EVAL directive
    , result :: Either EvalException NF
    }
  deriving stock (Generic, Show)
  deriving anyclass NFData
