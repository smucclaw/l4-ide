{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module L4.EvaluateLazy.Machine
( Frame
, EvalException (..)
, UserEvalException (..)
, InternalEvalException (..)
, Machine (Allocate, AllocateValue, PreAllocate, ..)
, Allocation (..)
, Config (..)
, forwardExpr
, matchBranches
, matchPattern
, backward
, EvalDirective (..)
, evalModule
, maximumStackSize
, initialEnvironment
, evalRef
, emptyEnvironment
, prettyEvalException
, boolView
, pattern ValBool
-- * Constants exposed for the eager evaluator
, builtinBinOps
)
where

import Base
import qualified Base.Text as Text
import qualified Base.Map as Map
import qualified Base.Set as Set
import Control.Concurrent
import           Network.HTTP.Req ((=:))
import qualified Network.HTTP.Req as Req
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import L4.Annotation
import L4.Evaluate.Operators
import L4.Evaluate.ValueLazy
import L4.Parser.SrcSpan (SrcRange)
import L4.Print
import L4.Syntax
import qualified L4.TypeCheck as TypeCheck
import L4.TypeCheck.Types (EntityInfo)
import L4.EvaluateLazy.ContractFrame
import L4.Utils.Ratio

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
  | UpdateThunk Reference
  | ContractFrame ContractFrame
  | ConcatFrame [WHNF] {- -} [Expr Resolved] Environment -- accumulated values, remaining exprs, env
  | AsStringFrame -- AsString frame
  | JsonEncodeListFrame [Text] {- -} Reference Bool -- accumulated JSON strings, tail reference, expecting_tail (True = next value is tail, False = next value is element)
  | JsonEncodeNestedFrame [Text] {- -} Reference -- accumulated JSON strings, tail reference (waiting for nested list encoding to complete)
  | JsonEncodeConstructorFrame [(Text, Text)] Text [(Text, Reference)] -- accumulated (fieldName, encodedJson) pairs, current field name, remaining (fieldName, fieldRef) pairs to encode
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
  | EqualityOnUnsupportedType WHNF WHNF
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
  GetEntityInfo :: Machine EntityInfo
  PokeThunk :: Reference
    -> (ThreadId -> Thunk -> (Thunk, a))
    -> Machine a
  LiftIO :: IO a -> Machine a
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

instance MonadIO Machine where
  liftIO = LiftIO

pattern Allocate :: Expr Resolved -> (Reference -> Environment) -> Machine (Reference, Environment)
pattern Allocate expr k = Allocate' (Recursive expr k)

pattern AllocateValue :: WHNF -> Machine Reference
pattern AllocateValue whnf = Allocate' (Value whnf)

pattern PreAllocate :: Resolved -> Machine (Unique, Reference)
pattern PreAllocate r = Allocate' (PreAllocation r)

data Config
  = ForwardMachine Environment (Expr Resolved)
  | MatchBranchesMachine Reference Environment [Branch Resolved]
  | MatchPatternMachine Reference Environment (Pattern Resolved)
  | BackwardMachine WHNF
  | EvalRefMachine Reference
  | DoneMachine WHNF

pattern ForwardExpr :: Environment -> Expr Resolved -> Machine Config
pattern ForwardExpr env e = Config (ForwardMachine env e)

pattern MatchBranches :: Reference -> Environment -> [Branch Resolved] -> Machine Config
pattern MatchBranches r env e = Config (MatchBranchesMachine r env e)

pattern MatchPattern :: Reference -> Environment -> Pattern Resolved -> Machine Config
pattern MatchPattern r env pat = Config (MatchPatternMachine r env pat)

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

pattern StuckOnAssumed :: Resolved -> Machine b
pattern StuckOnAssumed assumedResolved = UserException (Stuck assumedResolved)


forwardExpr :: Environment -> Expr Resolved -> Machine Config
forwardExpr env = \ case
  RAnd _ann e1 e2 -> Backward (ValROp env ValRAnd (Left e1) (Left e2))
  ROr  _ann e1 e2 -> Backward (ValROp env ValROr (Left e1) (Left e2))
  And  _ann e1 e2 ->
    ForwardExpr env (IfThenElse emptyAnno e1 e2 falseExpr)
  Or   _ann e1 e2 ->
    ForwardExpr env (IfThenElse emptyAnno e1 trueExpr e2)
  Implies _ann e1 e2 ->
    ForwardExpr env (IfThenElse emptyAnno e1 e2 trueExpr)
  Not _ann e ->
    ForwardExpr env (IfThenElse emptyAnno e falseExpr trueExpr)
  Equals _ann e1 e2 -> do
    PushFrame (BinOp1 BinOpEquals e2 env)
    ForwardExpr env e1
  Plus _ann e1 e2 -> do
    PushFrame (BinOp1 BinOpPlus e2 env)
    ForwardExpr env e1
  Minus _ann e1 e2 -> do
    PushFrame (BinOp1 BinOpMinus e2 env)
    ForwardExpr env e1
  Times _ann e1 e2 -> do
    PushFrame (BinOp1 BinOpTimes e2 env)
    ForwardExpr env e1
  DividedBy _ann e1 e2 -> do
    PushFrame (BinOp1 BinOpDividedBy e2 env)
    ForwardExpr env e1
  Modulo _ann e1 e2 -> do
    PushFrame (BinOp1 BinOpModulo e2 env)
    ForwardExpr env e1
  Leq _ann e1 e2 -> do
    PushFrame (BinOp1 BinOpLeq e2 env)
    ForwardExpr env e1
  Geq _ann e1 e2 -> do
    PushFrame (BinOp1 BinOpGeq e2 env)
    ForwardExpr env e1
  Lt _ann e1 e2 -> do
    PushFrame (BinOp1 BinOpLt e2 env)
    ForwardExpr env e1
  Gt _ann e1 e2 -> do
    PushFrame (BinOp1 BinOpGt e2 env)
    ForwardExpr env e1
  Proj _ann e l ->
    ForwardExpr env (App emptyAnno l [e]) -- we desugar projection to plain function application
  Var _ann n -> -- still problematic: similarity / overlap between this and App with no args
    expectTerm env n >>= EvalRef
  Cons _ann e1 e2 -> do
    rf1 <- allocate_ e1 env
    rf2 <- allocate_ e2 env
    Backward (ValCons rf1 rf2)
  Lam _ann givens e ->
    Backward (ValClosure givens e env)
  App _ann n [] ->
    expectTerm env n >>= EvalRef
  App ann n es@(_ : _) -> do
    let expectedType = case getAnno ann of
          Anno {extra = Extension {resolvedInfo = Just (TypeInfo ty _)}} -> Just ty
          _ -> Nothing
    rs <- traverse (`allocate_` env) es
    PushFrame (App1 rs expectedType)
    ForwardExpr env (Var emptyAnno n)
  AppNamed ann n [] _ ->
    ForwardExpr env (App ann n [])
  AppNamed _ann _n _nes Nothing ->
    InternalException $ RuntimeTypeError
      "named application where the order of arguments is not resolved"
  AppNamed ann n nes (Just order) ->
    let
     -- move expressions into order, drop names
      es = (\ (MkNamedExpr _ _ e) -> e) . snd <$> sortOn fst (zip order nes)
    in
      ForwardExpr env (App ann n es)
  IfThenElse _ann e1 e2 e3 -> do
    PushFrame (IfThenElse1 e2 e3 env)
    ForwardExpr env e1
  MultiWayIf _ann es e -> ForwardExpr env $ desugarMultiWayIf es e
    where
    desugarMultiWayIf :: [GuardedExpr Resolved] -> Expr Resolved -> Expr Resolved
    desugarMultiWayIf [] o = o
    desugarMultiWayIf (MkGuardedExpr _ann c f : es') o = IfThenElse emptyAnno c f $ desugarMultiWayIf es' o
  Consider _ann e branches -> do
    rf <- allocate_ e env
    MatchBranches rf env branches
  Lit _ann lit -> do
    rval <- runLit lit
    Backward rval
  Percent _ann e -> do
    PushFrame (UnaryBuiltin0 UnaryPercent Nothing)
    ForwardExpr env e
  List _ann [] ->
    Backward ValNil
  List _ann (e : es) ->
    ForwardExpr env (Cons emptyAnno e (List emptyAnno es))
  Where _ann e ds -> do
    env' <- evalRecLocalDecls env ds
    let combinedEnv = Map.union env' env
    ForwardExpr combinedEnv e
  Regulative _ann (MkObligation _ party action due followup lest) ->
    Backward (ValObligation env (Left party) action (Left due) (fromMaybe fulfilExpr followup) lest)
  Event _ann ev ->
    ForwardExpr env (desugarEvent ev)
  Fetch _ann e -> do
    PushFrame (UnaryBuiltin0 UnaryFetch Nothing)
    ForwardExpr env e
  Post _ann e1 e2 e3 -> do
    PushFrame (Post1 e2 e3 env)
    ForwardExpr env e1
  Concat _ann [] ->
    Backward (ValString "")
  Concat _ann (e : es) -> do
    PushFrame (ConcatFrame [] es env)
    ForwardExpr env e
  AsString _ann e -> do
    PushFrame AsStringFrame
    ForwardExpr env e

backward :: WHNF -> Machine Config
backward val = WithPoppedFrame $ \ case
  Nothing -> Done val
  Just (BinOp1 binOp e2 env) -> do
    PushFrame (BinOp2 binOp val)
    ForwardExpr env e2
  Just (BinOp2 binOp val1) -> do
    runBinOp binOp val1 val
  Just (Post1 e2 e3 env) -> do
    PushFrame (Post2 val e3 env)
    ForwardExpr env e2
  Just (Post2 val1 e3 env) -> do
    PushFrame (Post3 val1 val)
    ForwardExpr env e3
  Just (Post3 val1 val2) -> do
    runPost val1 val2 val
  Just (BinBuiltin1 binOp r) -> do
    PushFrame (BinBuiltin2 binOp val)
    EvalRef r
  Just (BinBuiltin2 binOp val1) ->
    runBinOp binOp val1 val
  Just f@(App1 rs mTy) -> do
    case val of
      ValClosure givens e env' -> do
        env'' <- matchGivens givens f rs
        ForwardExpr (Map.union env'' env') e
      ValUnappliedConstructor r ->
        Backward (ValConstructor r rs)
      ValObligation env party act due followup lest -> do
        (time, events) <- case rs of
          [t, r] -> pure (t, r)
          rs' -> InternalException $ RuntimeTypeError $
            "expected a time stamp, and a list of events but found: " <> foldMap prettyLayout rs'
        PushFrame (ContractFrame (Contract1 ScrutinizeEvents {..}))
        EvalRef events
      ValROp env op rexpr1 rexpr2 -> do
        -- make sure to reassemble the operation after returning
        PushFrame $ ContractFrame $ RBinOp1 MkRBinOp1 {args = rs, ..}
        -- apply the arguments of the left hand expression to the
        -- expression
        PushFrame f
        maybeEvaluate env rexpr1 -- TODO: build application
      ValUnaryBuiltinFun fn -> do
        r <- expect1 rs
        PushFrame (UnaryBuiltin0 fn mTy)
        EvalRef r
      ValBinaryBuiltinFun fn -> do
        (x, y) <- expect2 rs
        case fn of
          -- 'BinOpCons' doesn't need to evaluate anything!
          BinOpCons -> do
            Backward $ ValCons x y
          _ -> do
            PushFrame (BinBuiltin1 fn y)
            EvalRef x
      ValFulfilled -> Backward ValFulfilled
      ValAssumed r ->
        StuckOnAssumed r -- TODO: we can do better here
      res -> InternalException (RuntimeTypeError $ "expected a function but found: " <> prettyLayout res)
  Just (IfThenElse1 e2 e3 env) ->
    case val of
      ValBool True -> ForwardExpr env e2
      ValBool False -> ForwardExpr env e3

      ValAssumed r -> StuckOnAssumed r

      _ -> InternalException $ RuntimeTypeError $
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
  Just (PatCons0 p1 env p2) -> do
    case val of
      ValCons rf1 rf2 -> do
        PushFrame (PatCons1 rf2 env p2)
        MatchPattern rf1 env p1
      _ ->
        patternMatchFailure
  Just (PatCons1 rf2 env p2) -> do
    case val of
      ValEnvironment env1 -> do
        PushFrame (PatCons2 env1)
        MatchPattern rf2 env p2
      _ ->
        InternalException $ RuntimeTypeError $
          "expected an environment but found: " <> prettyLayout val <> " when matching FOLLOWED BY"
  Just (PatCons2 env1) ->
    case val of
      ValEnvironment env2 ->
        Backward (ValEnvironment (Map.union env2 env1))
      _ -> InternalException $ RuntimeTypeError $
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
                  []             -> Backward (ValEnvironment Map.empty)
                  ((r, p) : rps) -> do
                    PushFrame (PatApp1 [] rps)
                    MatchPattern r env p
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
            MatchPattern r env p
      _ -> InternalException $ RuntimeTypeError $
        "expected an environment but found: " <> prettyLayout val <> " when matching constructor"
  Just (PatLit0 env lit) -> do
    PushFrame (PatLit1 val)
    ForwardExpr env lit
  Just (PatLit1 lit) -> do
    PushFrame PatLit2
    runBinOpEquals lit val
  Just PatLit2 ->
    case val of
      -- NOTE: in future, we may give the pattern that was matched a name, potentially
      ValBool True -> Backward $ ValEnvironment emptyEnvironment
      ValBool False -> patternMatchFailure
      _ -> InternalException $ RuntimeTypeError $
        "expected a boolean but found: " <> prettyLayout val <> " while matching literal pattern"
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
  Just (UnaryBuiltin0 fn mTy) -> do
    runBuiltin val fn mTy
  Just (ConcatFrame acc [] _env) -> do
    -- All arguments evaluated, concatenate them
    runConcat (reverse (val : acc))
  Just (ConcatFrame acc (e : es) env) -> do
    -- Evaluate next argument
    PushFrame (ConcatFrame (val : acc) es env)
    ForwardExpr env e
  Just AsStringFrame -> do
    -- Convert the value to string
    runAsString val
  Just (JsonEncodeListFrame acc tailRef expectingTail) -> do
    -- Handle the value we got back
    if expectingTail
      then
        -- We just evaluated the tail, so val is either ValNil or ValCons
        case val of
          ValNil -> do
            -- We're done! Combine all accumulated JSON strings into an array
            let jsonArray = "[" <> Text.intercalate "," (reverse acc) <> "]"
            Backward $ ValString jsonArray
          ValCons headRef nextTailRef -> do
            -- More elements to process. Evaluate the head element first
            PushFrame (JsonEncodeListFrame acc nextTailRef False)
            EvalRef headRef
          _ ->
            -- Should not happen - tail should be ValNil or ValCons
            InternalException $ RuntimeTypeError "Expected list (ValNil or ValCons) for tail"
      else
        -- We just evaluated an element, so encode it and continue with the tail
        case val of
          ValNil -> do
            -- Element is an empty list
            PushFrame (JsonEncodeListFrame ("[]" : acc) tailRef True)
            EvalRef tailRef
          ValCons elemHeadRef elemTailRef -> do
            -- Element is a non-empty list, need to recursively encode it
            -- Push a frame to wait for the nested encoding, then start encoding the nested list
            PushFrame (JsonEncodeNestedFrame acc tailRef)  -- Will continue with tail after nested encoding
            PushFrame (JsonEncodeListFrame [] elemTailRef False)  -- Encode the nested list
            EvalRef elemHeadRef
          _ -> do
            -- Element is a primitive value
            jsonStr <- encodeValueToJson val
            PushFrame (JsonEncodeListFrame (jsonStr : acc) tailRef True)
            EvalRef tailRef
  Just (JsonEncodeNestedFrame acc tailRef) -> do
    -- We just finished encoding a nested list, val should be a ValString with the JSON
    case val of
      ValString nestedJson -> do
        -- Add the nested JSON to accumulator and continue with the tail
        PushFrame (JsonEncodeListFrame (nestedJson : acc) tailRef True)
        EvalRef tailRef
      _ ->
        -- Should not happen - nested list encoding should return ValString
        InternalException $ RuntimeTypeError "Expected ValString from nested list encoding"
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
            Backward $ ValString jsonObject
          ((nextFieldName, nextFieldRef):rest) -> do
            -- More fields to encode
            PushFrame (JsonEncodeConstructorFrame newAcc nextFieldName rest)
            PushFrame (UnaryBuiltin0 UnaryJsonEncode Nothing)
            EvalRef nextFieldRef
      _ ->
        -- Should not happen - field encoding should return ValString
        InternalException $ RuntimeTypeError "Expected ValString from field encoding"
  Just (UpdateThunk rf) -> do
    updateThunkToWHNF rf val
    Backward val
  Just (ContractFrame cFrame) -> backwardContractFrame val cFrame

backwardContractFrame :: Value Reference -> ContractFrame -> Machine Config
backwardContractFrame val = \ case
  Contract1 ScrutinizeEvents {..} -> do
    case val of
      ValCons e es -> do
        pushCFrame (Contract2 ScrutinizeEvent {events = es, ..})
        EvalRef e
      ValNil -> Backward (ValObligation env party act due followup lest)
      _ -> InternalException $ RuntimeTypeError $
        "expected LIST EVENT but found: " <> prettyLayout val <> " when scrutinizing regulative events"
  Contract2 ScrutinizeEvent {..} -> case val of
    ValEvent ev'party ev'act ev'time -> do
      pushCFrame (Contract3 CurrentTimeWHNF {..})
      EvalRef ev'time
    _ -> InternalException $ RuntimeTypeError $
      "expected an EVENT but found: " <> prettyLayout val <> " when scrutinizing a regulative event"
  Contract3 CurrentTimeWHNF {..} -> do
    pushCFrame (Contract4 ScrutinizeDue {ev'time = val, ..})
    EvalRef time
  Contract4 ScrutinizeDue {..} -> do
    case due of
       Right due' -> do
         pushCFrame (Contract5 CheckTiming {time = val, ..})
         Backward due'
       Left (Just due') -> do
         pushCFrame (Contract5 CheckTiming {time = val,..})
         ForwardExpr env due'
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
      -- NOTE: the deadline has passed. there are now two options:
      -- 1. there's no lest clause, i.e. this is an obligation. We return a breach.
      -- 2. there's a lest clause, i.e. this is an external choice. We continue by reducing
      --    the lest clause
      then case lest of
        Nothing -> do
          -- NOTE: this is not too nice, but not wanting this would require to change `App1` to take MaybeEvaluated's
          partyR <- either (`allocate_` env) AllocateValue party
          Backward (ValBreached (DeadlineMissed ev'party ev'act stamp partyR act deadline))
        Just lestFollowup -> AllocateValue ev'time
          >>= continueWithFollowup env lestFollowup events
      else do
        -- NOTE: we have observed the event and do not branch, either, the
        -- only thing that may now happen is that we try a new event. Hence we
        -- drop the ev'time, set our time to ev'time and set our due to the new due
        pushCFrame (Contract6 PartyWHNF {time = ev'time, due = Right $ ValNumber newDue, ..})
        maybeEvaluate env party
  Contract6 PartyWHNF {..} -> do
    pushCFrame (Contract7 PartyEqual {party = val, ..})
    EvalRef ev'party
  Contract7 PartyEqual {..} -> do
    pushCFrame (Contract8 ScrutinizeParty {ev'party = val, ..})
    runBinOpEquals party val
  Contract8 ScrutinizeParty {..} ->
    case val of
      ValBool True -> do
        pushCFrame (Contract11 (ActionDoesn'tmatch {..}))
        pushCFrame (Contract9 ScrutinizeEnvironment {..})
        MatchPattern ev'act env act.action
      ValBool False -> do
        newTime <- AllocateValue time
        tryNextEvent ScrutinizeEvents {party = Right party, time = newTime, ..} events
      _ -> InternalException $ RuntimeTypeError $
        "expected BOOLEAN but found: " <> prettyLayout val
  Contract11 ActionDoesn'tmatch {} ->
    -- NOTE: this is a "guard frame" which only matters if we're unwinding the stack after a pattern match failure
    Backward val
  Contract9 ScrutinizeEnvironment {..} ->
    case val of
      ValEnvironment henceEnv -> do
        pushCFrame $ Contract10 ScrutinizeActions {..}
        ForwardExpr (env `Map.union` henceEnv) (fromMaybe trueExpr act.provided)
      _ -> InternalException $ RuntimeTypeError $
        "expected environment but found: " <> prettyLayout val
  Contract10 ScrutinizeActions {..} ->
    case val of
      ValBool True -> AllocateValue time
        >>= continueWithFollowup (env `Map.union` henceEnv) followup events
      ValBool False -> do
        newTime <- AllocateValue time
        tryNextEvent ScrutinizeEvents {party = Right party, time = newTime, ..} events
      _ -> InternalException $ RuntimeTypeError $
        "expected BOOLEAN but found: " <> prettyLayout val
  RBinOp1 MkRBinOp1 {..}
    -- NOTE: this is weirdly asymmetric because
    -- in case of AND we can never abort earlier but have to instead
    -- wait for the left hand side expression to run to observe
    -- how we'll have to do the blame assignment
    | ValROr <- op
    , ValFulfilled <- val -> Backward ValFulfilled

  RBinOp1 MkRBinOp1 {..} -> do

    -- push a frame for when the evaluation of the
    -- second argument has completed
    pushCFrame $ RBinOp2 MkRBinOp2 {rval1 = val, ..}
    -- pass the arguments to the regulative expression
    PushFrame $ App1 args Nothing
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
      Backward
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
    -> Backward ValFulfilled

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
    -> Backward (ValBreached reason)
  RBinOp2 MkRBinOp2 {..}
    | ValRAnd <- op
    , ValBreached reason <- val
    -> Backward (ValBreached reason)

  -- OR
  RBinOp2 MkRBinOp2 {..}
    | ValROr <- op
    , ValFulfilled <- val
    -> Backward ValFulfilled
  RBinOp2 MkRBinOp2 {..}
    | ValROr <- op
    , ValFulfilled <- rval1
    -> Backward ValFulfilled


  -- NOTE: otherwise, we do not have enough information to do
  -- any reduction of the contract clauses and thus have to return
  -- a value that represents the operator applied to each operand
  RBinOp2 MkRBinOp2 {..} ->
    Backward (ValROp env op (Right rval1) (Right val))
  where
    tryNextEvent :: ScrutinizeEvents -> Reference -> Machine Config
    tryNextEvent frame events = do
      pushCFrame (Contract1 frame)
      EvalRef events

    pushCFrame = PushFrame . ContractFrame

    continueWithFollowup :: Environment -> RExpr -> Reference -> Reference -> Machine Config
    continueWithFollowup env followup events time = do
      PushFrame (App1 [time, events] Nothing)
      ForwardExpr env followup

    assertTime = \ case
      ValNumber i -> pure i
      v -> InternalException $ RuntimeTypeError $
        "expected a NUMBER but got: " <> prettyLayout v

maybeEvaluate :: Environment -> MaybeEvaluated -> Machine Config
maybeEvaluate env = either (ForwardExpr env) Backward

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
      PushFrame f -- provides better error context
      InternalException $
        RuntimeTypeError "given signatures' values' lengths do not match"

matchBranches :: Reference -> Environment -> [Branch Resolved] -> Machine Config
matchBranches scrutinee _env [] =
  UserException (NonExhaustivePatterns scrutinee)
matchBranches _scrutinee env (MkBranch _ann (Otherwise _ann') e : _) =
  ForwardExpr env e
matchBranches scrutinee env (MkBranch _ann (When _ann' pat) e : branches) = do
  PushFrame (ConsiderWhen1 scrutinee e branches env)
  MatchPattern scrutinee env pat

matchPattern :: Reference -> Environment -> Pattern Resolved -> Machine Config
matchPattern scrutinee _env (PatVar _ann n) = do
  Backward (ValEnvironment (Map.singleton (getUnique n) scrutinee))
matchPattern scrutinee _env (PatApp _ann n [])
  | getUnique n == TypeCheck.emptyUnique = do -- pattern for the empty list
  PushFrame PatNil0
  EvalRef scrutinee
matchPattern scrutinee env (PatCons _ann p1 p2) = do
  PushFrame (PatCons0 p1 env p2 )
  EvalRef scrutinee
matchPattern scrutinee env (PatApp _ann n ps) = do
  PushFrame (PatApp0 n env ps)
  EvalRef scrutinee
matchPattern scrutinee env (PatExpr _ann expr) = do
  PushFrame (PatLit0 env expr)
  EvalRef scrutinee
matchPattern scrutinee _env (PatLit _ann lit) = do
  PushFrame $ PatLit1 case lit of
    NumericLit _ n -> ValNumber n
    StringLit _ s -> ValString  s
  EvalRef scrutinee

-- | This unwinds the stack until it finds the enclosing pattern match and then resumes.
patternMatchFailure :: Machine Config
patternMatchFailure = WithPoppedFrame $ \ case
  Nothing ->
    InternalException UnhandledPatternMatch
  Just (ConsiderWhen1 scrutinee _ branches env) ->
    MatchBranches scrutinee env branches
  -- we have unwound the frame that would reenter when scrutinizing the event
  Just (ContractFrame (Contract11 ActionDoesn'tmatch {..})) -> do
    newTime <- AllocateValue time
    PushFrame $ ContractFrame $ Contract1 ScrutinizeEvents {party = Right party, time = newTime, ..}
    EvalRef events
  Just _ ->
    patternMatchFailure

runLit :: Lit -> Machine WHNF
runLit (NumericLit _ann num) = pure (ValNumber num)
runLit (StringLit _ann str)  = pure (ValString str)

expect1 :: [a] -> Machine a
expect1 = \ case
  [x] -> pure x
  xs -> InternalException (RuntimeTypeError $ "Expected 1 argument, but got " <> Text.show (length xs))

expect2 :: [a] -> Machine (a, a)
expect2 = \ case
  [x, y] -> pure (x, y)
  xs -> InternalException (RuntimeTypeError $ "Expected 1 argument, but got " <> Text.show (length xs))

expectNumber :: WHNF -> Machine Rational
expectNumber = \ case
  ValNumber f -> pure f
  _ -> InternalException (RuntimeTypeError "Expected number.")

expectString :: WHNF -> Machine Text
expectString = \ case
  ValString f -> pure f
  _ -> InternalException (RuntimeTypeError "Expected string.")

expectInteger :: BinOp -> Rational -> Machine Integer
expectInteger op n = do
  case isInteger n of
    Nothing -> UserException (NotAnInteger op n)
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
    InternalException $ RuntimeTypeError "Internal error: ValCons should be handled by frame-based evaluation in runBuiltin"
  ValConstructor conRef []
    | nameToText (TypeCheck.getName conRef) == "NOTHING" -> pure "null"
    | nameToText (TypeCheck.getName conRef) == "TRUE" -> pure "true"
    | nameToText (TypeCheck.getName conRef) == "FALSE" -> pure "false"
  -- Note: For constructors with fields, we can't encode them directly within encodeValueToJson
  -- because we need to evaluate (force) each field reference. This requires frames.
  -- So constructors are handled in runBuiltin where we can push frames.
  ValConstructor conRef _fields -> do
    InternalException $ RuntimeTypeError $
      "Internal error: Constructor encoding should be handled in runBuiltin, not encodeValueToJson: " <>
      nameToText (TypeCheck.getName conRef)
  val -> InternalException $ RuntimeTypeError $ "Cannot encode value to JSON: " <> prettyLayout val
  where
    escapeJson :: Text -> Text
    escapeJson = Text.concatMap \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      c -> Text.singleton c

-- | Decode JSON string to L4 value wrapped in MAYBE
-- Returns JUST value on success, NOTHING on parse error
-- | Type-directed JSON decoding - uses type information to construct proper records
decodeJsonToValueTyped :: Text -> Type' Resolved -> Machine WHNF
decodeJsonToValueTyped jsonStr ty = do
  case Aeson.eitherDecodeStrict' (TE.encodeUtf8 jsonStr) of
    Left _err -> do
      -- Parse error: return NOTHING
      pure $ ValConstructor TypeCheck.nothingRef []
    Right jsonValue -> do
      -- Parse success: convert to L4 value using type information and wrap in JUST
      l4Value <- jsonValueToWHNFTyped jsonValue ty
      justVal <- AllocateValue l4Value
      pure $ ValConstructor TypeCheck.justRef [justVal]

-- | Convert Aeson Value to L4 WHNF using type information
jsonValueToWHNFTyped :: Aeson.Value -> Type' Resolved -> Machine WHNF
jsonValueToWHNFTyped jsonValue ty = case ty of
  -- Handle record types: TyApp conRef []
  TyApp _anno conRef [] -> do
    -- This is a nullary type application, likely a record constructor
    entityInfo <- GetEntityInfo
    case Map.lookup (getUnique conRef) entityInfo of
      Nothing -> do
        -- Not a record, fall back to generic decoding
        jsonValueToWHNF jsonValue
      Just (_name, checkEntity) -> case checkEntity of
        TypeCheck.KnownTerm conType Constructor -> do
          -- This is a constructor, extract field names
          fieldNames <- extractFieldNames conType
          case jsonValue of
            Aeson.Object obj -> do
              -- Decode each field from the JSON object
              -- Note: We ignore extra fields in the JSON (Postel's Law)
              fieldRefs <- forM fieldNames $ \fieldName -> do
                case KeyMap.lookup (Key.fromText fieldName) obj of
                  Nothing -> do
                    -- Field missing in JSON, this is an error
                    InternalException $ RuntimeTypeError $
                      "Missing required field '" <> fieldName <> "' in JSON object"
                  Just fieldValue -> do
                    -- Decode the field value (recursive, but without type info for now)
                    fieldWHNF <- jsonValueToWHNF fieldValue
                    AllocateValue fieldWHNF
              -- Construct the record with the decoded fields
              pure $ ValConstructor conRef fieldRefs
            _ -> do
              -- JSON value is not an object, can't decode to record
              InternalException $ RuntimeTypeError $
                "Expected JSON object to decode to record type, but got: " <> Text.pack (show jsonValue)
        _ -> do
          -- Not a constructor, fall back to generic decoding
          jsonValueToWHNF jsonValue
  -- For other types, fall back to generic decoding
  _ -> jsonValueToWHNF jsonValue

decodeJsonToValue :: Text -> Machine WHNF
decodeJsonToValue jsonStr = do
  case Aeson.eitherDecodeStrict' (TE.encodeUtf8 jsonStr) of
    Left _err -> do
      -- Parse error: return NOTHING
      pure $ ValConstructor TypeCheck.nothingRef []
    Right jsonValue -> do
      -- Parse success: convert to L4 value and wrap in JUST
      l4Value <- jsonValueToWHNF jsonValue
      justVal <- AllocateValue l4Value
      pure $ ValConstructor TypeCheck.justRef [justVal]

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
  headRef <- AllocateValue headVal
  tailVal <- jsonListToWHNF xs
  tailRef <- AllocateValue tailVal
  pure $ ValCons headRef tailRef

runPost :: WHNF -> WHNF -> WHNF -> Machine Config
runPost urlVal headersVal bodyVal = do
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
      Backward $ ValString (TE.decodeUtf8 . LBS.toStrict $ Req.responseBody res)
    _ -> InternalException (RuntimeTypeError "POST only supports https")

runConcat :: [WHNF] -> Machine Config
runConcat vals = do
  strings <- traverse expectString vals
  Backward $ ValString (Text.concat strings)

runAsString :: WHNF -> Machine Config
runAsString val = do
  case val of
    ValNumber n ->
      -- Convert number to string
      -- If it's an integer, show without decimal point
      -- Otherwise, show as decimal
      let str = if denominator n == 1
                then Text.pack $ show (numerator n)
                else Text.pack $ show (fromRational n :: Double)
      in Backward $ ValString str
    ValString s ->
      -- Already a string, just return it
      Backward $ ValString s
    _ -> InternalException $ RuntimeTypeError $
      "AS STRING can only convert NUMBER or STRING to STRING, but found: " <> prettyLayout val

runBuiltin :: WHNF -> UnaryBuiltinFun -> Maybe (Type' Resolved) -> Machine Config
runBuiltin es op mTy = do
  case op of
    UnaryJsonEncode -> do
      case es of
        ValCons headRef tailRef -> do
          -- Start frame-based evaluation for non-empty lists
          -- We're about to evaluate the head element (expectingTail = False)
          PushFrame (JsonEncodeListFrame [] tailRef False)
          EvalRef headRef
        ValNil -> do
          -- Empty list is simple
          Backward $ ValString "[]"
        ValConstructor conRef []
          | nameToText (TypeCheck.getName conRef) == "NOTHING" ->
            -- NOTHING encodes to null
            Backward $ ValString "null"
          | nameToText (TypeCheck.getName conRef) == "TRUE" ->
            -- TRUE encodes to true
            Backward $ ValString "true"
          | nameToText (TypeCheck.getName conRef) == "FALSE" ->
            -- FALSE encodes to false
            Backward $ ValString "false"
        ValConstructor conRef [field]
          | nameToText (TypeCheck.getName conRef) == "JUST" -> do
            -- JUST wraps a single value, evaluate it and encode
            PushFrame (UnaryBuiltin0 UnaryJsonEncode Nothing)
            EvalRef field
        ValConstructor conRef fields -> do
          -- Encode record constructors as JSON objects with field names
          entityInfo <- GetEntityInfo
          case Map.lookup (getUnique conRef) entityInfo of
            Nothing ->
              InternalException $ RuntimeTypeError $ "Cannot find constructor in entity info: " <> nameToText (TypeCheck.getName conRef)
            Just (_name, checkEntity) -> case checkEntity of
              TypeCheck.KnownTerm conType Constructor -> do
                -- Extract field names from the constructor's function type
                fieldNames <- extractFieldNames conType
                if length fieldNames /= length fields
                  then InternalException $ RuntimeTypeError $
                    "Field count mismatch for constructor " <> nameToText (TypeCheck.getName conRef) <>
                    ": expected " <> Text.pack (show (length fieldNames)) <>
                    " but got " <> Text.pack (show (length fields))
                  else
                    let fieldPairs = zip fieldNames fields
                    in case fieldPairs of
                      [] ->
                        -- Nullary constructor (no fields)
                        Backward $ ValString "{}"
                      ((fn, fr):rest) -> do
                        -- Start frame-based encoding of fields
                        -- The frame stores: accumulated pairs, current field name being encoded, remaining pairs
                        PushFrame (JsonEncodeConstructorFrame [] fn rest)
                        -- Push frame to encode the first field value
                        PushFrame (UnaryBuiltin0 UnaryJsonEncode Nothing)
                        -- Evaluate the first field
                        EvalRef fr
              _ ->
                InternalException $ RuntimeTypeError $
                  "Expected constructor term but got different entity type for: " <> nameToText (TypeCheck.getName conRef)
        _ -> do
          -- For non-list, non-constructor values, use direct encoding
          jsonStr <- encodeValueToJson es
          Backward $ ValString jsonStr
    UnaryJsonDecode -> do
      jsonStr <- expectString es
      result <- case mTy of
        Just ty -> decodeJsonToValueTyped jsonStr ty
        Nothing -> decodeJsonToValue jsonStr
      Backward result
    UnaryFetch -> do
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
          Backward $ ValString (TE.decodeUtf8 . LBS.toStrict $ Req.responseBody res)
        _ -> InternalException (RuntimeTypeError "FETCH only supports https")
    _ -> do
      val :: Rational <- expectNumber es
      Backward case op of
        UnaryIsInteger -> valBool $ isJust $ isInteger val
        UnaryRound -> valInt $ round val
        UnaryCeiling -> valInt $ ceiling val
        UnaryFloor -> valInt $ floor val
        UnaryPercent -> ValNumber (val / 100)
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
runBinOp _op         (ValAssumed r) _e2                          = StuckOnAssumed r
runBinOp _op         _e1 (ValAssumed r)                          = StuckOnAssumed r
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
        [] -> Backward $ ValBool True
        ((r1, r2) : rss) -> do
          PushFrame (EqConstructor1 r2 rss)
          EvalRef r1
  | otherwise                                           = Backward $ ValBool False
-- TODO: we probably also want to check ValObligations for equality
runBinOpEquals (ValAssumed r)          _                = StuckOnAssumed r
runBinOpEquals v1                       v2              = UserException (EqualityOnUnsupportedType v1 v2)

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
  u <- NewUnique
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
  join $ PokeThunk rf \tid -> \ case
    thunk@(WHNF val) -> (thunk, Backward val)
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
  pure [MkEvalDirective (rangeOf ann) False False expr env]
evalDirective env (LazyEvalTrace ann expr) =
  pure [MkEvalDirective (rangeOf ann) True False expr env]
evalDirective _env (Check _ann _expr) =
  pure []
evalDirective env (Contract ann expr t evs) =
  evalDirective env . LazyEval ann =<< contractToEvalDirective expr t evs
evalDirective env (Assert ann expr) =
  pure [MkEvalDirective (rangeOf ann) False True expr env]

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

prettyEvalException :: EvalException -> [Text]
prettyEvalException (InternalEvalException exc) = wrapInternal (prettyInternalEvalException exc)
  where
    wrapInternal :: [Text] -> [Text]
    wrapInternal msgs = [ "Internal error:" ] <> msgs <> [ "Please report this as a bug." ]
prettyEvalException (UserEvalException exc)     = prettyUserEvalException exc

prettyInternalEvalException :: InternalEvalException -> [Text]
prettyInternalEvalException = \ case
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
prettyUserEvalException = \ case
  BlackholeForced expr ->
    [ "Infinite loop detected while trying to evaluate:"
    , prettyLayout expr ]
  EqualityOnUnsupportedType v1 v2 ->
    [ "Trying to check equality on types that do not support it"
    , "These were the values you tried to compare:" ]
    <> indentMany v1
    <> indentMany v2
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
  nothingRef <- AllocateValue (ValConstructor TypeCheck.nothingRef [])
  justRef <- AllocateValue (ValUnappliedConstructor TypeCheck.justRef)
  evalContractRef <- AllocateValue =<< evalContractVal
  eventCRef <- AllocateValue eventCVal
  isIntegerRef <- AllocateValue (ValUnaryBuiltinFun UnaryIsInteger)
  roundRef <- AllocateValue (ValUnaryBuiltinFun UnaryRound)
  ceilingRef <- AllocateValue (ValUnaryBuiltinFun UnaryCeiling)
  floorRef <- AllocateValue (ValUnaryBuiltinFun UnaryFloor)
  fetchRef <- AllocateValue (ValUnaryBuiltinFun UnaryFetch)
  jsonEncodeRef <- AllocateValue (ValUnaryBuiltinFun UnaryJsonEncode)
  jsonDecodeRef <- AllocateValue (ValUnaryBuiltinFun UnaryJsonDecode)
  fulfilRef <- AllocateValue ValFulfilled
  neverMatchesPartyRef <- AllocateValue ValNeverMatchesParty
  neverMatchesActRef <- AllocateValue ValNeverMatchesAct
  waitUntilRef <- AllocateValue =<< waitUntilVal eventCRef neverMatchesPartyRef neverMatchesActRef
  andRef <- AllocateValue =<< andValClosure trueRef falseRef
  orRef <- AllocateValue =<< orValClosure trueRef falseRef
  impliesRef <- AllocateValue =<< impliesValClosure trueRef falseRef
  notRef <- AllocateValue =<< notValClosure trueRef falseRef

  builtinBinOpRefs <-
    traverse
      (\(funVal, uniq) -> do
        r <- AllocateValue $ ValBinaryBuiltinFun funVal
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
      , (TypeCheck.evalContractUnique, evalContractRef)
      , (TypeCheck.eventCUnique, eventCRef)
      , (TypeCheck.fulfilUnique, fulfilRef)
      , (TypeCheck.isIntegerUnique, isIntegerRef)
      , (TypeCheck.roundUnique, roundRef)
      , (TypeCheck.ceilingUnique, ceilingRef)
      , (TypeCheck.floorUnique, floorRef)
      , (TypeCheck.fetchUnique, fetchRef)
      , (TypeCheck.jsonEncodeUnique, jsonEncodeRef)
      , (TypeCheck.jsonDecodeUnique, jsonDecodeRef)
      , (TypeCheck.waitUntilUnique, waitUntilRef)
      , (TypeCheck.andUnique, andRef)
      , (TypeCheck.orUnique, orRef)
      , (TypeCheck.impliesUnique, impliesRef)
      , (TypeCheck.notUnique, notRef)
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
      , (BinOpCons,      [TypeCheck.consUnique])
      , (BinOpEquals,    [TypeCheck.equalsUnique])
      ]
  , unique <- uniques
  ]

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
