module L4.EvaluateLazy.ContractFrame where

import L4.Evaluate.ValueLazy

data ContractFrame
  = Contract1 ScrutinizeEvents
  -- ^ if elements are left in the list of the events
  -- - continue by evaluating that event else
  -- - abort with a contract breach
  | Contract2 ScrutinizeEvent
  -- ^ scrutinizes the event to extract references for
  -- party, action and timestamp of the event, continue
  -- by evaluting the time of the event
  | Contract3 CurrentTimeWHNF
  -- ^ continue by evaluating the current time
  | Contract4 ScrutinizeDue
  -- ^ checks if there's a due time, if that's the case, continue by checking
  -- timing constraints, if not, then skip the timing and go straight to checking
  -- the party
  | Contract5 CheckTiming
  -- ^ scrutinizes the current time, the timestamp of the event and the due time
  -- We check if the event happens within the due time, if that's the case, we continue
  -- by checking the party and evaluting the obligation's party argument.
  -- If that's not the case, then there are two options
  -- 1. If there's a lest clause, then go on by evaluting the lest clause
  -- 2. If there's no lest clause, we attach all the information we have
  --    to a breach and return that instead
  | Contract6 PartyWHNF
  -- ^ continue by evaluating the event's party
  | Contract7 PartyEqual
  -- ^ evaluates the equality between parties
  | Contract8 ScrutinizeParty
  -- ^ checks if the party of the event matches
  -- - if yes, continue by evaluating the action of the contract
  -- - if no, continue with the next event
  | Contract9 ActionWHNF
  -- ^ continue by evlauting the event's action
  | Contract10 ActionsEqual
  -- ^ continue by evaluating equality between the actions
  | Contract11 ScrutinizeActions
  -- ^ checks if the actions of the event matches the one of the obligation
  -- - if no, continue with the next event
  | RBinOp1 RBinOp1
  -- ^ Regulative BinOp frame while evaluating a regulative expression
  | RBinOp2 RBinOp2
  -- ^ Regulative BinOp frame while evaluating the second expression of a bin op
  deriving stock Show

data ScrutinizeEvents = ScrutinizeEvents
  { party :: MaybeEvaluated, act :: MaybeEvaluated, due :: MaybeEvaluated'  (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , time :: Reference
  , env :: Environment
  }
  deriving stock Show

data ScrutinizeEvent = ScrutinizeEvent
  { party :: MaybeEvaluated, act :: MaybeEvaluated, due :: MaybeEvaluated' (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data CurrentTimeWHNF = CurrentTimeWHNF
  { party :: MaybeEvaluated, act :: MaybeEvaluated, due :: MaybeEvaluated' (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , ev'party :: Reference, ev'act :: Reference, ev'time :: Reference
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data ScrutinizeDue = ScrutinizeDue
  { party :: MaybeEvaluated, act :: MaybeEvaluated, due :: MaybeEvaluated' (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , ev'party :: Reference, ev'act :: Reference, ev'time :: WHNF
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data CheckTiming = CheckTiming
  { party :: MaybeEvaluated, act :: MaybeEvaluated, due :: MaybeEvaluated' (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , ev'party :: Reference, ev'act :: Reference, ev'time :: WHNF
  , events :: Reference, time :: WHNF
  , env :: Environment
  }
  deriving stock Show

data PartyWHNF = PartyWHNF
  { act :: MaybeEvaluated, due :: MaybeEvaluated' (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , ev'party :: Reference, ev'act :: Reference
  , events :: Reference, time :: WHNF
  , env :: Environment
  }
  deriving stock Show

data PartyEqual = PartyEqual
  { party :: WHNF, act :: MaybeEvaluated, due :: MaybeEvaluated' (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , ev'party :: Reference, ev'act :: Reference
  , events :: Reference, time :: WHNF
  , env :: Environment
  }
  deriving stock Show

data ScrutinizeParty = ScrutinizeParty
  { party :: WHNF, act :: MaybeEvaluated, due :: MaybeEvaluated' (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , ev'party :: WHNF, ev'act :: Reference
  , events :: Reference, time :: WHNF
  , env :: Environment
  }
  deriving stock Show

data ActionWHNF = ActionWHNF
  { party :: WHNF, due :: MaybeEvaluated' (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , ev'party :: WHNF, ev'act :: Reference
  , events :: Reference, time :: WHNF
  , env :: Environment
  }
  deriving stock Show

data ActionsEqual = ActionsEqual
  { party :: WHNF, act :: WHNF, due :: MaybeEvaluated' (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , ev'party :: WHNF, ev'act :: Reference
  , events :: Reference, time :: WHNF
  , env :: Environment
  }
  deriving stock Show

data ScrutinizeActions = ScrutinizeActions
  { party :: WHNF, act :: WHNF, due :: MaybeEvaluated' (Maybe RExpr), followup :: RExpr, lest :: Maybe RExpr
  , ev'party :: WHNF, ev'act :: WHNF
  , events :: Reference, time :: WHNF
  , env :: Environment
  }
  deriving stock Show

data RBinOp1 = MkRBinOp1
  { op :: RBinOp
  , rexpr2 :: MaybeEvaluated
  , args :: [Reference] -- ^ the arguments to the remaining contract expr
  , env :: Environment
  }
  deriving stock Show

data RBinOp2 = MkRBinOp2
  { op :: RBinOp
  , rval1 :: WHNF
  , env :: Environment
  }
  deriving stock Show
