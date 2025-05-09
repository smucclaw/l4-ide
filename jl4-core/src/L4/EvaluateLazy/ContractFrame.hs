module L4.EvaluateLazy.ContractFrame where

import L4.Evaluate.ValueLazy

data ContractFrame
  = Contract1 ScrutEvents
  -- ^ if elements are left in the list of the events
  -- - continue by evaluating that event else
  -- - abort with a contract breach
  | Contract2 ScrutEvent
  -- ^ scrutinizes the event to extract references for
  -- party, action and time stamp of the event, continue
  -- by evaluting the party of the obligation statement
  | Contract3 PartyWHNF
  -- ^ evaluates the party of the event
  | Contract3' PartyEqual
  -- ^ evaluates the equality between parties
  | Contract4 ScrutParty
  -- ^ checks if the party of the event matches
  -- - if yes, continue by evaluating the action of the contract
  -- - if no, continue with the next event
  | Contract5 ActWHNF
  -- ^ evaluates the act of the event
  | Contract5' ActEqual
  -- ^ evaluates the equality between acts
  | Contract6 ScrutAct
  -- ^ checks if the act of the event matches
  -- - if yes, and
  --   - there's a due date: continue by evluating the due of the contract
  --   - there's no due date: continue with the followup contract
  -- - if no, continue with the next event
  | Contract7 StampWHNF
  -- ^ evaluates the stamp of the event
  | Contract8 CurTimeWHNF
  -- ^ evaluates the current time of the evaluation
  | Contract9 ScrutTime
  -- ^ scrutinizes the current time, the timestamp of the event and the contract
  -- - if the timestamp of the contract is after the current time plus the time
  --   the event duration time, the contract is breached
  -- - if the timestamp is within the due time
  --   - advance time
  --   - continue with followup event
  deriving stock Show

data ScrutEvents = ScrutEvents
  { party :: MaybeEvaluated, act :: MaybeEvaluated, due :: Maybe RExpr, followup :: RExpr
  , time :: Reference
  , env :: Environment
  }
  deriving stock Show

data ScrutEvent = ScrutEvent
  { party :: MaybeEvaluated, act :: MaybeEvaluated, due :: Maybe RExpr, followup :: RExpr
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data PartyWHNF = PartyWHNF
  { act :: MaybeEvaluated, due :: Maybe RExpr, followup :: RExpr
  , ev'party :: Reference, ev'act :: Reference, ev'time :: Reference
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data PartyEqual = PartyEqual
  { party :: WHNF, act :: MaybeEvaluated, due :: Maybe RExpr, followup :: RExpr
  , ev'party :: Reference, ev'act :: Reference, ev'time :: Reference
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data ScrutParty = ScrutParty
  { party :: WHNF, act :: MaybeEvaluated, due :: Maybe RExpr, followup :: RExpr
  , ev'party :: WHNF, ev'act :: Reference, ev'time :: Reference
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data ActWHNF = ActWHNF
  { party :: WHNF, due :: Maybe RExpr, followup :: RExpr
  , ev'party :: WHNF, ev'act :: Reference, ev'time :: Reference
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data ActEqual = ActEqual
  { party :: WHNF, act :: WHNF, due :: Maybe RExpr, followup :: RExpr
  , ev'party :: WHNF, ev'act :: Reference, ev'time :: Reference
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show


data ScrutAct = ScrutAct
  { party :: WHNF, act :: WHNF, due :: Maybe RExpr, followup :: RExpr
  , ev'party :: WHNF, ev'act :: WHNF, ev'time :: Reference
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data StampWHNF = StampWHNF
  { followup :: RExpr
  , ev'party :: WHNF, ev'act :: WHNF, ev'time :: Reference
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data CurTimeWHNF = CurTimeWHNF
  { due :: WHNF, followup :: RExpr
  , ev'party :: WHNF, ev'act :: WHNF
  , events :: Reference, time :: Reference
  , env :: Environment
  }
  deriving stock Show

data ScrutTime = ScrutTime
  { due :: WHNF, followup :: RExpr
  , ev'party :: WHNF, ev'act :: WHNF, ev'time :: WHNF
  , events :: Reference
  , env :: Environment
  }
  deriving stock Show
