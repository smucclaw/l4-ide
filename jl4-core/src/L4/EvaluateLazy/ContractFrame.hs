module L4.EvaluateLazy.ContractFrame where

import L4.Evaluate.ValueLazy

data ContractFrame
  = Contract1 C1Frame -- event list to whnf
  | Contract2 C2Frame -- event to whnf
  | Contract3 C3Frame -- party to whnf
  | Contract4 C4Frame -- party from event to whnf
  | Contract5 C5Frame -- action to whnf
  | Contract6 C6Frame -- action to whnf
  | Contract7 C7Frame -- due to whnf
  | Contract8 C8Frame -- due to whnf
  | Contract9 C9Frame -- due to whnf
  deriving stock Show

data C1Frame = C1Frame
  { party :: MaybeEvaluated, act :: MaybeEvaluated, due :: Maybe Reference, followup :: Reference
  , time :: Reference
  }
  deriving stock Show

data C2Frame = C2Frame
  { party :: MaybeEvaluated, act :: MaybeEvaluated, due :: Maybe Reference, followup :: Reference
  , events :: Reference, time :: Reference
  }
  deriving stock Show

data C3Frame = C3Frame
  { act :: MaybeEvaluated, due :: Maybe Reference, followup :: Reference
  , ev'party :: Reference, ev'act :: Reference, ev'time :: Reference
  , events :: Reference, time :: Reference
  }
  deriving stock Show

data C4Frame = C4Frame
  { party :: WHNF, act :: MaybeEvaluated, due :: Maybe Reference, followup :: Reference
  , ev'act :: Reference, ev'time :: Reference
  , events :: Reference, time :: Reference
  }
  deriving stock Show

data C5Frame = C5Frame
  { party :: WHNF, due :: Maybe Reference, followup :: Reference
  , ev'party :: WHNF, ev'act :: Reference, ev'time :: Reference
  , events :: Reference, time :: Reference
  }
  deriving stock Show

data C6Frame = C6Frame
  { party :: WHNF, act :: WHNF, due :: Maybe Reference, followup :: Reference
  , ev'party :: WHNF, ev'time :: Reference
  , events :: Reference, time :: Reference
  }
  deriving stock Show

data C7Frame = C7Frame
  { followup :: Reference
  , ev'party :: WHNF, ev'act :: WHNF, ev'time :: Reference
  , events :: Reference, time :: Reference
  }
  deriving stock Show

data C8Frame = C8Frame
  { due :: WHNF, followup :: Reference
  , ev'party :: WHNF, ev'act :: WHNF
  , events :: Reference, time :: Reference
  }
  deriving stock Show

data C9Frame = C9Frame
  { due :: WHNF, followup :: Reference
  , ev'party :: WHNF, ev'act :: WHNF, ev'time :: WHNF
  , events :: Reference
  }
  deriving stock Show

type MaybeEvaluated = Either WHNF Reference
