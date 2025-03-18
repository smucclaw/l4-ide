module L4.Evaluate.Value where

import Base
import L4.Syntax

type Environment = Map Unique Value

data Value =
    ValNumber Int -- for now
  | ValString Text
  | ValList [Value]
  | ValClosure (GivenSig Resolved) (Expr Resolved) Environment
  | ValUnappliedConstructor Resolved
  | ValConstructor Resolved [Value]
  | ValAssumed Resolved
  -- | ValEnvironment Environment

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
  -- rnf (ValEnvironment env)        = env `seq` ()

