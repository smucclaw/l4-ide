import { Corefs } from './environment'
import type { BoolVar, Unique, Name } from '@repo/viz-expr'
import {
  type Value,
  type BoolVal,
  type Expr,
  TrueVal,
  FalseVal,
  UnknownVal,
  isUnknownVal,
} from './type.js'
import { Subst } from './environment.js'
import { LirContext } from '../layout-ir/core.js'
import type { LirId } from '../layout-ir/core.js'
import type {
  LadderGraphLirNode,
  VarLirNode,
} from '../layout-ir/ladder-lir.svelte.js'
import type { DirectedAcyclicGraph } from '$lib/algebraic-graphs/dag'

/**********************************************************
                   Evaluator
**********************************************************/

/* The Evaluator should NOT set values on the LirNodes. It can update the env, but not the LirNodes themselves.

We can think of the what-if style eval as
a normal evaluator that's evaluating

  (App (FunV params body fenv) args)

where the closure is that of a boolean function whose params correspond to the leaf LadderGraph nodes,
args is Map<Unique, BoolValues>,
and the set of Unique keys in args is a subset of the set of params
(this is a proper subset when not all the BoolVarNodes have a value)

How would the evaluation go?

Start by extending our environment with the Map<Unique, BoolValues> args.

* Suppose we have a NNF.
* Get all the paths
* To eval a path, look up the value of each non-bundling-node in the Env; see if all of the non-bundling-nodes eval to True.

OK now the question is:

what should the signature of the eval function be?

*/

export interface Evaluator {
  eval(context: LirContext, expr: Expr): Value
}

export const SimpleEvaluator: Evaluator = {
  /** An UnknownVal result means there wasn't enough info */
  eval(context: LirContext, expr: Expr): Value {
    return eval_(context, expr, new Subst([]))
  },
}

function eval_(context: LirContext, expr: Expr, env: Subst): Value {}
