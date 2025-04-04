import type { Unique } from '@repo/viz-expr'
import type {
  Value,
  BoolVal,
  BoolLit,
  Lam,
  App,
  LadderGraphExpr,
  Expr,
} from './type.js'
import {
  TrueVal,
  FalseVal,
  UnknownVal,
  isFunVal,
  isTrueVal,
  isFalseVal,
  isBoolVal,
  isBoolLit,
  FunV,
  isApp,
  isLam,
  isLadderGraphExpr,
} from './type.js'
import { Environment } from './environment.js'
import { LirContext } from '../layout-ir/core.js'
import type { LirId } from '../layout-ir/core.js'
import type {
  LadderLirNode,
  BoolVarLirNode,
  MergedNotLirNode,
  SemanticLadderLirNode,
} from '../layout-ir/ladder-lir.svelte.js'
import {
  isBoolVarLirNode,
  isMergedNotLirNode,
  isBundlingFlowLirNode,
  isSemanticLadderLirNode,
} from '../layout-ir/ladder-lir.svelte.js'
import type { DirectedAcyclicGraph } from '$lib/algebraic-graphs/dag'
import { match, P } from 'ts-pattern'

/**********************************************************
                   Evaluator
**********************************************************/

/* 
---------------------------------
       UI vs. semantics
----------------------------------
The Evaluator should be using the Environment and NOT the get/setValue methods on LirNodes,
since the LirNode values are meant to be for UI purposes
(and hence fall under the purview of the LadderGraphLirNode).

----------------------------------
See Note [WhatIf-style evaluation]
----------------------------------

*/

export interface Evaluator {
  eval(context: LirContext, expr: Expr): Value
}

export const SimpleEagerEvaluator: Evaluator = {
  /** An UnknownVal result means there wasn't enough info */
  eval(context: LirContext, expr: Expr): Value {
    return eval_(context, expr, new Environment([]))
  },
}

/** Internal helper */
function eval_(context: LirContext, expr: Expr, env: Environment): Value {
  return match(expr)
    .with(P.when(isBoolLit), (bl: BoolLit) => bl.getValue())
    .with(P.when(isLam), (lam: Lam) => {
      return new FunV(lam.getParams(), lam.getBody(), env)
    })
    .with(P.when(isApp), (app: App) => {
      const closureV = eval_(context, app.getFunc(), env)
      if (!isFunVal(closureV)) {
        throw new Error(`Expected a function value, but got ${closureV}`)
      }

      const paramArgPairs: Array<[Unique, Value]> = Array.from(
        app.getArgs()
      ).map(([uniq, expr]) => [uniq, eval_(context, expr, env)])
      const extendedEnv = closureV
        .getEnv()
        .getClonedEnvExtendedWith(paramArgPairs)
      return eval_(context, closureV.body, extendedEnv)
    })
    .with(P.when(isLadderGraphExpr), (ladder: LadderGraphExpr) =>
      evalLadderGraphExpr(context, env, ladder)
    )
    .exhaustive()
}

/*
TODO: might be better to map the SemanticLadderLirNodes to 
something that doesn't have any UI info at all --- perhaps just re-use something like
the original VizExpr / IRExpr --- and have the eval be over that.
So, e.g., instead of Dag<LirId>, would have something like Dag<SemanticNodeWithoutUIInfo>,
but each SemanticNodeWithoutUIInfo can be traced back to the corresponding SemanticLadderNode
*/
function evalSemanticLadderNode(
  context: LirContext,
  node: SemanticLadderLirNode,
  env: Environment
): BoolVal {
  return match(node)
    .with(
      P.when(isBoolVarLirNode),
      (node: BoolVarLirNode) => env.get(node.getUnique(context)) as BoolVal
    )
    .with(P.when(isMergedNotLirNode), (node: MergedNotLirNode) => {
      const negandV = evalLadderGraphExpr(context, env, node.getNegand(context))
      if (!isBoolVal(negandV)) {
        throw new Error('Expected the negand to eval to a BoolVal')
      }
      return match(negandV)
        .with({ $type: 'TrueVal' }, () => new FalseVal())
        .with({ $type: 'FalseVal' }, () => new TrueVal())
        .with({ $type: 'UnknownVal' }, () => new UnknownVal())
        .exhaustive()
    })
    .exhaustive()
}

function evalLadderGraphExpr(
  context: LirContext,
  env: Environment,
  ladder: LadderGraphExpr
): Value {
  const flow = ladder.getExprGraph()
  // TODO: Implement: for each NotStartNode, merge the < NotStart, ...negand, NotEnd> into one compact MergedNotNode.
  // ---------

  /* Assume we have an NNF.

  * If there's any path where where all the non-bundling-node vertices
  (which at this point would either be a MergedNot node or a bool var node) evaluate to true, then result is True
  * If no path satisfies the formula, then result is False
  * Otherwise, indeterminate / unknown result
  */
  const pathVals = flow.getAllPaths().map((p) => evalPath(context, env, p))
  return evalOrChain(pathVals)
}

/***************************
      AndChain, OrChain
****************************/

function evalAndChain(bools: BoolVal[]) {
  if (bools.some(isFalseVal)) {
    return new FalseVal()
  }
  if (bools.every(isTrueVal)) {
    return new TrueVal()
  }
  return new UnknownVal()
}

function evalOrChain(bools: BoolVal[]) {
  if (bools.some(isTrueVal)) {
    return new TrueVal()
  }
  if (bools.every(isFalseVal)) {
    return new FalseVal()
  }
  return new UnknownVal()
}

/***************************
      evalPath
****************************/

function evalPath(
  context: LirContext,
  env: Environment,
  /** A path graph that is a subgraph of the Ladder graph
   * (and where the Ladder graph is an NNF and
   * the Nots have been merged into a MergedNotLirNode)
   * TODO: Add more type safety here
   */
  pathGraph: DirectedAcyclicGraph<LirId>
) {
  const substantiveVertices = pathGraph
    .getVertices()
    .map((v) => context.get(v) as LadderLirNode)
    .filter((n) => !isBundlingFlowLirNode(n))

  if (!substantiveVertices.every(isSemanticLadderLirNode)) {
    throw new Error(
      'Not all vertices are compact -- the Nots have not been merged!'
    )
    // TODO: replace this with more type-level stuff
  }

  const evalResults = substantiveVertices.map((node) =>
    evalSemanticLadderNode(context, node, env)
  )

  // Remember that a 'path' here is basically an AndChain
  return evalAndChain(evalResults)
}
