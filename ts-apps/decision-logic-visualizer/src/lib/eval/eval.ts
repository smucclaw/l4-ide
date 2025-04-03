import type { Unique } from '@repo/viz-expr'
import type {
  Value,
  BoolVal,
  BoolLit,
  Lam,
  App,
  LadderDagExpr,
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
  // isUnknownVal,
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
  CompactLadderLirNode,
} from '../layout-ir/ladder-lir.svelte.js'
import {
  isBoolVarLirNode,
  isMergedNotLirNode,
  isBundlingFlowLirNode,
  isCompactLadderLirNode,
} from '../layout-ir/ladder-lir.svelte.js'
import type { DirectedAcyclicGraph } from '$lib/algebraic-graphs/dag'
import { match, P } from 'ts-pattern'

/**********************************************************
                   Evaluator
**********************************************************/

/* The Evaluator should NOT set values on the LirNodes.
It can update the Environment, but it shouldn't be setting values on LirNodes,
since the LirNode values are meant to be for UI purposes
(and hence fall under the purview of the LadderGraphLirNode.)

----------------------------------
See Note [WhatIf-style evaluation]
----------------------------------

*/

export interface Evaluator {
  eval(context: LirContext, expr: Expr): Value
}

export const SimpleEvaluator: Evaluator = {
  /** An UnknownVal result means there wasn't enough info */
  eval(context: LirContext, expr: Expr): Value {
    return eval_(context, expr, new Environment([]))
  },
}
// TODO: May not even need LirContext
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
    .with(P.when(isLadderGraphExpr), (ladder: LadderDagExpr) =>
      evalLadderDagExpr(context, env, ladder)
    )
    .exhaustive()
}

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

  if (!substantiveVertices.every(isCompactLadderLirNode)) {
    throw new Error(
      'Not all vertices are compact -- the Nots have not been merged!'
    )
    // TODO: replace this with more type-level stuff
  }

  const evalResults = substantiveVertices.map((node) =>
    evalCompactLadderNode(context, node, env)
  )

  // Remember that a path here amounts to an AndChain
  return evalAndChain(evalResults)
}

function evalCompactLadderNode(
  context: LirContext,
  node: CompactLadderLirNode,
  env: Environment
): BoolVal {
  return match(node)
    .with(
      P.when(isBoolVarLirNode),
      (node: BoolVarLirNode) => env.get(node.getUnique(context)) as BoolVal
    )
    .with(P.when(isMergedNotLirNode), (node: MergedNotLirNode) => {
      const negandV = evalLadderDagExpr(context, env, node.getNegand(context))
      if (!isBoolVal(negandV)) {
        throw new Error('Expected the negand to eval to a BoolVal')
      }
      return evalNotBoolVal(negandV)
    })
    .exhaustive()
}

function evalNotBoolVal(val: BoolVal): BoolVal {
  return match(val)
    .with({ $type: 'TrueVal' }, () => new FalseVal())
    .with({ $type: 'FalseVal' }, () => new TrueVal())
    .with({ $type: 'UnknownVal' }, () => new UnknownVal())
    .exhaustive()
}

function evalLadderDagExpr(
  context: LirContext,
  env: Environment,
  ladder: LadderDagExpr
): Value {
  const flow = ladder.getExprGraph()
  // TODO: Implement: for each NotStartNode, merge the < NotStart, ...negand, NotEnd> into one compact NotNode.
  // ---------

  /* Assume we have an NNF.

  * if there's any path where where all the non-bundling-node vertices
  (which at this point would either be a compact NOt node or a bool var node) evaluate to true, then result is True
  * If no path satisfies the formula, then result is False
  * Otherwise, indeterminate / unknown result
  */
  const paths = flow.getAllPaths()
  const pathVals = paths.map((p) => evalPath(context, env, p))
  return evalOrChain(pathVals)
}
