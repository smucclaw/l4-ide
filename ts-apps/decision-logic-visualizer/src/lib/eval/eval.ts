import type { Value, BoolVal, Expr, EVBoolVar, Not, Or, And } from './type.js'
import {
  TrueVal,
  FalseVal,
  UnknownVal,
  isTrueVal,
  isFalseVal,
  isBoolVal,
} from './type.js'
import { Assignment } from './assignment.js'
import { match } from 'ts-pattern'

/**********************************************************
                   Evaluator
**********************************************************/

/*
---------------------------------
       UI vs. evaln
----------------------------------
The Evaluator should be using the Subst and NOT the get/setValue methods on LirNodes,
since the LirNode values are meant to be for UI purposes
(and hence fall under the purview of the LadderGraphLirNode).

*/

/*
At least two approaches:

1. Do eval on the original IRExpr. 
   Apply whatever UI stuff is needed on the Dag<LirId> using the 
   IRId => LirId mapping
   -- eg for incompatible paths, for each IRNode that's false, 
      grey out the corresponding Dag<LirId>

2. Do eval on the Dag<LirId> directly

Ended up going with 2, since the postprocessing of the Dag<LirId> gets a bit involved,
and since 2 might align more with synchronizing with the backend in the future.
*/

/** Boolean operator evaluator */
export interface Evaluator {
  eval(ladder: Expr, assignment: Assignment): Value
}

export const Evaluator: Evaluator = {
  eval(ladder: Expr, assignment: Assignment): Value {
    return eval_(ladder, assignment)
  },
}

function eval_(ladder: Expr, assignment: Assignment): Value {
  return match(ladder)
    .with(
      { $type: 'BoolVar' },
      (expr: EVBoolVar) => assignment.get(expr.name.unique) as BoolVal
    )
    .with({ $type: 'Not' }, (expr: Not) => {
      const negandV = eval_(expr.negand, assignment)
      if (!isBoolVal(negandV)) {
        throw new Error('Expected the negajnd to eval to a BoolVal')
      }
      return match(negandV)
        .with({ $type: 'TrueVal' }, () => new FalseVal())
        .with({ $type: 'FalseVal' }, () => new TrueVal())
        .with({ $type: 'UnknownVal' }, () => new UnknownVal())
        .exhaustive()
    })
    .with({ $type: 'And' }, (expr: And) => {
      const andVals = expr.args.map((arg) => eval_(arg, assignment))
      return evalAndChain(andVals)
    })
    .with({ $type: 'Or' }, (expr: Or) => {
      const orVals = expr.args.map((arg) => eval_(arg, assignment))
      return evalOrChain(orVals)
    })
    .exhaustive()
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

// function evalLadderGraph(
//   context: LirContext,
//   assignment: Assignment,
//   ladder: DirectedAcyclicGraph<LirId>
// ): Value {
//   // TODO: Implement: for each NotStartNode, merge the < NotStart, ...negand, NotEnd> into one compact MergedNotNode.
//   // ---------

//   /* Assume we have an NNF.

//   * If there's any path where where all the non-bundling-node vertices
//   (which at this point would either be a MergedNot node or a bool var node) evaluate to true, then result is True
//   * If no path satisfies the formula, then result is False
//   * Otherwise, indeterminate / unknown result
//   */
//   const pathVals = ladder
//     .getAllPaths()
//     .map((p) => evalPath(context, assignment, p))
//   return evalOrChain(pathVals)
// }

// /***************************
//       evalPath
// ****************************/

// function evalPath(
//   context: LirContext,
//   args: Assignment,
//   /** A path graph that is a subgraph of the Ladder graph
//    * (and where the Ladder graph is an NNF and
//    * the Nots have been merged into a MergedNotLirNode)
//    * TODO: Add more type safety here
//    */
//   pathGraph: DirectedAcyclicGraph<LirId>
// ) {
//   const substantiveVertices = pathGraph
//     .getVertices()
//     .map((v) => context.get(v) as LadderLirNode)
//     .filter((n) => !isBundlingFlowLirNode(n))

//   if (!substantiveVertices.every(isSemanticLadderLirNode)) {
//     throw new Error(
//       'Not all vertices are SemanticLadderLirNodes -- the Nots have not been merged!'
//     )
//     // TODO: replace this with more type-level stuff
//   }

//   const evalResults = substantiveVertices.map((node) =>
//     evalSemanticLadderNode(context, args, node)
//   )

//   // Remember that a 'path' here is basically an AndChain
//   return evalAndChain(evalResults)
// }
