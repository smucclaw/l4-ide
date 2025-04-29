import { App, IRId } from '@repo/viz-expr'
import type { UBoolVal, Expr, EvUBoolVar, Not, Or, And } from './type.js'
import { TrueVal, FalseVal, UnknownVal, isTrueVal, isFalseVal } from './type.js'
import { Assignment } from './assignment.js'
import { match } from 'ts-pattern'

/**********************************************************
                   Evaluator
**********************************************************/

/*
------------------------------------------
       UI vs. evaln
------------------------------------------
The Evaluator should be using the Assignment and NOT the get/setValue methods on LirNodes,
since the LirNode values are meant to be for the UI
(and hence fall under the purview of the LadderGraphLirNode).

-------------------------------------------
  Implementation: At least two approaches
-------------------------------------------

1. Do eval on the original IRExpr. 
   Apply whatever UI stuff is needed on the Dag<LirId> using the 
   IRId => LirId mapping
   -- eg for incompatible paths, for each IRNode that's false, 
      grey out the corresponding Dag<LirId>

2. Do eval on the Dag<LirId> directly

Ended up going with 2, since the postprocessing of the Dag<LirId> gets a bit involved,
and since 2 might align better with synchronizing with the backend in the future.
*/

export interface EvalResult {
  result: UBoolVal
  intermediate: Map<IRId, UBoolVal>
}

/** Boolean operator evaluator */
export interface LadderEvaluator {
  eval(ladder: Expr, assignment: Assignment): EvalResult
}

export const Evaluator: LadderEvaluator = {
  eval(ladder: Expr, assignment: Assignment): EvalResult {
    return eval_(ladder, assignment, new Map<IRId, UBoolVal>())
  },
}

function eval_(
  ladder: Expr,
  assignment: Assignment,
  intermediate: Map<IRId, UBoolVal>
): EvalResult {
  return match(ladder)
    .with({ $type: 'UBoolVar' }, (expr: EvUBoolVar) => {
      const result = assignment.get(expr.name.unique) as UBoolVal
      const newIntermediate = intermediate.set(expr.id, result)
      return {
        result,
        intermediate: newIntermediate,
      }
    })
    .with({ $type: 'Not' }, (expr: Not) => {
      const { result: negandV, intermediate: intermediate2 } = eval_(
        expr.negand,
        assignment,
        intermediate
      )
      const result = match(negandV)
        .with({ $type: 'TrueVal' }, () => new FalseVal())
        .with({ $type: 'FalseVal' }, () => new TrueVal())
        .with({ $type: 'UnknownVal' }, () => new UnknownVal())
        .exhaustive()

      const finalIntermediate = new Map(intermediate2).set(expr.id, result)

      return {
        result,
        intermediate: finalIntermediate,
      }
    })
    .with({ $type: 'And' }, (expr: And) => {
      const andResults = expr.args.map((arg) =>
        eval_(arg, assignment, intermediate)
      )
      const result = evalAndChain(andResults.map((res) => res.result))

      const combinedIntermeds = andResults
        .map((res) => res.intermediate)
        .reduceRight((acc, res) => {
          return new Map([...acc, ...res])
        })
      const finalIntermediate = new Map(combinedIntermeds).set(expr.id, result)

      return {
        result,
        intermediate: finalIntermediate,
      }
    })
    .with({ $type: 'Or' }, (expr: Or) => {
      const orResults = expr.args.map((arg) =>
        eval_(arg, assignment, intermediate)
      )
      const result = evalOrChain(orResults.map((res) => res.result))

      const combinedIntermediates = orResults
        .map((res) => res.intermediate)
        .reduceRight((acc, res) => {
          return new Map([...acc, ...res])
        })
      const finalIntermediate = new Map(combinedIntermediates).set(
        expr.id,
        result
      )

      return {
        result,
        intermediate: finalIntermediate,
      }
    })
    .with({ $type: 'App' }, (expr: App) => {
      // TODO: TEMPORARY placeholder code just to get the UI going -- will replace this with a query to the backend
      const tempPlaceholderResult = new UnknownVal()
      const newIntermediate = new Map(intermediate).set(
        expr.id,
        tempPlaceholderResult
      )
      return {
        result: tempPlaceholderResult,
        intermediate: newIntermediate,
      }
    })
    .exhaustive()
}

/***************************
      AndChain, OrChain
****************************/

function evalAndChain(bools: UBoolVal[]) {
  if (bools.some(isFalseVal)) {
    return new FalseVal()
  }
  if (bools.every(isTrueVal)) {
    return new TrueVal()
  }
  return new UnknownVal()
}

function evalOrChain(bools: UBoolVal[]) {
  if (bools.some(isTrueVal)) {
    return new TrueVal()
  }
  if (bools.every(isFalseVal)) {
    return new FalseVal()
  }
  return new UnknownVal()
}
