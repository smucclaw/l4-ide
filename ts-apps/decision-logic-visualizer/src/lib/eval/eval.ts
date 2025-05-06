import { App, IRId } from '@repo/viz-expr'
import type { UBoolVal, Expr, EvUBoolVar, Not, Or, And } from './type.js'
import {
  TrueVal,
  FalseVal,
  UnknownVal,
  isTrueVal,
  isFalseVal,
  isUnknownVal,
  toVEBoolValue,
  toUBoolVal,
} from './type.js'
import { Assignment } from './assignment.js'
import type { L4Connection } from '$lib/l4-connection.js'
import type { VersionedDocId } from '@repo/viz-expr'
import { match, P } from 'ts-pattern'

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
  eval(
    l4connection: L4Connection,
    verTxtDocId: VersionedDocId,
    ladder: Expr,
    assignment: Assignment
  ): Promise<EvalResult>
}

export const Evaluator: LadderEvaluator = {
  async eval(
    l4connection: L4Connection,
    verDocId: VersionedDocId,
    ladder: Expr,
    assignment: Assignment
  ): Promise<EvalResult> {
    async function eval_(
      ladder: Expr,
      intermediate: Map<IRId, UBoolVal>
    ): Promise<EvalResult> {
      return match(ladder)
        .with({ $type: 'UBoolVar' }, (expr: EvUBoolVar) => {
          const result = assignment.get(expr.name.unique) as UBoolVal
          const newIntermediate = intermediate.set(expr.id, result)
          return {
            result,
            intermediate: newIntermediate,
          }
        })
        .with({ $type: 'Not' }, async (expr: Not) => {
          const { result: negandV, intermediate: intermediate2 } = await eval_(
            expr.negand,
            intermediate
          )
          const result = match(negandV)
            .with(P.when(isTrueVal), () => new FalseVal())
            .with(P.when(isFalseVal), () => new TrueVal())
            .with(P.when(isUnknownVal), () => new UnknownVal())
            .exhaustive()

          const finalIntermediate = new Map(intermediate2).set(expr.id, result)

          return {
            result,
            intermediate: finalIntermediate,
          }
        })
        .with({ $type: 'And' }, async (expr: And) => {
          const andResults = await Promise.all(
            expr.args.map((arg) => eval_(arg, intermediate))
          )
          const result = evalAndChain(andResults.map((res) => res.result))

          const finalIntermediate = combineIntermediates(
            andResults.map((res) => res.intermediate)
          ).set(expr.id, result)

          return {
            result,
            intermediate: finalIntermediate,
          }
        })
        .with({ $type: 'Or' }, async (expr: Or) => {
          const orResults = await Promise.all(
            expr.args.map((arg) => eval_(arg, intermediate))
          )
          const result = evalOrChain(orResults.map((res) => res.result))

          const finalIntermediate = combineIntermediates(
            orResults.map((res) => res.intermediate)
          ).set(expr.id, result)

          return {
            result,
            intermediate: finalIntermediate,
          }
        })
        .with({ $type: 'App' }, async (expr: App) => {
          const argResults = await Promise.all(
            expr.args.map((arg) => eval_(arg, intermediate))
          )
          const args = argResults.map((res) => res.result)

          if (args.some(isUnknownVal)) {
            console.log(
              "Currently don't support eval-ing an App with Unknown args: will just return UnknownVal"
            )
            const res = new UnknownVal()
            const finalIntermediate = combineIntermediates(
              argResults.map((res) => res.intermediate)
            ).set(expr.id, res)
            return {
              result: res,
              intermediate: finalIntermediate,
            }
          }
          const argsForApp = args
            .map(toVEBoolValue)
            .filter((a) => a !== 'Unknown')

          const lspResponse = await l4connection.evalApp(
            expr.id,
            argsForApp,
            verDocId
          )
          if (!lspResponse) {
            throw new Error(`Problem evaluating App ${expr}`)
          }
          const res = toUBoolVal(lspResponse.value)
          const finalIntermediate = combineIntermediates(
            argResults.map((res) => res.intermediate)
          ).set(expr.id, res)

          return {
            result: res,
            intermediate: finalIntermediate,
          }
        })
        .exhaustive()
    }

    return eval_(ladder, new Map<IRId, UBoolVal>())
  },
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

/***************************
      Misc helpers
****************************/

/** Helper to combine multiple intermediate result maps into one */
function combineIntermediates(
  intermediates: Map<IRId, UBoolVal>[]
): Map<IRId, UBoolVal> {
  return new Map(
    intermediates.reduceRight(
      (accEntries, intermediate) => [...intermediate, ...accEntries],
      [] as [IRId, UBoolVal][]
    )
  )
}
