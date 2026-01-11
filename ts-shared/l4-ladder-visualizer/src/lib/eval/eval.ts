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
import type { Unique } from '@repo/viz-expr'

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
  /** UBoolVar uniques consulted during evaluation. */
  consultedUniques: Set<Unique>
  /** Expression roots that would be short-circuited in left-to-right semantics. */
  shortCircuitedRoots: Set<IRId>
}

/** Boolean operator evaluator */
export interface LadderEvaluator {
  eval(
    l4connection: L4Connection,
    verDocId: VersionedDocId,
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
        .with({ $type: 'TrueE' }, () => {
          const result = new TrueVal()
          const newIntermediate = intermediate.set(ladder.id, result)
          return {
            result,
            intermediate: newIntermediate,
            consultedUniques: new Set<Unique>(),
            shortCircuitedRoots: new Set<IRId>(),
          }
        })
        .with({ $type: 'FalseE' }, () => {
          const result = new FalseVal()
          const newIntermediate = intermediate.set(ladder.id, result)
          return {
            result,
            intermediate: newIntermediate,
            consultedUniques: new Set<Unique>(),
            shortCircuitedRoots: new Set<IRId>(),
          }
        })
        .with({ $type: 'UBoolVar' }, (expr: EvUBoolVar) => {
          const result = assignment.get(expr.name.unique) as UBoolVal
          const newIntermediate = intermediate.set(expr.id, result)
          return {
            result,
            intermediate: newIntermediate,
            consultedUniques: new Set([expr.name.unique]),
            shortCircuitedRoots: new Set<IRId>(),
          }
        })
        .with({ $type: 'Not' }, async (expr: Not) => {
          const child = await eval_(expr.negand, intermediate)
          const negandV = child.result
          const result = match(negandV)
            .with(P.when(isTrueVal), () => new FalseVal())
            .with(P.when(isFalseVal), () => new TrueVal())
            .with(P.when(isUnknownVal), () => new UnknownVal())
            .exhaustive()

          const finalIntermediate = new Map(child.intermediate).set(
            expr.id,
            result
          )

          return {
            result,
            intermediate: finalIntermediate,
            consultedUniques: child.consultedUniques,
            shortCircuitedRoots: child.shortCircuitedRoots,
          }
        })
        .with({ $type: 'And' }, async (expr: And) => {
          const andResults = await Promise.all(
            expr.args.map((arg) => eval_(arg, intermediate))
          )
          const { result, shortCircuitedRoots } = evalAndChain(
            andResults.map((res) => res.result),
            expr.args.map((arg) => arg.id)
          )

          const finalIntermediate = combineIntermediates(
            andResults.map((res) => res.intermediate)
          ).set(expr.id, result)

          return {
            result,
            intermediate: finalIntermediate,
            consultedUniques: unionSets(
              andResults.map((r) => r.consultedUniques)
            ),
            shortCircuitedRoots: unionSets([
              shortCircuitedRoots,
              ...andResults.map((r) => r.shortCircuitedRoots),
            ]),
          }
        })
        .with({ $type: 'Or' }, async (expr: Or) => {
          const orResults = await Promise.all(
            expr.args.map((arg) => eval_(arg, intermediate))
          )
          const { result, shortCircuitedRoots } = evalOrChain(
            orResults.map((res) => res.result),
            expr.args.map((arg) => arg.id)
          )

          const finalIntermediate = combineIntermediates(
            orResults.map((res) => res.intermediate)
          ).set(expr.id, result)

          return {
            result,
            intermediate: finalIntermediate,
            consultedUniques: unionSets(
              orResults.map((r) => r.consultedUniques)
            ),
            shortCircuitedRoots: unionSets([
              shortCircuitedRoots,
              ...orResults.map((r) => r.shortCircuitedRoots),
            ]),
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
              consultedUniques: unionSets(
                argResults.map((r) => r.consultedUniques)
              ),
              shortCircuitedRoots: unionSets(
                argResults.map((r) => r.shortCircuitedRoots)
              ),
            }
          }
          const argsForApp = args
            .map(toVEBoolValue)
            .filter((a) => a !== 'UnknownV')

          const lspResponse = await l4connection.evalApp(
            expr.id,
            argsForApp,
            verDocId
          )
          console.log('eval.ts: eval app lspResponse', lspResponse)
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
            consultedUniques: unionSets(
              argResults.map((r) => r.consultedUniques)
            ),
            shortCircuitedRoots: unionSets(
              argResults.map((r) => r.shortCircuitedRoots)
            ),
          }
        })
        .with({ $type: 'InertE' }, (inert) => {
          // Inert elements evaluate to the identity for their containing operator:
          // InertAnd → True (identity for AND), InertOr → False (identity for OR)
          const result =
            inert.context === 'InertAnd' ? new TrueVal() : new FalseVal()
          const newIntermediate = intermediate.set(ladder.id, result)
          return {
            result,
            intermediate: newIntermediate,
            consultedUniques: new Set<Unique>(),
            shortCircuitedRoots: new Set<IRId>(),
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

function evalAndChain(bools: UBoolVal[], ids: IRId[]) {
  const firstFalse = bools.findIndex(isFalseVal)
  if (firstFalse !== -1) {
    return {
      result: new FalseVal(),
      shortCircuitedRoots: new Set(ids.slice(firstFalse + 1)),
    }
  }
  if (bools.every(isTrueVal)) {
    return {
      result: new TrueVal(),
      shortCircuitedRoots: new Set<IRId>(),
    }
  }
  return {
    result: new UnknownVal(),
    shortCircuitedRoots: new Set<IRId>(),
  }
}

function evalOrChain(bools: UBoolVal[], ids: IRId[]) {
  const firstTrue = bools.findIndex(isTrueVal)
  if (firstTrue !== -1) {
    return {
      result: new TrueVal(),
      shortCircuitedRoots: new Set(ids.slice(firstTrue + 1)),
    }
  }
  if (bools.every(isFalseVal)) {
    return {
      result: new FalseVal(),
      shortCircuitedRoots: new Set<IRId>(),
    }
  }
  return {
    result: new UnknownVal(),
    shortCircuitedRoots: new Set<IRId>(),
  }
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

function unionSets<T>(sets: ReadonlyArray<ReadonlySet<T>>): Set<T> {
  const out = new Set<T>()
  for (const s of sets) {
    for (const v of s) out.add(v)
  }
  return out
}
