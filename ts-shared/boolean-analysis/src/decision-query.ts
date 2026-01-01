import type { IRExpr, Unique } from '@repo/viz-expr'
import { match } from 'ts-pattern'
import { ROBDD } from './robdd.js'

export type Determined = boolean | null

export interface QueryOutcome {
  determined: Determined
  support: Unique[]
}

export interface VarImpact {
  ifTrue: QueryOutcome
  ifFalse: QueryOutcome
}

export interface DecisionQueryResult {
  determined: Determined
  support: Unique[]
  ranked: Unique[]
  impact: Map<Unique, VarImpact>
  stats: { nodes: number; support: number }
}

export interface CompiledDecisionQuery {
  /** Order used for BDD variable indices */
  readonly varOrder: Unique[]
  /** Query with known bindings. Absent vars are treated as unknown. */
  query(bindings: ReadonlyMap<Unique, boolean>): DecisionQueryResult
}

function collectVarOrder(expr: IRExpr): Unique[] {
  const seen = new Set<Unique>()
  const out: Unique[] = []
  const go = (e: IRExpr) => {
    match(e)
      .with({ $type: 'UBoolVar' }, (v) => {
        if (!seen.has(v.name.unique)) {
          seen.add(v.name.unique)
          out.push(v.name.unique)
        }
      })
      .with({ $type: 'Not' }, (n) => go(n.negand))
      .with({ $type: 'And' }, (a) => a.args.forEach(go))
      .with({ $type: 'Or' }, (o) => o.args.forEach(go))
      .with({ $type: 'App' }, () => {
        throw new Error(
          'Cannot compile decision query: App nodes not supported'
        )
      })
      .otherwise(() => {})
  }
  go(expr)
  return out
}

function isDeterminedId(id: number): Determined {
  if (id === ROBDD.TRUE) return true
  if (id === ROBDD.FALSE) return false
  return null
}

function uniqSorted(nums: Iterable<number>): number[] {
  return Array.from(new Set(nums)).sort((a, b) => a - b)
}

export function compileDecisionQuery(
  expr: IRExpr,
  varOrder: Unique[] = collectVarOrder(expr)
): CompiledDecisionQuery {
  const bdd = new ROBDD()
  const uniqueToVarIndex = new Map<Unique, number>()
  varOrder.forEach((u, i) => uniqueToVarIndex.set(u, i))

  const compile = (e: IRExpr): number => {
    return match(e)
      .with({ $type: 'TrueE' }, () => ROBDD.TRUE)
      .with({ $type: 'FalseE' }, () => ROBDD.FALSE)
      .with({ $type: 'UBoolVar' }, (v) => {
        const idx = uniqueToVarIndex.get(v.name.unique)
        if (idx === undefined) {
          throw new Error(
            `Internal error: missing var index for unique ${v.name.unique}`
          )
        }
        return bdd.var(idx)
      })
      .with({ $type: 'Not' }, (n) => bdd.neg(compile(n.negand)))
      .with({ $type: 'And' }, (a) =>
        a.args.reduce((acc, x) => bdd.apply('and', acc, compile(x)), ROBDD.TRUE)
      )
      .with({ $type: 'Or' }, (o) =>
        o.args.reduce((acc, x) => bdd.apply('or', acc, compile(x)), ROBDD.FALSE)
      )
      .with({ $type: 'App' }, (app) => {
        throw new Error(
          `Cannot compile decision query: App ${app.fnName.label} not supported`
        )
      })
      .exhaustive()
  }

  const root = compile(expr)

  const supportFromRestricted = (restricted: number): Unique[] => {
    const idxs = bdd.support(restricted)
    return uniqSorted(
      Array.from(idxs)
        .map((i) => varOrder[i])
        .filter((u): u is Unique => u !== undefined)
    )
  }

  const outcomeFromRestricted = (restricted: number): QueryOutcome => ({
    determined: isDeterminedId(restricted),
    support: supportFromRestricted(restricted),
  })

  const rank = (
    support: Unique[],
    impact: Map<Unique, VarImpact>
  ): Unique[] => {
    const score = (u: Unique): [number, number] => {
      const imp = impact.get(u)
      const determinableCount =
        (imp?.ifTrue.determined !== null ? 1 : 0) +
        (imp?.ifFalse.determined !== null ? 1 : 0)
      const level = uniqueToVarIndex.get(u) ?? 1_000_000
      return [-determinableCount, level]
    }
    return support.slice().sort((a, b) => {
      const [a0, a1] = score(a)
      const [b0, b1] = score(b)
      if (a0 !== b0) return a0 - b0
      return a1 - b1
    })
  }

  return {
    varOrder,
    query(bindings: ReadonlyMap<Unique, boolean>): DecisionQueryResult {
      const idxBindings = new Map<number, boolean>()
      for (const [u, b] of bindings.entries()) {
        const idx = uniqueToVarIndex.get(u)
        if (idx !== undefined) idxBindings.set(idx, b)
      }

      const restricted = bdd.restrict(root, idxBindings)
      const support = supportFromRestricted(restricted)
      const determined = isDeterminedId(restricted)

      const impact = new Map<Unique, VarImpact>()
      for (const u of support) {
        const idx = uniqueToVarIndex.get(u)
        if (idx === undefined) continue
        const withTrue = bdd.restrict(restricted, new Map([[idx, true]]))
        const withFalse = bdd.restrict(restricted, new Map([[idx, false]]))
        impact.set(u, {
          ifTrue: outcomeFromRestricted(withTrue),
          ifFalse: outcomeFromRestricted(withFalse),
        })
      }

      return {
        determined,
        support,
        ranked: rank(support, impact),
        impact,
        stats: { nodes: bdd.nodeCount(), support: support.length },
      }
    },
  }
}
