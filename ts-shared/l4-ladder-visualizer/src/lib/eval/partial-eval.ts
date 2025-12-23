import type { IRExpr, IRId, Unique } from '@repo/viz-expr'
import { match } from 'ts-pattern'
import type { Assignment } from './assignment.js'
import type { EvalResult } from './eval.js'
import { type UBoolVal } from './type.js'
import {
  compileDecisionQuery,
  type CompiledDecisionQuery,
} from '@repo/boolean-analysis'

export type RelevanceStatus =
  | 'unknown'
  | 'determined'
  | 'relevant'
  | 'irrelevant'
  | 'short-circuited'

export interface PartialEvalAnalysis {
  overallResult: UBoolVal
  isDetermined: boolean

  notAsked: Unique[]
  stillNeeded: Unique[]
  dontCare: Unique[]

  nodeRelevance: Map<IRId, RelevanceStatus>
  irrelevantRootIds: Set<IRId>
  consultedUniques: Set<Unique>
  shortCircuitedRoots: Set<IRId>
  shortCircuitedNodeIds: Set<IRId>

  restrictedExpr: IRExpr
  restrictedExprText: string

  bddStats?: { nodes: number; support: number }
  usedBdd: boolean
}

function isDeterminedValue(v: UBoolVal): boolean {
  return v.$type === 'TrueV' || v.$type === 'FalseV'
}

function setIntersectEmpty<T>(a: ReadonlySet<T>, b: ReadonlySet<T>): boolean {
  for (const x of a) {
    if (b.has(x)) return false
  }
  return true
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
      .with({ $type: 'App' }, (app) => app.args.forEach(go))
      .otherwise(() => {})
  }
  go(expr)
  return out
}

function containsApp(expr: IRExpr): boolean {
  let found = false
  const go = (e: IRExpr) => {
    if (found) return
    match(e)
      .with({ $type: 'App' }, () => {
        found = true
      })
      .with({ $type: 'Not' }, (n) => go(n.negand))
      .with({ $type: 'And' }, (a) => a.args.forEach(go))
      .with({ $type: 'Or' }, (o) => o.args.forEach(go))
      .with({ $type: 'UBoolVar' }, () => {})
      .otherwise(() => {})
  }
  go(expr)
  return found
}

function restrictExpr(expr: IRExpr, assignment: Assignment): IRExpr {
  return match(expr)
    .with({ $type: 'UBoolVar' }, (v): IRExpr => {
      const val = assignment.get(v.name.unique)
      if (!val) return v
      if (val.$type === 'TrueV') return { $type: 'TrueE', id: v.id } as IRExpr
      if (val.$type === 'FalseV') return { $type: 'FalseE', id: v.id } as IRExpr
      return v
    })
    .with({ $type: 'TrueE' }, (t) => t)
    .with({ $type: 'FalseE' }, (f) => f)
    .with({ $type: 'Not' }, (n): IRExpr => {
      const inner = restrictExpr(n.negand, assignment)
      return match(inner)
        .with(
          { $type: 'TrueE' },
          () => ({ $type: 'FalseE', id: n.id }) as IRExpr
        )
        .with(
          { $type: 'FalseE' },
          () => ({ $type: 'TrueE', id: n.id }) as IRExpr
        )
        .with({ $type: 'Not' }, (nn) => nn.negand)
        .otherwise(() => ({ ...n, negand: inner }))
    })
    .with({ $type: 'And' }, (a): IRExpr => {
      const args = a.args.map((x) => restrictExpr(x, assignment))
      if (args.some((x) => x.$type === 'FalseE')) {
        return { $type: 'FalseE', id: a.id } as IRExpr
      }
      const filtered = args.filter((x) => x.$type !== 'TrueE')
      if (filtered.length === 0) return { $type: 'TrueE', id: a.id } as IRExpr
      if (filtered.length === 1) return filtered[0]
      return { ...a, args: filtered }
    })
    .with({ $type: 'Or' }, (o): IRExpr => {
      const args = o.args.map((x) => restrictExpr(x, assignment))
      if (args.some((x) => x.$type === 'TrueE')) {
        return { $type: 'TrueE', id: o.id } as IRExpr
      }
      const filtered = args.filter((x) => x.$type !== 'FalseE')
      if (filtered.length === 0) return { $type: 'FalseE', id: o.id } as IRExpr
      if (filtered.length === 1) return filtered[0]
      return { ...o, args: filtered }
    })
    .with({ $type: 'App' }, (app): IRExpr => {
      const args = app.args.map((x) => restrictExpr(x, assignment))
      return { ...app, args }
    })
    .exhaustive()
}

function exprToText(expr: IRExpr): string {
  const go = (e: IRExpr): string =>
    match(e)
      .with({ $type: 'TrueE' }, () => 'TRUE')
      .with({ $type: 'FalseE' }, () => 'FALSE')
      .with({ $type: 'UBoolVar' }, (v) => v.name.label)
      .with({ $type: 'Not' }, (n) => `¬(${go(n.negand)})`)
      .with({ $type: 'And' }, (a) => `(${a.args.map(go).join(' ∧ ')})`)
      .with({ $type: 'Or' }, (o) => `(${o.args.map(go).join(' ∨ ')})`)
      .with({ $type: 'App' }, (app) => {
        const args = app.args.map(go).join(', ')
        return `${app.fnName.label}(${args})`
      })
      .exhaustive()
  return go(expr)
}

function collectSubtreeVars(expr: IRExpr): Map<IRId, Set<Unique>> {
  const memo = new Map<IRId, Set<Unique>>()

  const go = (e: IRExpr): Set<Unique> => {
    const existing = memo.get(e.id)
    if (existing) return existing

    const vars = match(e)
      .with({ $type: 'UBoolVar' }, (v) => new Set<Unique>([v.name.unique]))
      .with({ $type: 'Not' }, (n) => new Set(go(n.negand)))
      .with({ $type: 'And' }, (a) => unionUniqueSets(a.args.map(go)))
      .with({ $type: 'Or' }, (o) => unionUniqueSets(o.args.map(go)))
      .with({ $type: 'App' }, (app) => unionUniqueSets(app.args.map(go)))
      .otherwise(() => new Set<Unique>())

    memo.set(e.id, vars)
    return vars
  }

  go(expr)
  return memo
}

function collectSubtreeIds(expr: IRExpr): Map<IRId, Set<IRId>> {
  const memo = new Map<IRId, Set<IRId>>()

  const go = (e: IRExpr): Set<IRId> => {
    const existing = memo.get(e.id)
    if (existing) return existing

    const children = match(e)
      .with({ $type: 'Not' }, (n) => [n.negand])
      .with({ $type: 'And' }, (a) => a.args)
      .with({ $type: 'Or' }, (o) => o.args)
      .with({ $type: 'App' }, (app) => app.args)
      .otherwise(() => [])

    const out = new Set<IRId>([e.id])
    for (const c of children) {
      for (const cid of go(c)) out.add(cid)
    }
    memo.set(e.id, out)
    return out
  }

  go(expr)
  return memo
}

function collectParentMap(expr: IRExpr): Map<IRId, IRId | null> {
  const parent = new Map<IRId, IRId | null>()

  const go = (e: IRExpr, p: IRId | null) => {
    if (!parent.has(e.id)) parent.set(e.id, p)

    const children = match(e)
      .with({ $type: 'Not' }, (n) => [n.negand])
      .with({ $type: 'And' }, (a) => a.args)
      .with({ $type: 'Or' }, (o) => o.args)
      .with({ $type: 'App' }, (app) => app.args)
      .otherwise(() => [])

    for (const c of children) go(c, e.id)
  }

  go(expr, null)
  return parent
}

function unionUniqueSets(
  sets: ReadonlyArray<ReadonlySet<Unique>>
): Set<Unique> {
  const out = new Set<Unique>()
  for (const s of sets) {
    for (const v of s) out.add(v)
  }
  return out
}

function uniqSorted(nums: Iterable<number>): number[] {
  return Array.from(new Set(nums)).sort((a, b) => a - b)
}

export class PartialEvalAnalyzer {
  readonly #expr: IRExpr
  readonly #allVarsInOrder: Unique[]
  readonly #subtreeVars: Map<IRId, Set<Unique>>
  readonly #subtreeIds: Map<IRId, Set<IRId>>
  readonly #parentMap: Map<IRId, IRId | null>

  readonly #usedBdd: boolean
  readonly #compiled?: CompiledDecisionQuery

  constructor(expr: IRExpr, varOrder: Unique[] = collectVarOrder(expr)) {
    this.#expr = expr
    this.#allVarsInOrder = varOrder
    this.#subtreeVars = collectSubtreeVars(expr)
    this.#subtreeIds = collectSubtreeIds(expr)
    this.#parentMap = collectParentMap(expr)

    if (containsApp(expr)) {
      this.#usedBdd = false
      return
    }

    this.#compiled = compileDecisionQuery(expr, varOrder)
    this.#usedBdd = true
  }

  analyze(assignment: Assignment, evalResult: EvalResult): PartialEvalAnalysis {
    const restrictedExpr = restrictExpr(this.#expr, assignment)
    const restrictedExprText = exprToText(restrictedExpr)

    const notAsked = this.#allVarsInOrder.filter((u) => {
      const v = assignment.get(u)
      return !v || v.$type === 'UnknownV'
    })

    let support = new Set<Unique>()
    let bddStats: PartialEvalAnalysis['bddStats'] = undefined

    if (this.#usedBdd && this.#compiled) {
      const bindings = new Map<Unique, boolean>()
      for (const [unique, val] of assignment.getEntries()) {
        if (val.$type === 'TrueV') bindings.set(unique, true)
        if (val.$type === 'FalseV') bindings.set(unique, false)
      }

      const q = this.#compiled.query(bindings)
      support = new Set<Unique>(q.support)
      bddStats = { nodes: q.stats.nodes, support: q.stats.support }
    } else {
      // Fallback: syntactic support of the restricted expression.
      const vars = collectVarOrder(restrictedExpr)
      support = new Set<Unique>(vars)
    }

    const stillNeeded = notAsked.filter((u) => support.has(u))
    const dontCare = notAsked.filter((u) => !support.has(u))

    // Expand short-circuit roots to whole subtrees for shading.
    const shortCircuitedNodeIds = new Set<IRId>()
    for (const rootId of evalResult.shortCircuitedRoots) {
      const ids = this.#subtreeIds.get(rootId)
      if (!ids) continue
      for (const id of ids) shortCircuitedNodeIds.add(id)
    }

    const nodeRelevance = new Map<IRId, RelevanceStatus>()
    const supportSet = support

    for (const [nodeId, subtreeVarSet] of this.#subtreeVars.entries()) {
      if (shortCircuitedNodeIds.has(nodeId)) {
        nodeRelevance.set(nodeId, 'short-circuited')
        continue
      }
      const v = evalResult.intermediate.get(nodeId)
      if (v && isDeterminedValue(v)) {
        nodeRelevance.set(nodeId, 'determined')
        continue
      }
      if (setIntersectEmpty(subtreeVarSet, supportSet)) {
        nodeRelevance.set(nodeId, 'irrelevant')
        continue
      }
      nodeRelevance.set(nodeId, 'relevant')
    }

    const irrelevantRootIds = new Set<IRId>()
    for (const [nodeId, status] of nodeRelevance.entries()) {
      if (status !== 'irrelevant') continue
      const p = this.#parentMap.get(nodeId) ?? null
      if (!p) {
        irrelevantRootIds.add(nodeId)
        continue
      }
      if (nodeRelevance.get(p) !== 'irrelevant') irrelevantRootIds.add(nodeId)
    }

    return {
      overallResult: evalResult.result,
      isDetermined: isDeterminedValue(evalResult.result),
      notAsked: uniqSorted(notAsked),
      stillNeeded: uniqSorted(stillNeeded),
      dontCare: uniqSorted(dontCare),
      nodeRelevance,
      irrelevantRootIds,
      consultedUniques: evalResult.consultedUniques,
      shortCircuitedRoots: evalResult.shortCircuitedRoots,
      shortCircuitedNodeIds,
      restrictedExpr,
      restrictedExprText,
      bddStats,
      usedBdd: this.#usedBdd,
    }
  }
}
