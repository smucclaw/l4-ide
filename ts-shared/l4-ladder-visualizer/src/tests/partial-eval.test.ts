import { describe, expect, it } from 'vitest'
import type { IRExpr, IRId, Name } from '@repo/viz-expr'
import { Assignment } from '../lib/eval/assignment.js'
import type { EvalResult } from '../lib/eval/eval.js'
import { FalseVal, TrueVal, UnknownVal } from '../lib/eval/type.js'
import { PartialEvalAnalyzer } from '../lib/eval/partial-eval.js'

function mkId(n: number): IRId {
  return { id: n }
}

function mkName(unique: number, label: string): Name {
  return { unique, label }
}

function uboolVar(idNum: number, unique: number, label: string): IRExpr {
  return {
    $type: 'UBoolVar',
    id: mkId(idNum),
    name: mkName(unique, label),
    value: 'UnknownV',
    canInline: false,
  }
}

function evalResultOf(
  result: TrueVal | FalseVal | UnknownVal,
  intermediate: Array<[IRId, TrueVal | FalseVal | UnknownVal]>,
  shortCircuitedRoots: IRId[] = [],
  consultedUniques: number[] = []
): EvalResult {
  return {
    result,
    intermediate: new Map(intermediate),
    consultedUniques: new Set(consultedUniques),
    shortCircuitedRoots: new Set(shortCircuitedRoots),
  }
}

describe('PartialEvalAnalyzer', () => {
  it('uses BDD and reports remaining support for a ∧ b with a=True', () => {
    const a = uboolVar(1, 1, 'a')
    const b = uboolVar(2, 2, 'b')
    const rootId = mkId(10)
    const expr: IRExpr = { $type: 'And', id: rootId, args: [a, b] }

    const analyzer = new PartialEvalAnalyzer(expr, [1, 2])
    const assignment = Assignment.fromEntries([[1, new TrueVal()]])
    const ev = evalResultOf(
      new UnknownVal(),
      [
        [a.id, new TrueVal()],
        [b.id, new UnknownVal()],
        [rootId, new UnknownVal()],
      ],
      [],
      [1, 2]
    )

    const analysis = analyzer.analyze(assignment, ev)
    expect(analysis.usedBdd).toBe(true)
    expect(analysis.notAsked).toEqual([2])
    expect(analysis.stillNeeded).toEqual([2])
    expect(analysis.dontCare).toEqual([])
  })

  it('marks short-circuited branches in a ∨ b with a=True', () => {
    const a = uboolVar(1, 1, 'a')
    const b = uboolVar(2, 2, 'b')
    const rootId = mkId(11)
    const expr: IRExpr = { $type: 'Or', id: rootId, args: [a, b] }

    const analyzer = new PartialEvalAnalyzer(expr, [1, 2])
    const assignment = Assignment.fromEntries([[1, new TrueVal()]])
    const ev = evalResultOf(
      new TrueVal(),
      [
        [a.id, new TrueVal()],
        [b.id, new UnknownVal()],
        [rootId, new TrueVal()],
      ],
      [b.id],
      [1]
    )

    const analysis = analyzer.analyze(assignment, ev)
    expect(analysis.usedBdd).toBe(true)
    expect(analysis.notAsked).toEqual([2])
    expect(analysis.stillNeeded).toEqual([])
    expect(analysis.dontCare).toEqual([2])
    expect(analysis.nodeRelevance.get(b.id)).toBe('short-circuited')
  })

  it('falls back to syntactic support when expression contains App', () => {
    const a = uboolVar(1, 1, 'a')
    const rootId = mkId(12)
    const expr: IRExpr = {
      $type: 'App',
      id: rootId,
      fnName: mkName(999, 'p'),
      args: [a],
    }

    const analyzer = new PartialEvalAnalyzer(expr, [1])
    const assignment = new Assignment([])
    const ev = evalResultOf(
      new UnknownVal(),
      [[rootId, new UnknownVal()]],
      [],
      []
    )

    const analysis = analyzer.analyze(assignment, ev)
    expect(analysis.usedBdd).toBe(false)
    expect(analysis.notAsked).toEqual([1])
    expect(analysis.stillNeeded).toEqual([1])
    expect(analysis.dontCare).toEqual([])
  })
})
