import { describe, expect, it } from 'vitest'
import { compileDecisionQuery } from '../decision-query.js'
import type { IRExpr } from '@repo/viz-expr'

function uboolVar(id: number, unique: number, label: string): IRExpr {
  return {
    $type: 'UBoolVar',
    id: { id },
    name: { unique, label },
    value: 'UnknownV',
    canInline: false,
  }
}

function and(id: number, args: IRExpr[]): IRExpr {
  return { $type: 'And', id: { id }, args }
}

function or(id: number, args: IRExpr[]): IRExpr {
  return { $type: 'Or', id: { id }, args }
}

describe('compileDecisionQuery', () => {
  it('computes support via reduction: x ∨ (x ∧ y) has support {x}', () => {
    const x = uboolVar(1, 1, 'x')
    const y = uboolVar(2, 2, 'y')
    const expr = or(3, [x, and(4, [x, y])])

    const compiled = compileDecisionQuery(expr, [1, 2])
    const res = compiled.query(new Map())

    expect(res.determined).toBeNull()
    expect(res.support).toEqual([1])
    expect(res.ranked).toEqual([1])
  })

  it('(x ∨ y) ranks earlier vars first when equal impact', () => {
    const x = uboolVar(1, 1, 'x')
    const y = uboolVar(2, 2, 'y')
    const expr = or(3, [x, y])

    const compiled = compileDecisionQuery(expr, [1, 2])
    const res = compiled.query(new Map())

    expect(res.support).toEqual([1, 2])
    expect(res.ranked).toEqual([1, 2])
    expect(res.impact.get(1)?.ifTrue.determined).toBe(true)
    expect(res.impact.get(1)?.ifFalse.determined).toBeNull()
  })

  it('restriction can determine result and empty support', () => {
    const x = uboolVar(1, 1, 'x')
    const y = uboolVar(2, 2, 'y')
    const expr = or(3, [x, y])

    const compiled = compileDecisionQuery(expr, [1, 2])
    const res = compiled.query(new Map([[1, true]]))

    expect(res.determined).toBe(true)
    expect(res.support).toEqual([])
    expect(res.ranked).toEqual([])
  })
})
