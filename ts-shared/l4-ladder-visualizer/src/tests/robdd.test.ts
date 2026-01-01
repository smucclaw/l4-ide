import { describe, expect, it } from 'vitest'
import { ROBDD } from '@repo/boolean-analysis'

describe('ROBDD', () => {
  it('reduces absorption: x ∨ (x ∧ y) = x', () => {
    const bdd = new ROBDD()
    const x = bdd.var(0)
    const y = bdd.var(1)
    const xAndY = bdd.apply('and', x, y)
    const expr = bdd.apply('or', x, xAndY)
    expect(expr).toBe(x)
  })

  it('restrict produces constants and support shrinks', () => {
    const bdd = new ROBDD()
    const a = bdd.var(0)
    const b = bdd.var(1)
    const f = bdd.apply('or', a, b)

    const fATrue = bdd.restrict(f, new Map([[0, true]]))
    expect(fATrue).toBe(ROBDD.TRUE)
    expect(Array.from(bdd.support(fATrue))).toEqual([])

    const fAFalse = bdd.restrict(f, new Map([[0, false]]))
    expect(fAFalse).toBe(b)
    expect(Array.from(bdd.support(fAFalse))).toEqual([1])
  })
})
