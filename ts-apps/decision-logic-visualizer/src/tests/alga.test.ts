import { describe, test, expect } from 'vitest'
import {
  vertex,
  // edge,
  overlay,
  connect,
  // empty,
  ComparisonResult,
} from '../lib/algebraic-graphs/alga'
import type { Ord } from '../lib/algebraic-graphs/alga'

class NumberWrapper implements Ord {
  constructor(private value: number) {}

  isEqualTo(other: unknown): boolean {
    return other instanceof NumberWrapper && this.value === other.value
  }

  compare(other: NumberWrapper): ComparisonResult {
    if (this.value < other.value) return ComparisonResult.LessThan
    if (this.value > other.value) return ComparisonResult.GreaterThan
    return ComparisonResult.Equal
  }

  toString(): string {
    return `NWrapper ${this.value.toString()}`
  }
}

describe('Algebraic Graphs - Overlay Commutativity', () => {
  test('Commutativity of overlay for simple vertices', () => {
    const va = vertex(new NumberWrapper(1))
    const vb = vertex(new NumberWrapper(2))

    const aPlusB = overlay(va, vb)
    const bPlusA = overlay(vb, va)

    expect(aPlusB.isEqualTo(bPlusA)).toBeTruthy()
  })
})

describe('Algebraic Graphs - Overlay Associativity', () => {
  test('Overlay is associative', () => {
    const va = vertex(new NumberWrapper(1))
    const vb = vertex(new NumberWrapper(2))
    const vc = vertex(new NumberWrapper(3))

    const aPlusBC = overlay(va, overlay(vb, vc))
    const abPlusC = overlay(overlay(va, vb), vc)

    expect(aPlusBC.isEqualTo(abPlusC)).toBeTruthy()

    expect(aPlusBC.toString()).toStrictEqual(
      'vertices [NWrapper 1, NWrapper 2, NWrapper 3]'
    )
    expect(aPlusBC.toString()).toStrictEqual(abPlusC.toString())

    expect(aPlusBC.getVertices().sort((a, b) => a.compare(b))).toStrictEqual(
      abPlusC.getVertices().sort((a, b) => a.compare(b))
    )

    expect(aPlusBC.getEdges().sort((a, b) => a.compare(b))).toStrictEqual(
      abPlusC.getEdges().sort((a, b) => a.compare(b))
    )
  })
})

describe('Algebraic Graphs - Distributive Laws', () => {
  test('Connect distributes over Overlay', () => {
    const va = vertex(new NumberWrapper(1))
    const vb = vertex(new NumberWrapper(2))
    const vc = vertex(new NumberWrapper(3))

    const a_times_b_plus_c = connect(va, overlay(vb, vc))
    const a_times_b_plus_a_times_c = overlay(connect(va, vb), connect(va, vc))

    expect(a_times_b_plus_c.toString()).toStrictEqual(
      'edges [(NWrapper 1, NWrapper 2), (NWrapper 1, NWrapper 3)]'
    )

    expect(a_times_b_plus_c.isEqualTo(a_times_b_plus_a_times_c)).toBeTruthy()
  })
})
