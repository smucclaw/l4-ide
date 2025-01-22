import { describe, test, expect } from 'vitest'
import {
  vertex,
  edge,
  overlay,
  connect,
  empty,
  ComparisonResult,
} from '../lib/algebraic-graphs/alga'
import type { Ord } from '../lib/algebraic-graphs/alga'
import _ from 'lodash'

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

    expect(
      aPlusB
        .getVertices()
        .sort((a: NumberWrapper, b: NumberWrapper) => a.compare(b))
    ).toStrictEqual(
      bPlusA
        .getVertices()
        .sort((a: NumberWrapper, b: NumberWrapper) => a.compare(b))
    )
    expect(
      bPlusA
        .getVertices()
        .sort((a: NumberWrapper, b: NumberWrapper) => a.compare(b))
    ).toStrictEqual(
      aPlusB
        .getVertices()
        .sort((a: NumberWrapper, b: NumberWrapper) => a.compare(b))
    )
  })
})

describe('Algebraic Graphs - Overlay Associativity', () => {
  test('Overlay is associative', () => {
    const va = vertex(new NumberWrapper(1))
    const vb = vertex(new NumberWrapper(2))
    const vc = vertex(new NumberWrapper(3))

    const aPlusBC = overlay(va, overlay(vb, vc))
    const abPlusC = overlay(overlay(va, vb), vc)

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
