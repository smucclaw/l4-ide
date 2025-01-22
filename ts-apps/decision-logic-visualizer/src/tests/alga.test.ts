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
    ).toEqual(
      bPlusA
        .getVertices()
        .sort((a: NumberWrapper, b: NumberWrapper) => a.compare(b))
    )
    expect(
      bPlusA
        .getVertices()
        .sort((a: NumberWrapper, b: NumberWrapper) => a.compare(b))
    ).toEqual(
      aPlusB
        .getVertices()
        .sort((a: NumberWrapper, b: NumberWrapper) => a.compare(b))
    )
  })
})
