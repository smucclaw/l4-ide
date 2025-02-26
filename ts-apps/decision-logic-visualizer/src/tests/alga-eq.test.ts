import { describe, test, expect } from 'vitest'
import { empty, vertex, overlay } from '../lib/algebraic-graphs/dag.js'
import { ComparisonResult } from '../lib/utils.js'
import type { Ord, HasId } from '../lib/utils.js'

class NumberWrapper implements Ord<NumberWrapper>, HasId {
  constructor(private value: number) {}

  isEqualTo(other: NumberWrapper): boolean {
    return other instanceof NumberWrapper && this.value === other.value
  }

  compare(other: NumberWrapper): ComparisonResult {
    if (this.value < other.value) return ComparisonResult.LessThan
    if (this.value > other.value) return ComparisonResult.GreaterThan
    return ComparisonResult.Equal
  }

  getId(): string {
    return this.value.toString()
  }

  toString(): string {
    return `NWrapper ${this.value}`
  }
}

describe('Algebraic graphs -- isEqualTo and empty', () => {
  test('isEqualTo is structural equality, with empty vs. empty', () => {
    expect(
      empty<NumberWrapper>().isEqualTo(empty<NumberWrapper>())
    ).toBeTruthy()
  })

  test('Empty not == Vertex', () => {
    expect(
      // Interesting that the type checker permits this -- I guess it's because of structural typing
      // TODO: Refactor Eq (e.g. to use branded types?) and remove this test
      empty<NumberWrapper>().isEqualTo(vertex(new NumberWrapper(1)))
    ).toBeFalsy()
  })
})

describe('Algebraic graphs -- more isEqualTo', () => {
  test('isEqualTo for equivalent overlays', () => {
    const va1 = vertex(new NumberWrapper(1))
    const va2 = vertex(new NumberWrapper(1))
    const vb = vertex(new NumberWrapper(2))

    const overlay1 = overlay(va1, vb)
    const overlay2 = overlay(vb, va2)

    expect(overlay1.isEqualTo(overlay2)).toBeTruthy()
  })

  test('isEqualTo for non-equiv overlays', () => {
    const va = vertex(new NumberWrapper(1))
    const vb1 = vertex(new NumberWrapper(2))
    const vb2 = vertex(new NumberWrapper(3))

    const overlay1 = overlay(va, vb1) // 1 + 2
    const overlay2 = overlay(va, vb2) // 1 + 3

    expect(overlay1.isEqualTo(overlay2)).toBeFalsy()
  })
})
