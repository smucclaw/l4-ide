import { describe, test, expect } from 'vitest'
import { empty, vertex, overlay, connect } from '../lib/algebraic-graphs/alga'
import { ComparisonResult } from '../lib/utils'
import type { Ord } from '../lib/utils'

class NumberWrapper implements Ord<NumberWrapper> {
  constructor(private value: number) {}

  isEqualTo(other: NumberWrapper): boolean {
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

describe('Algebraic graphs -- Overlay Commutativity', () => {
  test('Commutativity of overlay for simple vertices', () => {
    const va = vertex(new NumberWrapper(1))
    const vb = vertex(new NumberWrapper(2))

    const aPlusB = overlay(va, vb)
    const bPlusA = overlay(vb, va)

    expect(aPlusB.isEqualTo(bPlusA)).toBeTruthy()
  })
})

describe('Algebraic graphs - Overlay Associativity', () => {
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

describe('Algebraic graphs - Distributive Laws', () => {
  test('Connect distributes over Overlay', () => {
    const va = vertex(new NumberWrapper(1))
    const vb = vertex(new NumberWrapper(2))
    const vc = vertex(new NumberWrapper(3))

    const a_times_b_plus_c = connect(va, overlay(vb, vc))
    const a_times_b_plus_a_times_c = overlay(connect(va, vb), connect(va, vc))

    expect(a_times_b_plus_c.toString()).toStrictEqual(
      'edges [<NWrapper 1,NWrapper 2>, <NWrapper 1,NWrapper 3>, <NWrapper 2,NWrapper 1>, <NWrapper 3,NWrapper 1>]'
    )
    expect(a_times_b_plus_c.pPrint()).toStrictEqual(
      'edges [(NWrapper 1, NWrapper 2), (NWrapper 1, NWrapper 3)]'
    )

    expect(a_times_b_plus_c.isEqualTo(a_times_b_plus_a_times_c)).toBeTruthy()
  })
})

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
