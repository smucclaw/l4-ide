import { describe, test, expect } from 'vitest'
import {
  empty,
  edge,
  vertex,
  overlay,
  connect,
  pathFromValues,
  graphFromEdges,
} from '../lib/algebraic-graphs/alga.js'
import { ComparisonResult } from '../lib/utils.js'
import type { Ord } from '../lib/utils.js'

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

describe('Algebraic graphs -- pathFromValues helper for making undirected path graph', () => {
  test('Empty path should return an empty graph', () => {
    const g = pathFromValues<NumberWrapper>([])
    expect(g.isEqualTo(empty<NumberWrapper>())).toBeTruthy()
    expect(g.getVertices()).toStrictEqual([])
    expect(g.getEdges()).toStrictEqual([])
  })

  test('Path with one vertex should return a graph with that vertex', () => {
    const nw1 = new NumberWrapper(1)
    const g = pathFromValues([nw1])
    expect(g.isEqualTo(vertex(nw1))).toBeTruthy()
    expect(g.getVertices()).toStrictEqual([nw1])
    expect(g.getEdges()).toStrictEqual([])
  })

  test('Path with two vertices should return a graph with an edge between them', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const g = pathFromValues([nw1, nw2])
    const expectedGraph = edge(nw1, nw2)
    expect(g.isEqualTo(expectedGraph)).toBeTruthy()
  })

  test('Path with multiple vertices should return a graph forming a path', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)
    const nw4 = new NumberWrapper(4)
    const g = pathFromValues([nw1, nw2, nw3, nw4])
    // Expected edges are (1,2), (2,3), (3,4)
    const expectedGraph = [
      edge(nw1, nw2),
      edge(nw2, nw3),
      edge(nw3, nw4),
    ].reduce(overlay)
    expect(g.isEqualTo(expectedGraph)).toBeTruthy()
    expect(g.toString()).toStrictEqual(
      'edges [<NWrapper 1,NWrapper 2>, <NWrapper 2,NWrapper 1>, <NWrapper 2,NWrapper 3>, <NWrapper 3,NWrapper 2>, <NWrapper 3,NWrapper 4>, <NWrapper 4,NWrapper 3>]'
    )
  })

  test('Simple path graph with duplicate vertices 1 -> 2 -> 1', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const g = pathFromValues([nw1, nw2, nw1])
    const expectedGraph = overlay(edge(nw1, nw2), edge(nw2, nw1))
    expect(g.isEqualTo(expectedGraph)).toBeTruthy()
  })
})

describe('Algebraic graphs -- graphFromEdges function', () => {
  test('graphFromEdges with empty list should return an empty graph', () => {
    const g = graphFromEdges<NumberWrapper>([])
    expect(g.isEqualTo(empty<NumberWrapper>())).toBeTruthy()
  })

  test('graphFromEdges with single edge should return that edge', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const g = graphFromEdges([[nw1, nw2]])
    const expectedGraph = edge(nw1, nw2)
    expect(g.isEqualTo(expectedGraph)).toBeTruthy()
  })

  test('graphFromEdges with multiple edges should be equivalent to overlaying those edges', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)
    const edges = [
      [nw1, nw2],
      [nw2, nw3],
    ] as [NumberWrapper, NumberWrapper][]

    const g = graphFromEdges(edges)
    const expectedGraph = overlay(edge(nw1, nw2), edge(nw2, nw3))
    expect(g.isEqualTo(expectedGraph)).toBeTruthy()
  })

  test('edgeCount of graphFromEdges equals length of unique edges', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)
    const edges = [
      [nw1, nw2],
      [nw2, nw3],
      [nw1, nw2],
    ] as [NumberWrapper, NumberWrapper][]

    const g = graphFromEdges(edges)
    const uniqueEdges = [edge(nw1, nw2), edge(nw2, nw3)]

    expect(g.getEdges().length).toBe(uniqueEdges.length)
  })
})
