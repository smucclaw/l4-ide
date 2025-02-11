import { describe, test, expect } from 'vitest'
import {
  empty,
  vertex,
  overlay,
  connect,
  connectNodeToSource,
  connectSinkToNode,
} from '../lib/algebraic-graphs/dag.js' // Adjust the import path accordingly
import type { Ord, HasId } from '$lib/utils.js'
import { ComparisonResult } from '$lib/utils.js'

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

/*******************
      Overlay
********************/

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

/*******************
    Connect
********************/

describe('Alga: Connect', () => {
  test('Connect two vertices should create an edge from first to second', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const dag = vertex(nw1).connect(vertex(nw2))
    expect(dag.getVertices()).toEqual(expect.arrayContaining([nw1, nw2]))
    expect(dag.getEdges().map((e) => [e.getU(), e.getV()])).toEqual([
      [nw1, nw2],
    ])
  })

  test('Connect is not commutative for directed graphs', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const dag1 = connect(vertex(nw1), vertex(nw2))
    const dag2 = connect(vertex(nw2), vertex(nw1))
    expect(dag1.isEqualTo(dag2)).toBeFalsy()
  })
})

describe('Alga: DAG - Other Basic Operations', () => {
  test('Empty DAG should have no vertices or edges', () => {
    const dag = empty<NumberWrapper>()
    expect(dag.getVertices()).toEqual([])
    expect(dag.getEdges()).toEqual([])
  })

  test('Vertex DAG should have one vertex and no edges', () => {
    const nw1 = new NumberWrapper(1)
    const dag = vertex(nw1)
    expect(dag.getVertices()).toEqual([nw1])
    expect(dag.getEdges()).toEqual([])
  })

  test('Overlay of two vertices should contain both vertices', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const da = vertex(nw1)
    const db = vertex(nw2)
    const dag = da.overlay(db)
    expect(dag.getVertices()).toEqual(expect.arrayContaining([nw1, nw2]))
    expect(dag.getEdges()).toEqual([])
  })
})

/*******************
  Distributivity
********************/

describe('Algebraic graphs - Distributive Laws', () => {
  test('Connect distributes over Overlay', () => {
    const va = vertex(new NumberWrapper(1))
    const vb = vertex(new NumberWrapper(2))
    const vc = vertex(new NumberWrapper(3))

    const a_times_b_plus_c = connect(va, overlay(vb, vc))
    const a_times_b_plus_a_times_c = overlay(connect(va, vb), connect(va, vc))

    expect(a_times_b_plus_c.isEqualTo(a_times_b_plus_a_times_c)).toBeTruthy()
  })
})

/*******************
      Monoid
********************/

describe('Alga: DAG - Monoid Laws', () => {
  test('x -> empty = x for dags)', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const x = connect(vertex(nw1), vertex(nw2))

    // Empty DAG
    const emptyDag = empty<NumberWrapper>()

    // x -> empty
    const resultDag = connect(x, emptyDag)

    // resultDag should be equal to x
    expect(resultDag.isEqualTo(x)).toBeTruthy()
  })

  test('empty -> x = x for dags', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const x = connect(vertex(nw1), vertex(nw2))

    // Empty DAG
    const emptyDag = empty<NumberWrapper>()

    // empty -> x
    const resultDag = connect(emptyDag, x)

    // resultDag should be equal to x
    expect(resultDag.isEqualTo(x)).toBeTruthy()
  })
})

/******************************************
      DAG specific tests
*******************************************/

describe('DAG - Topological Sort', () => {
  test('Simple DAG topological sort', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)
    const dag = connect(vertex(nw3), connect(vertex(nw2), vertex(nw1)))
    const topSort = dag.getTopSort()
    expect(topSort).toEqual([nw3, nw2, nw1])
  })

  test('getSource should return the first vertex in topological order', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const dag = vertex(nw1).connect(vertex(nw2))
    expect(dag.getSource()).toEqual(vertex(nw1))
  })

  test('getSink should return the last vertex in topological order', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)
    const dag = connect(vertex(nw1), connect(vertex(nw2), vertex(nw3)))
    expect(dag.getSink()).toEqual(vertex(nw3))
  })

  test('Connecting sink to new node makes the new node the sink', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)

    // 1 -> 2
    const dag = connect(vertex(nw1), vertex(nw2))
    expect(dag.getSink()).toEqual(vertex(nw2))
    const updatedDag = connectSinkToNode(dag, nw3)
    expect(updatedDag.getSink()).toEqual(vertex(nw3))
  })

  test('Connecting new node to source makes the new node the source', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw0 = new NumberWrapper(0)

    // 1 -> 2
    const dag = vertex(nw1).connect(vertex(nw2))
    expect(dag.getSource()).toEqual(vertex(nw1))
    const updatedDag = connectNodeToSource(dag, nw0)
    expect(updatedDag.getSource()).toEqual(vertex(nw0))
  })

  test('Topological sort throws error on cyclic graph', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const dag1 = connect(vertex(nw1), vertex(nw2))
    const dag2 = connect(vertex(nw2), vertex(nw1))
    const cyclicDag = overlay(dag1, dag2)
    expect(() => cyclicDag.getTopSort()).toThrow()
  })
})

describe('DAG - Equality and Structure', () => {
  test('isEqualTo (from the Eq interface) is structural equality', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)

    const dag1 = connect(vertex(nw1), vertex(nw2))
    const dag2 = connect(vertex(nw1), vertex(nw2))
    expect(dag1.isEqualTo(dag2)).toBeTruthy()
  })

  test('Differently directed edges make DAGs not equal', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)

    const dag1 = connect(vertex(nw1), vertex(nw2))
    const dag2 = connect(vertex(nw2), vertex(nw1))
    expect(dag1.isEqualTo(dag2)).toBeFalsy()
  })
})

describe('Alga: DAG - Complex Structures', () => {
  test('Overlay of DAGs combines vertices without adding edges', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)

    const dag1 = vertex(nw1)
    const dag2 = connect(vertex(nw2), vertex(nw3))
    const combinedDag = overlay(dag1, dag2)

    expect(combinedDag.getVertices()).toEqual(
      expect.arrayContaining([nw1, nw2, nw3])
    )
    expect(combinedDag.getEdges().map((e) => [e.getU(), e.getV()])).toEqual([
      [nw2, nw3],
    ])
  })

  test('Connecting two DAGs merges their structures', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)
    const nw4 = new NumberWrapper(4)

    const dag1 = connect(vertex(nw1), vertex(nw2))
    const dag2 = connect(vertex(nw3), vertex(nw4))
    const connectedDag = connect(dag1, dag2)
    expect(connectedDag.getVertices()).toEqual(
      expect.arrayContaining([nw1, nw2, nw3, nw4])
    )
    expect(connectedDag.getEdges().map((e) => [e.getU(), e.getV()])).toEqual([
      [nw1, nw2],
      [nw1, nw3],
      [nw1, nw4],
      [nw2, nw3],
      [nw2, nw4],
      [nw3, nw4],
    ])

    const topSort = connectedDag.getTopSort()
    expect(topSort).toEqual([nw1, nw2, nw3, nw4])
  })

  test('Connecting DAGs with overlay', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)

    const dag1 = connect(vertex(nw1), vertex(nw2))
    const dag2 = connect(vertex(nw1), vertex(nw3))
    const dag = overlay(dag1, dag2)

    expect(dag.getEdges().map((e) => [e.getU(), e.getV()])).toEqual([
      [nw1, nw2],
      [nw1, nw3],
    ])

    const topSort = dag.getTopSort()
    expect(topSort).toEqual([nw1, nw2, nw3])
  })

  test('Cyclic graph detection in complex structures', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)

    const dag1 = connect(vertex(nw1), vertex(nw2))
    const dag2 = connect(vertex(nw2), vertex(nw3))
    const backEdge = connect(vertex(nw3), vertex(nw1))
    const cyclicDag = overlay(dag1, overlay(dag2, backEdge))

    expect(() => cyclicDag.getTopSort()).toThrow()
  })
})

describe('Alga: DAG - Distributive Laws', () => {
  test('Connect distributes over Overlay', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const nw3 = new NumberWrapper(3)

    const left = connect(vertex(nw1), overlay(vertex(nw2), vertex(nw3)))
    const right = overlay(
      connect(vertex(nw1), vertex(nw2)),
      connect(vertex(nw1), vertex(nw3))
    )

    expect(left.isEqualTo(right)).toBeTruthy()
  })
})

describe('DAG - toString', () => {
  test('toString', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const dag = connect(vertex(nw1), vertex(nw2))
    expect(dag.toString()).toBe('edges [<NWrapper 1,NWrapper 2>]')
  })
})
