import { describe, test, expect } from 'vitest'
import {
  empty,
  vertex,
  overlay,
  connect,
  connectSinkToNode,
  pathFromValues,
} from '../lib/algebraic-graphs/dag.js'
import { NumberWrapper } from './number-wrapper.js'

const nws = [0, 1, 2, 3, 4, 5, 6].map((n) => new NumberWrapper(n))

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
  test('x * empty = x for dags)', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const x = connect(vertex(nw1), vertex(nw2))
    const emptyDag = empty<NumberWrapper>()

    const resultDag = connect(x, emptyDag) // x * empty
    expect(resultDag.isEqualTo(x)).toBeTruthy()
  })

  test('empty * x = x for dags', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const x = connect(vertex(nw1), vertex(nw2))
    const emptyDag = empty<NumberWrapper>()

    const resultDag = connect(emptyDag, x) // empty * x
    expect(resultDag.isEqualTo(x)).toBeTruthy()
  })

  test('x + empty = x for dags (overlay identity)', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const x = connect(vertex(nw1), vertex(nw2))
    const emptyDag = empty<NumberWrapper>()

    const resultDag = overlay(x, emptyDag) // x + empty
    expect(resultDag.isEqualTo(x)).toBeTruthy()
  })

  test('empty + x = x for dags (overlay identity)', () => {
    const nw1 = new NumberWrapper(1)
    const nw2 = new NumberWrapper(2)
    const x = connect(vertex(nw1), vertex(nw2))
    const emptyDag = empty<NumberWrapper>()

    const resultDag = overlay(emptyDag, x) // empty + x
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

  // test('Connecting new node to source makes the new node the source', () => {
  //   const nw1 = new NumberWrapper(1)
  //   const nw2 = new NumberWrapper(2)
  //   const nw0 = new NumberWrapper(0)

  //   // 1 -> 2
  //   const dag = vertex(nw1).connect(vertex(nw2))
  //   expect(dag.getSource()).toEqual(vertex(nw1))
  //   const updatedDag = connectVertexToSourceOf(nw0, dag)
  //   expect(updatedDag.getSource()).toEqual(vertex(nw0))
  // })

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
    expect(dag.toString()).toBe(
      'vertices [NWrapper 1, NWrapper 2]\n edges [<NWrapper 1,NWrapper 2>]'
    )
  })
})

/********************************************************************
  Note that the raw vertices
  can't just be a member of our pseudo Eq and Ord
  --- must also have the kind of referential equality that you want
**********************************************************************/

describe('the kind of equality in this alga mini-lib', () => {
  test('Alga: not enough for raw vertices to be pseudo Eq -- must also have the kind of referential equality you want', () => {
    const dag = pathFromValues([
      new NumberWrapper(1),
      new NumberWrapper(2),
      new NumberWrapper(3),
    ])
    const mappedDag = dag.gmap((a) => new NumberWrapper(a.getValueAsNumber()))

    expect(mappedDag.isEqualTo(dag)).toBeFalsy()
  })
})

/******************************************
       gmap (Graph Map)
*******************************************/

// TODO: I really should randomly generate graphs, or at least make a couple of different test graphs

describe('DAG - gmap (Graph Map)', () => {
  test('gmap with id should return the same (structurally speaking) graph', () => {
    const dag = pathFromValues([
      new NumberWrapper(1),
      new NumberWrapper(2),
      new NumberWrapper(3),
    ])

    const mappedDag = dag.gmap((a) => a)

    expect(mappedDag.isEqualTo(dag)).toBeTruthy()
  })

  test('gmap with a function should apply to all vertices', () => {
    const vertices = [nws[1], nws[2], nws[3]]
    const dag = pathFromValues(vertices)

    const mappedDag = dag.gmap((a) => nws[a.getValueAsNumber() + 1])

    const expectedVertices = [nws[2], nws[3], nws[4]]
    const expectedDag = pathFromValues(expectedVertices)

    expect(mappedDag.isEqualTo(expectedDag)).toBeTruthy()
  })

  test('gmap should satisfy functor composition law: map f (map g) x == map (f . g) x', () => {
    const dag = connect(
      vertex(new NumberWrapper(1)),
      vertex(new NumberWrapper(2))
    ).overlay(vertex(new NumberWrapper(3)))

    const f = (a: NumberWrapper) => new NumberWrapper(a.getValueAsNumber() + 1)
    const g = (a: NumberWrapper) => new NumberWrapper(a.getValueAsNumber() * 2)

    const composed = dag.gmap((a) => f(g(a)))
    const mappedSequentially = dag.gmap(g).gmap(f)

    expect(composed.isEqualTo(mappedSequentially)).toBeTruthy()
  })
})

/******************************************
        bind (FlatMap)
*******************************************/

describe('DAG - bind (FlatMap)', () => {
  test('bind with `vertex` function should replace vertices', () => {
    const dag = pathFromValues([nws[1], nws[2], nws[3]])

    const boundDag = dag.bind((a) => vertex(nws[a.getValueAsNumber() * 2]))

    const expectedDag = pathFromValues([nws[2], nws[4], nws[6]])

    expect(boundDag.isEqualTo(expectedDag)).toBeTruthy()
  })
})

/******************************************
         induce (a kind of filter)
*******************************************/

describe('DAG - induce using bind', () => {
  test('induce with predicate that always returns true should return the original graph', () => {
    const vertices = [
      new NumberWrapper(1),
      new NumberWrapper(2),
      new NumberWrapper(3),
    ]
    const dag = pathFromValues(vertices)

    const inducedDag = dag.induce(() => true)

    expect(inducedDag.isEqualTo(dag)).toBeTruthy()
  })

  test('induce with predicate that always returns false should return an empty graph', () => {
    const dag = pathFromValues([
      new NumberWrapper(1),
      new NumberWrapper(2),
      new NumberWrapper(3),
    ])

    const inducedDag = dag.induce(() => false)

    expect(inducedDag.getVertices().length).toStrictEqual(0)
  })

  test('induce can be used to remove a specific vertex', () => {
    const dag = pathFromValues([
      new NumberWrapper(1),
      new NumberWrapper(2),
      new NumberWrapper(3),
    ])

    const inducedDag = dag.induce((a) => a.getValueAsNumber() !== 2)

    const expectedDag = overlay(
      vertex(new NumberWrapper(1)),
      vertex(new NumberWrapper(3))
    )

    expect(inducedDag.isEqualTo(expectedDag)).toBeTruthy()
  })

  test('induce preserves edges between remaining vertices', () => {
    const dag = connect(
      vertex(new NumberWrapper(1)),
      connect(vertex(new NumberWrapper(2)), vertex(new NumberWrapper(3)))
    )

    const inducedDag = dag.induce((a) => a.getValueAsNumber() !== 2)

    const expectedDag = connect(
      vertex(new NumberWrapper(1)),
      vertex(new NumberWrapper(3))
    )

    expect(inducedDag.isEqualTo(expectedDag)).toBeTruthy()
  })

  test('induce with a complex predicate', () => {
    const dag =
      // (1 + 2) -> ( (3 -> 4) + 5 -> 6 )
      overlay(vertex(nws[1]), vertex(nws[2])).connect(
        connect(
          vertex(nws[3]),
          vertex(nws[4]).overlay(connect(vertex(nws[5]), vertex(nws[6])))
        )
      )
    const inducedDag = dag.induce((a) => a.getValueAsNumber() % 2 === 0)

    // 2 -> 4 + 2 -> 6
    const expectedDag = overlay(
      vertex(nws[2]).connect(vertex(nws[4])),
      vertex(nws[2]).connect(vertex(nws[6]))
    )

    expect(inducedDag.isEqualTo(expectedDag)).toBeTruthy()
  })
})
