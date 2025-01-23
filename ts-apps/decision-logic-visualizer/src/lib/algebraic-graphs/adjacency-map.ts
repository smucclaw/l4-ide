import type { Eq, Ord } from './alga.ts'
import { Edge, UndirectedEdge, ComparisonResult } from './alga.ts'
import _ from 'lodash'

/**********************************************************
  Internal Adjacency Map implementation / representation
***********************************************************/

/*
Adapted from
https://github.com/snowleopard/alga/blob/main/src/Algebra/Graph/AdjacencyMap.hs
and https://github.com/snowleopard/alga/blob/main/src/Algebra/Graph/Undirected.hs

----------

The following are NOT real objects,
since the implementation assumes that the `other` graph is also implemented using an Adjacency Map.
I.e., this is really an ADT --- the use of object / class syntax is just to make the syntax more ergonomic.

We can try to make it auto-agnostic in the future;
I did it this way to follow the original implementation more closely.

----------

The implementations here can almost definitely be made more performant:
I was optimizing for correctness and readability, not performance.
*/

/** Adjacency Map implementation of undirected Alga Graph */
export type UndirectedGraph<A extends Ord> =
  | Empty<A>
  | Vertex<A>
  | Overlay<A>
  | Connect<A>

/** Note that the string representation of a DirectedEdge differs from that of an UndirectedEdge. */
export class DirectedEdge<A extends Ord> extends Edge<A> {
  constructor(u: A, v: A) {
    super(u, v)
  }

  isEqualTo(that: unknown): boolean {
    return (
      that instanceof DirectedEdge &&
      this.getU().isEqualTo(that.getU()) &&
      this.getV().isEqualTo(that.getV())
    )
  }

  toString(): string {
    return `<${this.getU()}, ${this.getV()}>`
  }
}

/** The adjacency map of a graph:
 * each vertex is associated with a set of its direct neighbors.

 * The base class is a *directed* graph. */
export class BaseAMGraph<A extends Ord> implements Eq {
  protected adjacencyMap: Map<A, Set<A>>

  constructor(adjacencyMap?: Map<A, Set<A>>) {
    this.adjacencyMap = adjacencyMap ?? new Map()
  }

  // Alga ops
  overlay(other: UndirectedGraph<A>): UndirectedGraph<A> {
    return new Overlay(this, other)
  }

  connect(other: UndirectedGraph<A>): UndirectedGraph<A> {
    return new Connect(this, other)
  }

  // Misc useful

  /** Check if a graph contains a given vertex. */
  hasVertex(vertex: A): boolean {
    return this.adjacencyMap.has(vertex)
  }

  /** Check if a graph contains a given edge. */
  hasEdge(u: A, v: A): boolean {
    const neighbors = this.adjacencyMap.get(u)
    if (neighbors) {
      return neighbors.has(v)
    }
    return false
  }

  /** Get a sorted array of vertices */
  getVertices(): A[] {
    return Array.from(this.adjacencyMap.keys()).sort((a, b) => a.compare(b))
  }

  /** Internal helper */
  protected getAllEdges(): Array<[A, A]> {
    const vertices = this.getVertices()

    // Get [ <v, n> for each vertex v, for each neighbor n of v]
    const allEdges = vertices.flatMap((vertex) => {
      const neighbors = this.adjacencyMap.get(vertex) ?? new Set<A>()
      return Array.from(neighbors).map(
        (neighbor) => [vertex, neighbor] as [A, A]
      )
    })

    return allEdges.toSorted(([u1, v1], [u2, v2]) => {
      const firstComparison = u1.compare(u2)
      return firstComparison !== ComparisonResult.Equal
        ? firstComparison
        : v1.compare(v2)
    })
  }

  /** Get a sorted array of the unique directed edges */
  getEdges(): DirectedEdge<A>[] {
    const directedEdges = this.getAllEdges().map(
      ([u, v]) => new DirectedEdge(u, v)
    )
    return directedEdges.toSorted((a, b) => a.compare(b))
  }

  isEqualTo(other: unknown) {
    if (!(other instanceof BaseAMGraph)) return false
    // TODO: Improve this!!
    return this.toString() === other.toString()
  }

  // Getters and setters for the underlying adjacency map
  getAdjMap() {
    return this.adjacencyMap
  }

  protected setAdjMap(map: typeof this.adjacencyMap) {
    this.adjacencyMap = map
  }

  /** Stringifies the internal representation. Currently for internal use. */
  toString(): string {
    const vertices = this.getVertices()
    const edges = this.getAllEdges().map((edge) => edge.toString())

    if (vertices.length === 0) return 'empty'
    if (edges.length === 0) return `vertices [${vertices.join(', ')}]`
    return `edges [${edges.join(', ')}]`
  }
}

export class UndirectedAMGraph<A extends Ord> extends BaseAMGraph<A> {
  /** Get a sorted array of the unique 'undirected' edges.
   *
   * (I.e., if (a, b) appears, (b, a) won't be present.) */
  getEdges(): UndirectedEdge<A>[] {
    const edges = this.getAllEdges().map(([u, v]) => new UndirectedEdge(u, v))

    return _.uniqWith(edges, (a, b) => a.isEqualTo(b)).toSorted((a, b) =>
      a.compare(b)
    )
  }
}
/*********************
     Primitives
**********************/

/** Empty graph */
export class Empty<A extends Ord> extends BaseAMGraph<A> {
  readonly tag = 'Empty'

  constructor() {
    super()
  }
}

/** The graph consisting of a single isolated vertex. */
export class Vertex<A extends Ord> extends BaseAMGraph<A> {
  readonly tag = 'Vertex'

  constructor(readonly value: A) {
    super(new Map([[value, new Set()]]))
  }

  getValue() {
    return this.value
  }
}

/** Convenience wrapper over Overlay ctor.
 *
 * overlay is analogous to +
 */
export function overlay<A extends Ord>(
  x: UndirectedGraph<A>,
  y: UndirectedGraph<A>
): UndirectedGraph<A> {
  return new Overlay(x, y)
}

export class Overlay<A extends Ord> extends BaseAMGraph<A> {
  readonly tag = 'Overlay'

  constructor(
    readonly left: BaseAMGraph<A>,
    readonly right: BaseAMGraph<A>
  ) {
    /*
    The idea, in terms of an abstract Relation isomorphic to (V, E),
      where data Relation a = R { domain :: Set a, relation :: Set (a, a) }
      is this.

      overlay x y = R (domain x `union` domain y) (relation x `union` relation y)

    Or in terms of the Haskell AdjacencyMap representation:
      overlay :: Ord a => AdjacencyMap a -> AdjacencyMap a -> AdjacencyMap a
      overlay (AM x) (AM y) = AM $ Map.unionWith Set.union x y
    */
    super(graphUnion(left.getAdjMap(), right.getAdjMap()))
  }
}

/** Convenience wrapper over Connect ctor.
 *
 * connect is analogous to *
 */
export function connect<A extends Ord>(
  x: UndirectedGraph<A>,
  y: UndirectedGraph<A>
): UndirectedGraph<A> {
  return new Connect(x, y)
}

export class Connect<A extends Ord> extends BaseAMGraph<A> {
  readonly tag = 'Connect'

  constructor(
    readonly from: BaseAMGraph<A>,
    readonly to: BaseAMGraph<A>
  ) {
    const fromAdjMap = from.getAdjMap()
    const toAdjMap = to.getAdjMap()

    // Union domains and relations
    const combinedMap = graphUnion(from.getAdjMap(), to.getAdjMap())

    // Then union with cartesian product of from's vertices and to's vertices
    const fromVertices = Array.from(fromAdjMap.keys())
    const toVertices = Array.from(toAdjMap.keys())

    appendVerticesToSourceNeighbors(combinedMap, fromVertices, toVertices)
    // Reciprocal connections for undirected graph
    appendVerticesToSourceNeighbors(combinedMap, toVertices, fromVertices)

    super(combinedMap)
  }
}

/*************************************
  Internal helper functions
***************************************/

/** Mutating */
function appendVerticesToSourceNeighbors<A extends Ord>(
  combinedMap: Map<A, Set<A>>,
  sourceVertices: A[],
  targetVertices: A[]
) {
  sourceVertices.forEach((vertex) => {
    const currNeighbors = combinedMap.get(vertex) || new Set<A>()
    const newNeighbors = setUnion(currNeighbors, new Set(targetVertices))
    combinedMap.set(vertex, newNeighbors)
  })
}

/** Union the domains and relations of two adj-map graphs */
function graphUnion<A extends Ord>(x: Map<A, Set<A>>, y: Map<A, Set<A>>) {
  const combinedMap = new Map(x)

  for (const [yVertex, yNeighbors] of y.entries()) {
    const xNeighbors = combinedMap.get(yVertex) ?? new Set()
    combinedMap.set(yVertex, setUnion(xNeighbors, yNeighbors))
  }
  return combinedMap
}

const setUnion = <A extends Ord>(set1: Set<A>, set2: Set<A>): Set<A> => {
  return new Set([...set1, ...set2])
}
