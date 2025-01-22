import type { Ord } from './alga.ts'
import { UndirectedEdge } from './alga.ts'
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

/** The adjacency map of a graph:
 * each vertex is associated with a set of its direct neighbors. */
export class BaseAMGraph<A extends Ord> {
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

  getVertices(): A[] {
    return Array.from(this.adjacencyMap.keys())
  }

  getEdges(): UndirectedEdge<A>[] {
    const vertices = this.getVertices()
    const edges = []

    for (const vertex of vertices) {
      const neighbors = Array.from(
        this.adjacencyMap.get(vertex) ?? (new Set() as Set<A>)
      )
      for (const neighbor of neighbors) {
        edges.push(new UndirectedEdge(vertex, neighbor))
      }
    }

    return _.uniqWith(edges, (a, b) => a.isEqualTo(b))
  }

  // Getters and setters for the underlying adjacency map
  getAdjMap() {
    return this.adjacencyMap
  }

  protected setAdjMap(map: typeof this.adjacencyMap) {
    this.adjacencyMap = map
  }

  toString(): string {
    const vertices = Array.from(this.adjacencyMap.keys())
    const edges: string[] = []

    this.adjacencyMap.forEach((set, vertex) => {
      set.forEach((neighbor) => {
        edges.push(`(${vertex}, ${neighbor})`)
      })
    })

    if (vertices.length === 0) return 'empty'
    if (edges.length === 0) return `vertices [${vertices.join(', ')}]`
    return `edges [${edges.join(', ')}]`
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

    fromVertices.forEach((fromVertex) => {
      const currNeigbors = combinedMap.get(fromVertex) || new Set<A>()
      const newNeighbors = setUnion(currNeigbors, new Set(toVertices))
      combinedMap.set(fromVertex, newNeighbors)
    })

    super(combinedMap)
  }
}

/*************************************
  Internal helper functions
***************************************/

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
