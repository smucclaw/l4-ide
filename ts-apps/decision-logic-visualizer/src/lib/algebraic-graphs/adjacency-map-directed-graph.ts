import type { Eq, Ord } from '$lib/utils.ts'
import { ComparisonResult } from '$lib/utils.ts'
import { DirectedEdge } from './alga.ts'
import _ from 'lodash'

/**********************************************************
  Internal Adjacency Map implementation / representation for Directed Graphs
***********************************************************/

export type DirectedGraph<A extends Ord<A>> =
  | InstanceType<typeof Empty<A>>
  | InstanceType<typeof Vertex<A>>
  | InstanceType<typeof Overlay<A>>
  | InstanceType<typeof Connect<A>>

// Ignore the InstanceType<typeof ... syntax
// It's morally equivalent to Empty | Vertex | ...

/** The adjacency map of a graph:
 * each vertex is associated with a set of its direct neighbors.

 * This base class for AM graphs is a *directed* graph.
 * We get the *un*directed graph by doing extra stuff with Connect / adding commutativity to Connect.
 * (There's also a minor difference with `getEdges`, but that's more of a convenience feature)
 */
export class DirectedAMGraph<A extends Ord<A>>
  implements Eq<DirectedAMGraph<A>>
{
  protected adjacencyMap: Map<A, Set<A>>

  constructor(adjacencyMap?: Map<A, Set<A>>) {
    this.adjacencyMap = adjacencyMap ?? new Map()
  }

  // Alga ops
  overlay(other: DirectedAMGraph<A>): DirectedAMGraph<A> {
    return new Overlay(this, other)
  }

  connect(other: DirectedAMGraph<A>): DirectedAMGraph<A> {
    return new DirectedAMGraph(makeDirectedConnectAdjacencyMap(this, other))
  }

  // Getters and setters for the underlying adjacency map
  getAdjMap() {
    return this.adjacencyMap
  }

  protected setAdjMap(map: typeof this.adjacencyMap) {
    this.adjacencyMap = map
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

  /** Internal helper: Get a sorted array of *all* the [A, A] edges */
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

  isEqualTo(other: this): boolean {
    const isEqToPredicate = (
      x: DirectedAMGraph<A>,
      y: DirectedAMGraph<A>
    ): boolean => {
      return x.isEqualTo(y)
    }
    return (
      other instanceof DirectedAMGraph &&
      _.isEqualWith(this.getVertices(), other.getVertices(), isEqToPredicate) &&
      _.isEqualWith(this.getAllEdges(), other.getAllEdges(), isEqToPredicate)
    )
  }

  /** Pretty prints the *un*directed edges for undirected graphs (and directed for directed graphs)  */
  pPrint(): string {
    const vertices = this.getVertices()
    const edges = this.getEdges().map((edge) => edge.toString())

    if (vertices.length === 0) return 'empty'
    if (edges.length === 0) return `vertices [${vertices.join(', ')}]`
    return `edges [${edges.join(', ')}]`
  }

  /** Stringifies the internal representation. Currently for internal use. */
  toString(): string {
    const vertices = this.getVertices()
    const edges = this.getAllEdges().map((edge) => `<${edge.toString()}>`)

    if (vertices.length === 0) return 'empty'
    if (edges.length === 0) return `vertices [${vertices.join(', ')}]`
    return `edges [${edges.join(', ')}]`
  }
}

/*********************
     Primitives
**********************/

export function empty<A extends Ord<A>>() {
  return new Empty<A>()
}

/** Empty graph */
export const Empty = class<A extends Ord<A>> extends DirectedAMGraph<A> {
  constructor() {
    super()
  }
}

export function vertex<A extends Ord<A>>(a: A) {
  return new Vertex(a)
}

/** The graph consisting of a single isolated vertex. */
export const Vertex = class<A extends Ord<A>> extends DirectedAMGraph<A> {
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
export function overlay<A extends Ord<A>>(
  x: DirectedAMGraph<A>,
  y: DirectedAMGraph<A>
): DirectedAMGraph<A> {
  return new Overlay(x, y)
}

export const Overlay = class<A extends Ord<A>> extends DirectedAMGraph<A> {
  constructor(
    readonly left: DirectedAMGraph<A>,
    readonly right: DirectedAMGraph<A>
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

export const Connect = class<A extends Ord<A>> extends DirectedAMGraph<A> {
  constructor(
    readonly from: DirectedAMGraph<A>,
    readonly to: DirectedAMGraph<A>
  ) {
    super(makeDirectedConnectAdjacencyMap(from, to))
  }
}

export function makeDirectedConnectAdjacencyMap<A extends Ord<A>>(
  from: DirectedAMGraph<A>,
  to: DirectedAMGraph<A>
) {
  // Union domains and relations
  const combinedMap = graphUnion(from.getAdjMap(), to.getAdjMap())

  // Then union with cartesian product of from's vertices and to's vertices
  const fromVertices = Array.from(from.getAdjMap().keys())
  const toVertices = Array.from(to.getAdjMap().keys())
  appendVerticesToSourceNeighbors(combinedMap, fromVertices, toVertices)
  return combinedMap
}

/*************************************
        Helper functions
***************************************/

/** Mutating */
export function appendVerticesToSourceNeighbors<A extends Ord<A>>(
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
export function graphUnion<A extends Ord<A>>(
  x: Map<A, Set<A>>,
  y: Map<A, Set<A>>
) {
  const combinedMap = new Map(x)

  for (const [yVertex, yNeighbors] of y.entries()) {
    const xNeighbors = combinedMap.get(yVertex) ?? new Set()
    combinedMap.set(yVertex, setUnion(xNeighbors, yNeighbors))
  }
  return combinedMap
}

const setUnion = <A extends Ord<A>>(set1: Set<A>, set2: Set<A>): Set<A> => {
  return new Set([...set1, ...set2])
}
