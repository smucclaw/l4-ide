import type { Eq, Ord } from '$lib/utils.js'
import { DirectedEdge } from './alga.js'
import { BaseAMGraph } from './base-adjacency-map.js'

/****************************************************************************
  Internal Adjacency Map implementation / representation for Directed Graphs
*****************************************************************************/

export type DirectedGraph<A extends Ord<A>> =
  | Empty<A>
  | Vertex<A>
  | Overlay<A>
  | Connect<A>

// Ignore the InstanceType<typeof ... syntax
// It's morally equivalent to Empty | Vertex | ...

/** The adjacency map of a graph:
 * each vertex is associated with a set of its direct neighbors.

 * This base class for AM graphs is a *directed* graph.
 * We get the *un*directed graph by doing extra stuff with Connect / adding commutativity to Connect.
 * (There's also a minor difference with `getEdges`, but that's more of a convenience feature)
 */
export class DirectedAMGraph<A extends Ord<A>>
  extends BaseAMGraph<A>
  implements Eq<DirectedAMGraph<A>>
{
  constructor(adjacencyMap?: Map<A, Set<A>>) {
    super(adjacencyMap)
  }
  // Alga ops
  overlay(other: DirectedAMGraph<A>): DirectedAMGraph<A> {
    return new Overlay(this, other)
  }

  connect(other: DirectedAMGraph<A>): DirectedAMGraph<A> {
    return new DirectedAMGraph(makeDirectedConnectAdjacencyMap(this, other))
  }

  // Misc useful

  /** Get a sorted array of the unique directed edges */
  getEdges(): DirectedEdge<A>[] {
    const directedEdges = this.getAllEdges().map(
      ([u, v]) => new DirectedEdge(u, v)
    )
    return directedEdges.toSorted((a, b) => a.compare(b))
  }

  isEqualTo<T extends DirectedAMGraph<A>>(other: T): boolean {
    // TODO: Improve this!
    return this.toString() === other.toString()
  }
}

/*********************
     Primitives
**********************/

export function empty<A extends Ord<A>>() {
  return new Empty<A>()
}

/** Empty graph */
export class Empty<A extends Ord<A>> extends DirectedAMGraph<A> {
  constructor() {
    super()
  }
}

export function vertex<A extends Ord<A>>(a: A) {
  return new Vertex(a)
}

/** The graph consisting of a single isolated vertex. */
export class Vertex<A extends Ord<A>> extends DirectedAMGraph<A> {
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

export class Overlay<A extends Ord<A>> extends DirectedAMGraph<A> {
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

export class Connect<A extends Ord<A>> extends DirectedAMGraph<A> {
  constructor(
    readonly from: DirectedAMGraph<A>,
    readonly to: DirectedAMGraph<A>
  ) {
    super(makeDirectedConnectAdjacencyMap(from, to))
  }
}

export function makeDirectedConnectAdjacencyMap<
  A extends Ord<A>,
  T extends BaseAMGraph<A>,
>(from: T, to: T): Map<A, Set<A>> {
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
