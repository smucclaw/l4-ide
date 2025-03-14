import type { Eq, Ord } from '$lib/utils.js'
import {
  DirectedEdge,
  stringifyEdge as makeEdgeKey,
  type Edge,
  type EdgeAttributes,
  DefaultEdgeAttributes,
} from './edge.js'
import { BaseAMGraph } from './base-adjacency-map.js'

/****************************************************************************
  Internal Adjacency Map implementation / representation for Directed Graphs
*****************************************************************************/

export type DirectedGraph<A extends Ord<A>> =
  | Empty<A>
  | Vertex<A>
  | Overlay<A>
  | Connect<A>

export type EdgeAttributeMap<A extends Ord<A>> = Map<string, EdgeAttributes>

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
  constructor(
    adjacencyMap?: Map<A, Set<A>>,
    protected edgeAttributes: EdgeAttributeMap<A> = new Map<
      string,
      EdgeAttributes
    >()
  ) {
    super(adjacencyMap)
  }
  // Alga ops
  overlay(other: DirectedAMGraph<A>): DirectedAMGraph<A> {
    return new Overlay(this, other)
  }

  connect(other: DirectedAMGraph<A>): DirectedAMGraph<A> {
    return new Connect(this, other)
  }

  // Misc useful

  /** Get a (lexicographically) sorted array of the unique directed edges */
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

  // Getting / setting edge attributes

  getAttributesForEdge<T extends Edge<A>>(edge: T): EdgeAttributes {
    const edgeKey = makeEdgeKey(edge)
    const attrs = this.edgeAttributes.get(edgeKey)
    if (attrs) {
      return attrs.clone()
    } else {
      this.edgeAttributes.set(edgeKey, new DefaultEdgeAttributes())
      return new DefaultEdgeAttributes() as EdgeAttributes
    }
  }

  /** Will error if the input edge does not exist.
   */
  setEdgeAttributes<T extends Edge<A>>(edge: T, newAttr: EdgeAttributes) {
    if (!this.hasEdge(edge.getU(), edge.getV())) {
      throw new Error(
        `setEdgeAttribute: Edge (${edge.getU()}, ${edge.getV()}) does not exist`
      )
    }
    this.edgeAttributes.set(makeEdgeKey(edge), newAttr)
  }

  /** Internal */
  _getEdgeAttributesMap() {
    return this.edgeAttributes
  }

  dispose() {
    super.dispose() // Clears the adj map
    this.edgeAttributes.clear()
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
    const { adjMap, edgeAttrs } = mergeDirectedGraphs(left, right)
    super(adjMap, edgeAttrs)
  }

  dispose() {
    this.left.dispose()
    this.right.dispose()
  }
}

export class Connect<A extends Ord<A>> extends DirectedAMGraph<A> {
  constructor(
    readonly from: DirectedAMGraph<A>,
    readonly to: DirectedAMGraph<A>
  ) {
    const adjMap = makeDirectedConnectAdjacencyMap(from, to) as Map<A, Set<A>>
    const edgeAttributes = mergeEdgeAttributeMaps(
      from._getEdgeAttributesMap(),
      to._getEdgeAttributesMap()
    )
    super(adjMap, edgeAttributes)
  }

  dispose() {
    this.from.dispose()
    this.to.dispose()
  }
}

/**
 * Creates a new adjacency map by
 * unioning the graphs represented by the two argument adjacency maps and
 * connecting every vertex in the "from" adjacency map to every vertex in the "to" adjacency map.
 *
 * @param fromAdjMap The adjacency map representing the "from" graph.
 * @param toAdjMap The adjacency map representing the "to" graph.
 * @returns A new adjacency map representing the connected graphs.
 */
export function makeDirectedConnectAdjacencyMapFromAdjMaps<A extends Ord<A>>(
  from: Map<A, Set<A>>,
  to: Map<A, Set<A>>
): Map<A, Set<A>> {
  // Union domains and relations
  const combinedMap = graphUnion(from, to)

  // Then union with cartesian product of from's vertices and to's vertices
  const fromVertices = Array.from(from.keys())
  const toVertices = Array.from(to.keys())
  appendVerticesToSourceNeighbors(combinedMap, fromVertices, toVertices)
  return combinedMap
}

export function makeDirectedConnectAdjacencyMap<
  A extends Ord<A>,
  T extends DirectedAMGraph<A>,
>(from: T, to: T): Map<A, Set<A>> {
  return makeDirectedConnectAdjacencyMapFromAdjMaps(
    from.getAdjMap(),
    to.getAdjMap()
  )
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

export function mergeEdgeAttributeMaps<A extends Ord<A>>(
  left: EdgeAttributeMap<A>,
  right: EdgeAttributeMap<A>
): EdgeAttributeMap<A> {
  const merged = new Map(left)

  for (const [edge, rightAttributes] of right.entries()) {
    const leftAttributes = merged.get(edge) || new DefaultEdgeAttributes()
    merged.set(edge, leftAttributes.merge(rightAttributes))
  }

  return merged
}

/** Merge two directed adjacency maps and their edge-attribute maps.
 */
export function mergeDirectedGraphs<A extends Ord<A>>(
  g1: DirectedAMGraph<A>,
  g2: DirectedAMGraph<A>
): {
  adjMap: Map<A, Set<A>>
  edgeAttrs: EdgeAttributeMap<A>
} {
  return {
    adjMap: graphUnion(g1.getAdjMap(), g2.getAdjMap()),
    edgeAttrs: mergeEdgeAttributeMaps(
      g1._getEdgeAttributesMap(),
      g2._getEdgeAttributesMap()
    ),
  }
}

/** Union the domains and relations of two adj-map graphs,
but without taking the EdgeAttributes into account. */
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
