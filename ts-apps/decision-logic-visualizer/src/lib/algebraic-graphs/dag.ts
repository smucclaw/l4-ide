import type { Eq, Ord, HasId } from '$lib/utils.js'
import {
  DirectedAMGraph,
  graphUnion,
  makeDirectedConnectAdjacencyMapFromAdjMaps,
} from './adjacency-map-directed-graph.js'
import * as GY from 'graphology'
import { topologicalSort } from 'graphology-dag'
import { match } from 'ts-pattern'

/*
TODO: There is currently a fair bit of code duplication between the various kinds of alga graphs in this mini-lib.
Would be good to improve that.
*/

export type DirectedAcyclicGraph<A extends Ord<A> & HasId> =
  | Empty<A>
  | Vertex<A>
  | Overlay<A>
  | Connect<A>

/** Directed Acyclic Graph
*
* TODO: This currently isn't the safest,
in that we only check if it's a DAG when topSort is called
*/
export abstract class Dag<A extends Ord<A> & HasId>
  extends DirectedAMGraph<A>
  implements Eq<Dag<A>>
{
  /** A cached topological ordering */
  private topologicalOrdering?: A[]

  constructor(adjacencyMap?: Map<A, Set<A>>) {
    super(adjacencyMap)
  }

  override overlay(other: DirectedAcyclicGraph<A>): DirectedAcyclicGraph<A> {
    return new Overlay(this, other)
  }

  override connect(other: DirectedAcyclicGraph<A>): DirectedAcyclicGraph<A> {
    return new Connect(this, other)
  }

  /** Errors if not a DAG */
  getTopSort(): Array<A> {
    if (this.topologicalOrdering) return this.topologicalOrdering as A[]

    // TODO: Might be better to construct this from the start?

    const { idToVertex, graph } = gyGraphFromAdjacencyMap(this.getAdjMap())

    const topSort = topologicalSort(graph)
    this.topologicalOrdering = topSort.map((id) => idToVertex.get(id)) as A[]

    return this.topologicalOrdering as A[]
  }

  /**
   * Returns the unique source, if there is only one source.
   * No guarantees if there is more than one source.
   *
   * Time complexity: O(|V| + |E|) because naively implemented
   */
  getSource(): Vertex<A> | Empty<A> {
    const sources = Array.from(this.getSources())

    return match(sources)
      .with([], () => empty<A>())
      .otherwise(() => vertex(sources[0]))
  }

  /**
   * Returns the unique sink, if there is only one sink.
   * No guarantees if there is more than one sink.
   *
   * Time complexity: O(|V| + |E|) because naively implemented
   */
  getSink(): Vertex<A> | Empty<A> {
    const sinks = Array.from(this.getSinks())

    return match(sinks)
      .with([], () => empty<A>())
      .otherwise(() => vertex(sinks[0]))
  }

  /** Returns all source vertices (vertices with no incoming edges).
   *
   * Time complexity: O(|V| + |E|) because naively implemented*/
  getSources(): Set<A> {
    const allVertices = this.getVertices()

    const nonSources = new Set<A>(this.getAllEdges().map((edge) => edge[1]))

    const sources = allVertices.filter((vertex) => !nonSources.has(vertex))
    return new Set(sources)
  }

  /** Returns all sink vertices (vertices with no outgoing edges)

  * Time complexity: O(|V| + |E|) because naively implemented*/
  getSinks(): Set<A> {
    const allVertices = this.getVertices()

    const nonSinks = new Set<A>(this.getAllEdges().map((edge) => edge[0]))

    const sinks = allVertices.filter((vertex) => !nonSinks.has(vertex))
    return new Set(sinks)
  }
}

/*********************
     Primitives
**********************/

export function isEmpty<A extends Ord<A> & HasId>(g: DirectedAcyclicGraph<A>) {
  return g instanceof Empty
}

export function empty<A extends Ord<A> & HasId>() {
  return new Empty<A>()
}

/** Empty graph */
export class Empty<A extends Ord<A> & HasId> extends Dag<A> {
  constructor() {
    super()
  }
}

export function isVertex<A extends Ord<A> & HasId>(g: DirectedAcyclicGraph<A>) {
  return g instanceof Vertex
}

export function vertex<A extends Ord<A> & HasId>(a: A) {
  return new Vertex(a)
}

/** The graph consisting of a single isolated vertex. */
export class Vertex<A extends Ord<A> & HasId> extends Dag<A> {
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
export function overlay<A extends Ord<A> & HasId>(
  x: DirectedAcyclicGraph<A>,
  y: DirectedAcyclicGraph<A>
): DirectedAcyclicGraph<A> {
  return new Overlay(x, y)
}

export class Overlay<A extends Ord<A> & HasId> extends Dag<A> {
  constructor(
    readonly left: DirectedAcyclicGraph<A>,
    readonly right: DirectedAcyclicGraph<A>
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

export function connect<A extends Ord<A> & HasId>(
  x: DirectedAcyclicGraph<A>,
  y: DirectedAcyclicGraph<A>
): DirectedAcyclicGraph<A> {
  return new Connect(x, y)
}

export class Connect<A extends Ord<A> & HasId> extends Dag<A> {
  constructor(
    readonly from: DirectedAcyclicGraph<A>,
    readonly to: DirectedAcyclicGraph<A>
  ) {
    super(
      makeDirectedConnectAdjacencyMapFromAdjMaps(
        from.getAdjMap(),
        to.getAdjMap()
      )
    )
  }
}

/**********************************************
      Useful Helper Functions
***********************************************/

export function connectNodeToSource<A extends Ord<A> & HasId>(
  dag: DirectedAcyclicGraph<A>,
  node: A
): DirectedAcyclicGraph<A> {
  const nodeV = vertex(node)
  /* (G, ->, empty) is a monoid */
  return dag.overlay(nodeV.connect(dag.getSource()))
}

export function connectSinkToNode<A extends Ord<A> & HasId>(
  dag: DirectedAcyclicGraph<A>,
  node: A
): DirectedAcyclicGraph<A> {
  const nodeV = vertex(node)
  return dag.overlay(dag.getSink().connect(nodeV))
}

/**********************************************
      Graphology
  (used for the more typical dag operations)
***********************************************/

type IdType = ReturnType<HasId['getId']>

export function gyGraphFromAdjacencyMap<A extends Ord<A> & HasId>(
  adjacencyMap: Map<A, Set<A>>
): { idToVertex: Map<IdType, A>; graph: GY.DirectedGraph } {
  const idToVertex = new Map<IdType, A>()
  adjacencyMap.keys().forEach((vertex) => {
    idToVertex.set(vertex.getId(), vertex)
  })

  const graph = new GY.DirectedGraph()
  adjacencyMap.keys().forEach((vertex) => {
    graph.addNode(vertex.getId())
  })
  for (const [vertex, neighbors] of adjacencyMap) {
    for (const neighbor of neighbors) {
      graph.addEdge(vertex.getId(), neighbor.getId())
    }
  }
  return { idToVertex, graph }
}
