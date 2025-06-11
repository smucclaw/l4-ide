import type { Eq, Ord, HasId } from '@repo/layout-ir'
import {
  DirectedAMGraph,
  makeDirectedConnectAdjacencyMap,
  mergeEdgeAttributeMaps,
  mergeDirectedGraphs,
  type EdgeAttributeMap,
} from './adjacency-map-directed-graph.js'
import * as GY from 'graphology'
import { topologicalSort } from 'graphology-dag'
import { match, P } from 'ts-pattern'
import _ from 'lodash'

/* TODOs
- There is currently a fair bit of code duplication
between the various kinds of alga graphs in this mini-lib
(eg between this and adjacency-map-directed-graph.ts).
Would be good to improve that.
- May (not sure) be better to use `union` and `join`, like in https://github.com/snowleopard/alga/blob/main/src/Algebra/Graph/Acyclic/AdjacencyMap.hs
*/

export type DirectedAcyclicGraph<A extends Ord<A>> =
  | Empty<A>
  | Vertex<A>
  | Overlay<A>
  | Connect<A>

/** Directed Acyclic Graph
*
* TODO: This currently isn't the safest,
in that we only check if it's a DAG when topSort / getSource / getSink are called
*/
export abstract class Dag<A extends Ord<A>>
  extends DirectedAMGraph<A>
  implements Eq<Dag<A>>
{
  /** A cached topological ordering */
  private topologicalOrdering?: A[]

  constructor(
    adjacencyMap?: Map<A, Set<A>>,
    edgeAttributes?: EdgeAttributeMap<A>
  ) {
    super(adjacencyMap, edgeAttributes)
  }

  override overlay(other: DirectedAcyclicGraph<A>): DirectedAcyclicGraph<A> {
    return new Overlay(this, other)
  }

  override connect(other: DirectedAcyclicGraph<A>): DirectedAcyclicGraph<A> {
    return new Connect(this, other)
  }

  // TODO: Test / check: would this actually remove vertices?
  /**
  * instance Functor Graph where
      fmap f g = g >>= (vertex . f)
  */
  gmap<B extends Ord<B>>(f: (a: A) => B): DirectedAcyclicGraph<B> {
    return this.bind((a) => vertex(f(a)))
  }

  /**
   * FlatMap (bind) operation for the Dag monad.
   * Applies a function to each vertex in the graph and flattens the result.
   * Note: This is specialized to DAGs.
   */
  bind<B extends Ord<B>>(
    f: (a: A) => DirectedAcyclicGraph<B>
  ): DirectedAcyclicGraph<B> {
    const g = buildg<B>((e, v, o, c) =>
      foldg<A, DirectedAcyclicGraph<B>>(
        e,
        (a: A) => foldg(e, v, o, c, f(a)),
        o,
        c,
        this
      )
    )
    g.removeSelfLoops()
    return g
  }

  /**
   * Constructs the induced subgraph by removing vertices that do not satisfy `predicate`.
   */
  induce(predicate: (a: A) => boolean): DirectedAcyclicGraph<A> {
    return this.bind((a) => (predicate(a) ? vertex(a) : empty<A>()))
  }

  partition(predicate: (a: A) => boolean): {
    induced: DirectedAcyclicGraph<A>
    complement: DirectedAcyclicGraph<A>
  } {
    const complementPred = _.negate(predicate)
    return {
      induced: this.induce(predicate),
      complement: this.induce(complementPred),
    }
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

  /** Returns the unique source, if there is only one source.
   * No guarantees if there is more than one source. */
  getSource(): Vertex<A> | Empty<A> {
    /* This can be made more performant in the future,
    especially if we specialize it to DAGs with exactly one source and exactly one sink.
     */
    const topSort = this.getTopSort()
    return match(topSort)
      .with([], () => empty<A>())
      .otherwise(() => vertex(topSort[0]))
  }

  /** Returns the unique source, if there is only one source.
   * No guarantees if there is more than one sink. */
  getSink(): Vertex<A> | Empty<A> {
    const topSort = this.getTopSort()
    return match(topSort)
      .with([], () => empty<A>())
      .otherwise(() => vertex(topSort[topSort.length - 1]))
  }

  /** Internal helper */
  protected getAllPathsFromVertex(vertex: A): Array<Array<A>> {
    const neighbors = this.getAdjMap().get(vertex) ?? new Set()
    if (neighbors.size === 0) return [[vertex]]

    const pathsFromNeighbors = Array.from(neighbors).flatMap((neighbor) =>
      this.getAllPathsFromVertex(neighbor)
    )
    return pathsFromNeighbors.map((path) => [vertex, ...path])
  }

  getAllPaths() {
    const source = this.getSource()
    const barePaths: Array<Array<A>> = match(source)
      .with(P.instanceOf(Empty), () => [])
      .otherwise((source) =>
        this.getAllPathsFromVertex((source as Vertex<A>).getValue())
      )
    const pathGraphs = barePaths.map(pathFromValues)
    pathGraphs.forEach((pathGraph) => {
      const edges = pathGraph.getEdges()
      edges.forEach((edge) => {
        pathGraph.setEdgeAttributes(edge, this.getAttributesForEdge(edge))
      })
    })
    return pathGraphs
  }

  /** Internal helper */
  removeSelfLoops() {
    this.getEdges().forEach((edge) => {
      if (edge.getU().isEqualTo(edge.getV())) {
        this._removeEdge(edge.getU(), edge.getV())
        this.removeEdgeFromEdgeAttributes(edge)
      }
    })
  }
}

/*********************
     Primitives
**********************/

export function isEmpty<A extends Ord<A>>(g: DirectedAcyclicGraph<A>) {
  return g instanceof Empty
}

export function empty<A extends Ord<A>>() {
  return new Empty<A>()
}

/** Empty graph */
export class Empty<A extends Ord<A>> extends Dag<A> {
  constructor() {
    super()
  }
}

export function isVertex<A extends Ord<A>>(
  g: DirectedAcyclicGraph<A>
): g is Vertex<A> {
  return g instanceof Vertex
}

export function vertex<A extends Ord<A>>(a: A) {
  return new Vertex(a)
}

/** The graph consisting of a single isolated vertex. */
export class Vertex<A extends Ord<A>> extends Dag<A> {
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
  x: DirectedAcyclicGraph<A>,
  y: DirectedAcyclicGraph<A>
): DirectedAcyclicGraph<A> {
  return new Overlay(x, y)
}

export function isOverlay<A extends Ord<A>>(
  g: DirectedAcyclicGraph<A>
): g is Overlay<A> {
  return g instanceof Overlay
}

export class Overlay<A extends Ord<A>> extends Dag<A> {
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
    const { adjMap, edgeAttrs } = mergeDirectedGraphs(left, right)
    super(adjMap, edgeAttrs)
  }

  getLeft() {
    return this.left
  }

  getRight() {
    return this.right
  }
}

export function connect<A extends Ord<A>>(
  x: DirectedAcyclicGraph<A>,
  y: DirectedAcyclicGraph<A>
): DirectedAcyclicGraph<A> {
  return new Connect(x, y)
}

export function isConnect<A extends Ord<A>>(
  g: DirectedAcyclicGraph<A>
): g is Connect<A> {
  return g instanceof Connect
}

export class Connect<A extends Ord<A>> extends Dag<A> {
  constructor(
    readonly from: DirectedAcyclicGraph<A>,
    readonly to: DirectedAcyclicGraph<A>
  ) {
    const adjMap = makeDirectedConnectAdjacencyMap(from, to) as Map<A, Set<A>>
    const edgeAttributes = mergeEdgeAttributeMaps(
      from._getEdgeAttributesMap(),
      to._getEdgeAttributesMap()
    )
    super(adjMap, edgeAttributes)
  }

  getFrom() {
    return this.from
  }

  getTo() {
    return this.to
  }
}

/********************************************
         Graph folding
**********************************************/

// TODO: Right now foldg etc are only implemented for Dag;
// would be better if the base graph classes also had them

/**
 * Adapted from
 *  https://github.com/snowleopard/alga/blob/b50c5c3b0c80ff559d1ba75f31bd86dba1546bb2/src/Algebra/Graph.hs#L466
 * Generalised 'Graph' folding: recursively collapse a 'Graph' by applying
 * the provided functions to the leaves and internal nodes of the expression.
 * Complexity: O(s) applications of the given functions.
 *
 * Example usages:
 * - size computation: foldg(0, _ => 1, (x, y) => x + y, (x, y) => x + y, g)
 * - checking for emptiness: foldg(true, _ => false, (x, y) => x && y, (x, y) => x && y, g) == 'isEmpty'
 * - checking for a vertex: foldg(false, x => x === a, (x, y) => x || y, (x, y) => x || y, g)  == 'hasVertex' x
 *
 * Other noteworthy things:
 * - foldg 'empty' 'vertex'        'overlay' 'connect'        == id
 *
 * @param e The value to return for an Empty graph.
 * @param v Function to apply to a vertex's value.
 * @param o Combine results of Overlay nodes.
 * @param c Combine results of Connect nodes.
 * @param g The graph to fold over.
 */
export function foldg<A extends Ord<A>, B>(
  e: B,
  v: (a: A) => B,
  o: (b1: B, b2: B) => B,
  c: (b1: B, b2: B) => B,
  g: DirectedAcyclicGraph<A>
): B {
  return match(g)
    .with(P.when(isEmpty<A>), () => e)
    .with(P.when(isVertex<A>), (vtx: Vertex<A>) => v(vtx.getValue()))
    .with(P.when(isOverlay<A>), (overlayG: Overlay<A>) =>
      o(
        foldg(e, v, o, c, overlayG.getLeft()),
        foldg(e, v, o, c, overlayG.getRight())
      )
    )
    .with(P.when(isConnect<A>), (connectG: Connect<A>) =>
      c(
        foldg(e, v, o, c, connectG.getFrom()),
        foldg(e, v, o, c, connectG.getTo())
      )
    )
    .exhaustive()
}

/**
 * Adapted from https://github.com/snowleopard/alga/blob/b50c5c3b0c80ff559d1ba75f31bd86dba1546bb2/src/Algebra/Graph.hs#L522
 * Build a graph given an interpretation of the four graph construction
 * primitives 'empty', 'vertex', 'overlay' and 'connect', in that order.
 *
 *
 * Example usages:
 * - buildg((e, v, o, c) => e)                                    == empty()
 * - buildg((e, v, o, c) => v(x))                                 == vertex(x)
 * - buildg((e, v, o, c) => o(foldg(e, v, o, c, x), foldg(e, v, o, c, y))) == overlay(x, y)
 * - buildg((e, v, o, c) => c(foldg(e, v, o, c, x), foldg(e, v, o, c, y))) == connect(x, y)
 * - buildg((e, v, o, _c) => xs.map(v).reduce(o, e))              == vertices(xs)
 * - foldg(e, v, o, c, buildg(f))                                 == f(e, v, o, c)
 */
export function buildg<A extends Ord<A>>(
  f: (
    e: DirectedAcyclicGraph<A>,
    v: (a: A) => DirectedAcyclicGraph<A>,
    o: (
      x: DirectedAcyclicGraph<A>,
      y: DirectedAcyclicGraph<A>
    ) => DirectedAcyclicGraph<A>,
    c: (
      x: DirectedAcyclicGraph<A>,
      y: DirectedAcyclicGraph<A>
    ) => DirectedAcyclicGraph<A>
  ) => DirectedAcyclicGraph<A>
): DirectedAcyclicGraph<A> {
  return f(empty<A>(), vertex, overlay, connect)
}

/********************************************
    Basic graph construction primitives
**********************************************/

/** Construct the graph comprising /a single edge/.
 *
 * edge x y == 'connect' ('vertex' x) ('vertex' y)
 */
export function edge<A extends Ord<A>>(x: A, y: A): DirectedAcyclicGraph<A> {
  // Adapted from
  // https://github.com/snowleopard/alga/blob/b50c5c3b0c80ff559d1ba75f31bd86dba1546bb2/src/Algebra/Graph/AdjacencyMap.hs#L251
  if (x.isEqualTo(y)) {
    const vtx = vertex(x)
    return connect(vtx, vtx)
  } else {
    return connect(vertex(x), vertex(y))
  }
}

/**
 * Construct the graph comprising a given list of isolated vertices.
 */
export function vertices<A extends Ord<A>>(
  vertices: A[]
): DirectedAcyclicGraph<A> {
  return vertices.map((v) => vertex(v)).reduce(overlay, empty())
}

/** Overlay a given array of graphs. */
export function overlays<A extends Ord<A>>(
  graphs: DirectedAcyclicGraph<A>[]
): DirectedAcyclicGraph<A> {
  return graphs.reduce(overlay, empty())
}

/** Make path graph from an array of vertices */
export function pathFromValues<A extends Ord<A>>(
  vertices: A[]
): DirectedAcyclicGraph<A> {
  return pathFromVertices(vertices.map(vertex))
}

/**
 * Make a 'path graph' from an array of vertices.
 */
export function pathFromVertices<A extends Ord<A>>(
  vertices: Vertex<A>[]
): DirectedAcyclicGraph<A> {
  if (vertices.length === 0) {
    return empty()
  }
  if (vertices.length === 1) {
    return vertices[0]
  }
  const edges = vertices
    .slice(1)
    .map((neighborVertex, i) => connect(vertices[i], neighborVertex))
  return edges.reduce(overlay)
}

export function connectSinkToNode<A extends Ord<A>>(
  dag: DirectedAcyclicGraph<A>,
  node: A
): DirectedAcyclicGraph<A> {
  const nodeV = vertex(node)
  return dag.overlay(dag.getSink().connect(nodeV))
}

/**********************************************
         INTERNAL: Graphology
  (used for the more typical dag operations)
***********************************************/

type IdType = ReturnType<HasId['getId']>

export function gyGraphFromAdjacencyMap<A extends Ord<A>>(
  adjacencyMap: Map<A, Set<A>>
): { idToVertex: Map<IdType, A>; graph: GY.DirectedGraph } {
  const idToVertex = new Map<IdType, A>()
  for (const vertex of adjacencyMap.keys()) {
    idToVertex.set(vertex.toString(), vertex)
  }

  const graph = new GY.DirectedGraph()
  for (const vertex of adjacencyMap.keys()) {
    graph.addNode(vertex.toString())
  }
  for (const [vertex, neighbors] of adjacencyMap) {
    for (const neighbor of neighbors) {
      graph.addEdge(vertex.toString(), neighbor.toString())
    }
  }
  return { idToVertex, graph }
}
