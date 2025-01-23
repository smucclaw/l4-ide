import type { Eq, Ord } from '$lib/utils.ts'
import { UndirectedEdge } from './alga.ts'
import _ from 'lodash'
import {
  graphUnion,
  appendVerticesToSourceNeighbors,
  makeDirectedConnectAdjacencyMap,
} from './adjacency-map-directed-graph.ts'
import { DirectedAMGraph } from './adjacency-map-directed-graph.ts'

/**********************************************************
  Internal Adjacency Map implementation / representation
            for Undirected graphs
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
export type UndirectedGraph<A extends Ord<A>> =
  | Empty<A>
  | Vertex<A>
  | Overlay<A>
  | Connect<A>

export class UndirectedAMGraph<A extends Ord<A>>
  extends DirectedAMGraph<A>
  implements Eq<UndirectedAMGraph<A>>
{
  override connect(other: UndirectedAMGraph<A>): UndirectedAMGraph<A> {
    return new Connect(this, other)
  }

  override isEqualTo(other: this): boolean {
    // TODO: Improve this!
    return this.toString() === other.toString()
    // const isEqToPredicate = <T extends Eq<T>>(x: T, y: T): boolean => {
    //   return x.isEqualTo(y)
    // }
    // return (
    //   other instanceof UndirectedAMGraph &&
    //   _.isEqualWith(this.getVertices(), other.getVertices(), isEqToPredicate) &&
    //   _.isEqualWith(
    //     this.getAllEdges(),
    //     other.getAllEdges(),
    //     (first: [A, A], second: [A, A]) => {
    //       return first[0].isEqualTo(second[0]) && first[1].isEqualTo(second[1])
    //     }
    //   )
    // )
  }

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

/** Convenience wrapper. Same kind of implementation as DG.empty */
export function empty<A extends Ord<A>>() {
  return new Empty<A>()
}

/** Empty graph. Same implementation as DG.Empty */
export class Empty<A extends Ord<A>> extends UndirectedAMGraph<A> {
  constructor() {
    super()
  }
}

export function vertex<A extends Ord<A>>(a: A) {
  return new Vertex(a)
}

/** A single isolated vertex. Same kind of implementation as DG.empty */
export class Vertex<A extends Ord<A>> extends UndirectedAMGraph<A> {
  constructor(readonly value: A) {
    super(new Map([[value, new Set()]]))
  }

  getValue() {
    return this.value
  }
}

/** Convenience wrapper over Overlay ctor.
 * Same implementation as DirectedAMGraph's overlay
 */
export function overlay<A extends Ord<A>>(
  x: UndirectedAMGraph<A>,
  y: UndirectedAMGraph<A>
): UndirectedAMGraph<A> {
  return new Overlay(x, y)
}

export class Overlay<A extends Ord<A>> extends UndirectedAMGraph<A> {
  constructor(
    readonly left: UndirectedAMGraph<A>,
    readonly right: UndirectedAMGraph<A>
  ) {
    // same implementation as DirectedAMGraph's overlay
    super(graphUnion(left.getAdjMap(), right.getAdjMap()))
  }
}

/** Convenience wrapper over Connect ctor.
 *
 * connect is analogous to *
 */
export function connect<A extends Ord<A>>(
  x: UndirectedAMGraph<A>,
  y: UndirectedAMGraph<A>
): UndirectedAMGraph<A> {
  return new Connect(x, y)
}

export class Connect<A extends Ord<A>> extends UndirectedAMGraph<A> {
  constructor(
    readonly from: UndirectedAMGraph<A>,
    readonly to: UndirectedAMGraph<A>
  ) {
    // Create the adjacency map as you would for a directed connection
    const adjMap = makeDirectedConnectAdjacencyMap(from, to)

    // Add reciprocal connections necessary for the undirected graph
    // This is the main difference between directed and undirected graphs
    const fromVertices = Array.from(from.getAdjMap().keys())
    const toVertices = Array.from(to.getAdjMap().keys())
    appendVerticesToSourceNeighbors(adjMap, toVertices, fromVertices)

    // Invoke the superclass constructor with the adjacency map
    super(adjMap)
  }
}
