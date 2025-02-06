import * as AM from './adjacency-map-undirected.js'
import type { Ord } from '$lib/utils.js'
import { ComparisonResult, isLessThanOrEquals } from '$lib/utils.js'

/****************************
  Alga abstract interface
*****************************/

/** Algebraic undirected graph */
export type UndirectedGraph<A extends Ord<A>> = AM.UndirectedGraph<A>

export type Vertex<A extends Ord<A>> = AM.Vertex<A>

/********************************
      Edge types
*********************************/

export abstract class Edge<A extends Ord<A>> implements Ord<Edge<A>> {
  readonly u: A
  readonly v: A
  constructor(u: A, v: A) {
    this.u = u
    this.v = v
  }

  getU(): A {
    return this.u
  }

  getV(): A {
    return this.v
  }

  abstract isEqualTo<B extends Edge<A>>(that: B): boolean

  compare(that: this) {
    // lexicographical comparison
    const uComparison = this.getU().compare(that.getU())
    if (uComparison !== ComparisonResult.Equal) return uComparison

    return this.getV().compare(that.getV())
  }
}

export class UndirectedEdge<A extends Ord<A>> extends Edge<A> {
  readonly u: A
  readonly v: A
  constructor(u: A, v: A) {
    super(u, v)
    // Sort input vertices so that this.u <= this.v
    // The same `compare` implementation can then be used for both directed and undirected edges
    if (isLessThanOrEquals(u, v)) {
      this.u = u
      this.v = v
    } else {
      this.v = u
      this.u = v
    }
  }

  override isEqualTo<B extends UndirectedEdge<A>>(that: B): boolean {
    return (
      that instanceof UndirectedEdge &&
      ((this.u.isEqualTo(that.getU()) && this.v.isEqualTo(that.getV())) ||
        (this.u.isEqualTo(that.getV()) && this.v.isEqualTo(that.getU())))
    )
  }

  toString(): string {
    return `(${this.u}, ${this.v})`
  }
}

/** Note that the string representation of a DirectedEdge differs from that of an UndirectedEdge. */
export class DirectedEdge<A extends Ord<A>> extends Edge<A> {
  constructor(u: A, v: A) {
    super(u, v)
  }

  override isEqualTo<B extends DirectedEdge<A>>(that: B): boolean {
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

/********************************
    Abstract (ish) primitives
*********************************/

export const empty = AM.empty

export const vertex = AM.vertex

/** Convenience wrapper over Connect ctor.
 *
 * connect is analogous to *
 *
 * This is an associative operation with the identity
 * 'empty'.
 * It distributes over 'overlay' and obeys the decomposition axiom.
 */
export const connect = AM.connect

/** Convenience wrapper over Overlay ctor.
 *
 * overlay is analogous to +
 */
export const overlay = AM.overlay

/**************************************
  Other graph construction functions
***************************************/

/** Construct the graph comprising /a single edge/.
 *
 * edge x y == 'connect' ('vertex' x) ('vertex' y)
 */
export function edge<A extends Ord<A>>(x: A, y: A): UndirectedGraph<A> {
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
export function vertices<A extends Ord<A>>(vertices: A[]): UndirectedGraph<A> {
  return vertices
    .map((v) => vertex(v))
    .reduce(overlay, empty() as UndirectedGraph<A>)
}

/** Construct the graph from a list of edges. */
export function graphFromEdges<A extends Ord<A>>(
  edges: Array<[A, A]>
): UndirectedGraph<A> {
  return edges.map(([x, y]) => edge(x, y)).reduce(overlay, empty())
}

/** Make path graph from an array of vertices */
export function pathFromValues<A extends Ord<A>>(
  vertices: A[]
): UndirectedGraph<A> {
  return pathFromVertices(vertices.map(vertex))
}

/**
 * Make a 'path graph' from an array of vertices.
 */
export function pathFromVertices<A extends Ord<A>>(
  vertices: Vertex<A>[]
): UndirectedGraph<A> {
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
