import * as AM from './adjacency-map'

/****************************
  Alga abstract interface
*****************************/

/** Algebraic graph */
export type UndirectedGraph<A extends Ord> = AM.UndirectedGraph<A>

export class UndirectedEdge<A extends Ord> implements Eq {
  readonly u: A
  readonly v: A
  constructor(u: A, v: A) {
    if (isLessThanOrEquals(u, v)) {
      this.u = u
      this.v = v
    } else {
      this.v = u
      this.u = v
    }
  }

  isEqualTo(that: unknown): boolean {
    if (!(that instanceof UndirectedEdge)) return false
    return (
      (this.u.isEqualTo(that.u) && this.v.isEqualTo(that.v)) ||
      (this.u.isEqualTo(that.v) && this.v.isEqualTo(that.u))
    )
  }

  toString(): string {
    return `(${this.u}, ${this.v})`
  }
}

/******************************
    Abstract (ish) primitives
*******************************/

export function vertex<A extends Ord>(a: A): UndirectedGraph<A> {
  return new AM.Vertex(a)
}

export function empty<A extends Ord>(): UndirectedGraph<A> {
  return new AM.Empty()
}

/** Convenience wrapper over Connect ctor.
 *
 * connect is analogous to *
 */
export const connect = AM.connect as <A extends Ord>(
  x: UndirectedGraph<A>,
  y: UndirectedGraph<A>
) => UndirectedGraph<A>

/** Convenience wrapper over Overlay ctor.
 *
 * overlay is analogous to +
 */
export const overlay = AM.overlay as <A extends Ord>(
  x: UndirectedGraph<A>,
  y: UndirectedGraph<A>
) => UndirectedGraph<A>

/**************************************
  Other graph construction functions
***************************************/

/** Construct the graph comprising /a single edge/.
 *
 * edge x y == 'connect' ('vertex' x) ('vertex' y)
 */
export function edge<A extends Ord>(x: A, y: A): UndirectedGraph<A> {
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
export function vertices<A extends Ord>(vertices: A[]): UndirectedGraph<A> {
  return vertices
    .map((v) => vertex(v))
    .reduce(overlay, empty() as UndirectedGraph<A>)
}

// TODO: Test this
/** Make path graph from an array of vertices */
export function path<A extends Ord>(vertices: A[]): UndirectedGraph<A> {
  // TODO: Refactor this to use ts-pattern
  if (vertices.length === 0) {
    return empty()
  }
  if (vertices.length === 1) {
    return vertex(vertices[0])
  }
  const edges = vertices.slice(1).map((v, i) => edge(vertices[i], v))
  return edges.reduce((acc, curr) => overlay(acc, curr))
}

/****************************
    Pseudo Eq, Ord
*****************************/
// TODO: This is useful outside of alga too, so move it out later

/** For the underlying vertices */
export interface Eq {
  isEqualTo(other: unknown): boolean
}

export enum ComparisonResult {
  LessThan = -1,
  Equal = 0,
  GreaterThan = 1,
}

export interface Ord extends Eq {
  compare(other: this): ComparisonResult
}

export function isLessThanOrEquals<A extends Ord>(a: A, b: A): boolean {
  return [ComparisonResult.Equal, ComparisonResult.LessThan].includes(
    a.compare(b)
  )
}
