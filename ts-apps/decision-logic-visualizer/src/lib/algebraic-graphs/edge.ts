import type { Ord, StyleStr } from '$lib/utils.js'
import { ComparisonResult, isLessThanOrEquals } from '$lib/utils.js'

/********************************
      Edge types
*********************************/

export function stringifyEdge<A extends Ord<A>, T extends Edge<A>>(
  edge: T
): string {
  return `<${edge.getU()}, ${edge.getV()}>`
}

/** The most minimal 'Edge' */
export interface Edge<A extends Ord<A>> {
  getU(): A
  getV(): A
}

export interface EdgeWithOrd<A extends Ord<A>> extends Edge<A> {
  isEqualTo<B extends this>(that: B): boolean
  compare(that: this): ComparisonResult
}

export abstract class AbsEdgeWithOrd<A extends Ord<A>>
  implements EdgeWithOrd<A>
{
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

  abstract isEqualTo<B extends this>(that: B): boolean

  compare(that: this) {
    // lexicographical comparison
    const uComparison = this.getU().compare(that.getU())
    if (uComparison !== ComparisonResult.Equal) return uComparison

    return this.getV().compare(that.getV())
  }
}

export class UndirectedEdge<A extends Ord<A>> extends AbsEdgeWithOrd<A> {
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
export class DirectedEdge<A extends Ord<A>> extends AbsEdgeWithOrd<A> {
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
      Attributes
*********************************/
// This isn't in principle limited to edges; it's just that we will prob use LirNode methods for node data

export interface EdgeAttributes<A extends Ord<A>> {
  getStyles(): EdgeStyles
  setStyles(styles: EdgeStyles): void

  getLabel(): string
  setLabel(label: string): void

  merge(other: EdgeAttributes<A>): EdgeAttributes<A>
}

export class DefaultEdgeAttributes<A extends Ord<A>>
  implements EdgeAttributes<A>
{
  constructor(
    protected styles: EdgeStyles = new EmptyEdgeStyles(),
    protected label: string = ''
  ) {}

  getStyles(): EdgeStyles {
    return this.styles
  }

  setStyles(styles: EdgeStyles) {
    this.styles = styles
  }

  getLabel(): string {
    return this.label
  }

  setLabel(label: string) {
    this.label = label
  }

  merge(other: EdgeAttributes<A>): EdgeAttributes<A> {
    return mergeEdgeAttributes(this, other)
  }
}

/** <|> */
export function mergeEdgeAttributes<A extends Ord<A>>(
  a1: EdgeAttributes<A>,
  a2: EdgeAttributes<A>
): EdgeAttributes<A> {
  function mergeEdgeLabels(l1: string, l2: string) {
    if (l1 === emptyEdgeLabel) {
      return l2
    }
    if (l2 === emptyEdgeLabel) {
      return l1
    }
    return l2
  }

  function mergeEdgeStyles(s1: EdgeStyles, s2: EdgeStyles) {
    if (s1.getRawStyles() === '') {
      return s2
    }
    if (s2.getRawStyles() === '') {
      return s1
    }
    return s2
  }

  return new DefaultEdgeAttributes(
    mergeEdgeStyles(a1.getStyles(), a2.getStyles()),
    mergeEdgeLabels(a1.getLabel(), a2.getLabel())
  )
}

/***************************
    Edge Label and Styles
****************************/

export const emptyEdgeLabel = ''

export interface EdgeStyles {
  getRawStyles(): StyleStr
}

/** 'mempty' for EdgeStyles */
export class EmptyEdgeStyles implements EdgeStyles {
  constructor() {}

  getRawStyles() {
    return ''
  }
}

export class SelectedEdgeStyles implements EdgeStyles {
  constructor() {}

  getRawStyles() {
    return 'stroke: var(--color-highlighted-path)'
  }
}
