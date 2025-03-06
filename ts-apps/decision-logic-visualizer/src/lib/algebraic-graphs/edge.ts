import type { Eq, Ord } from '$lib/utils.js'
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

export interface EdgeWithOrd<A extends Ord<A>>
  extends Edge<A>,
    Ord<EdgeWithOrd<A>> {
  isEqualTo<B extends EdgeWithOrd<A>>(that: B): boolean
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

  abstract isEqualTo<B extends EdgeWithOrd<A>>(that: B): boolean

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

  override isEqualTo<B extends EdgeWithOrd<A>>(that: B): boolean {
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

  override isEqualTo<B extends EdgeWithOrd<A>>(that: B): boolean {
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
// This isn't in principle limited to edges;
// it's just that we will prob use LirNode methods for node data

export interface EdgeAttributes extends Eq<EdgeAttributes> {
  getStyles(): EdgeStyles
  setStyles(styles: EdgeStyles): void

  getLabel(): string
  setLabel(label: string): void

  isEqualTo<T extends EdgeAttributes>(other: T): boolean

  merge(other: EdgeAttributes): EdgeAttributes

  clone(): EdgeAttributes
}

export class DefaultEdgeAttributes implements EdgeAttributes {
  constructor(
    protected styles: EdgeStyles = new EmptyEdgeStyles(),
    protected label: string = ''
  ) {}

  isEqualTo<T extends EdgeAttributes>(other: T): boolean {
    return (
      this.getStyles().isEqualTo(other.getStyles()) &&
      this.getLabel() === other.getLabel()
    )
  }

  clone(): EdgeAttributes {
    return new DefaultEdgeAttributes(this.styles, this.label)
  }

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

  merge(other: EdgeAttributes): EdgeAttributes {
    return mergeEdgeAttributes(this, other)
  }
}

/** <|> */
export function mergeEdgeAttributes(
  a1: EdgeAttributes,
  a2: EdgeAttributes
): EdgeAttributes {
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
    if (isEmptyEdgeStyles(s1)) {
      return s2
    }
    if (isEmptyEdgeStyles(s2)) {
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

export interface EdgeStyles extends Eq<EdgeStyles> {
  $type: 'EmptyEdgeStyles' | 'HighlightedEdgeStyles'

  getStyleString(): string

  isEqualTo(other: EdgeStyles): boolean
}

export function isEmptyEdgeStyles(
  styles: EdgeStyles
): styles is EmptyEdgeStyles {
  return styles.$type === 'EmptyEdgeStyles'
}

export function isHighlightedEdgeStyles(
  styles: EdgeStyles
): styles is HighlightedEdgeStyles {
  return styles.$type === 'HighlightedEdgeStyles'
}

/** 'mempty' for EdgeStyles */
export class EmptyEdgeStyles implements EdgeStyles {
  $type = 'EmptyEdgeStyles' as const
  constructor() {}

  isEqualTo(other: EdgeStyles): boolean {
    return isEmptyEdgeStyles(other)
  }

  getStyleString(): EdgeStyleString {
    return 'stroke: var(--default-internal-stroke-color) !important;' as const
  }
}

export class HighlightedEdgeStyles implements EdgeStyles {
  $type = 'HighlightedEdgeStyles' as const
  constructor() {}

  isEqualTo(other: EdgeStyles): boolean {
    return isHighlightedEdgeStyles(other)
  }

  getStyleString(): EdgeStyleString {
    return 'stroke: var(--color-highlighted-path-in-flow) !important; stroke-width: 3px;' as const
  }
}

export const emptyEdgeLabel = ''

export type EdgeStyleString =
  | 'stroke: var(--color-highlighted-path-in-flow) !important; stroke-width: 3px;'
  | 'stroke: var(--default-internal-stroke-color) !important;'
