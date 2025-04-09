import type { Eq } from '$lib/utils.js'

/********************************
      Attributes
*********************************/
// This isn't in principle limited to edges;
// it's just that we will prob use LirNode methods for node data

export interface EdgeAttributes extends Eq<EdgeAttributes> {
  getStyles(): EdgeStylesContainer
  setStyles(styles: EdgeStylesContainer): void

  getLabel(): string
  setLabel(label: string): void

  isEqualTo<T extends EdgeAttributes>(other: T): boolean

  merge(other: EdgeAttributes): EdgeAttributes

  clone(): EdgeAttributes
}

export class DefaultEdgeAttributes implements EdgeAttributes {
  constructor(
    protected styles: EdgeStylesContainer = new EdgeStylesContainer(),
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

  getStyles(): EdgeStylesContainer {
    return this.styles
  }

  setStyles(styles: EdgeStylesContainer) {
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

  return new DefaultEdgeAttributes(
    a1.getStyles().mergeWith(a2.getStyles()),
    mergeEdgeLabels(a1.getLabel(), a2.getLabel())
  )
}

/***************************
    Edge Label and Styles
****************************/

export const emptyEdgeLabel = ''

/***************************
    Edge Style Container
****************************/

/** Container for the edge style strings */
export class EdgeStylesContainer implements Eq<EdgeStylesContainer> {
  constructor(
    private readonly base: BaseEdgeStyle = NonHighlightedEdgeStyle,
    private readonly modifiers: Set<typeof FadedEdgeStyle> = new Set()
  ) {}

  isHighlighted() {
    return this.base === HighlightedEdgeStyle
  }

  isFaded() {
    return this.modifiers.has(FadedEdgeStyle)
  }

  /********************************************************
                  Getters
  *********************************************************/

  getBase() {
    return this.base
  }

  getModifiers() {
    return this.modifiers
  }

  getStyleStrings(): Array<EdgeStyle> {
    return [this.base, ...this.modifiers]
  }

  getCombinedStyleString() {
    return [this.base, ...this.modifiers].join(' ')
  }

  /********************************************************
              Edge Style Counterparts
  *********************************************************/

  /** Get a version of the edge styles that's the same, except highlighted */
  getHighlightedCounterpart() {
    return new EdgeStylesContainer(HighlightedEdgeStyle, this.modifiers)
  }

  /** Get a version of the edge styles that's the same, except that it's *not* highlighted */
  getNonHighlightedCounterpart() {
    return new EdgeStylesContainer(NonHighlightedEdgeStyle, this.modifiers)
  }

  /** Get a version of the edge styles that's the same, except faded */
  getFadedCounterpart() {
    return new EdgeStylesContainer(
      this.base,
      new Set([...this.modifiers, FadedEdgeStyle])
    )
  }

  getNonFadedCounterpart() {
    return new EdgeStylesContainer(
      this.base,
      new Set([...this.modifiers].filter((elt) => elt !== FadedEdgeStyle))
    )
  }

  mergeWith(other: EdgeStylesContainer) {
    const newBase =
      this.base === NonHighlightedEdgeStyle ? other.base : this.base
    const newModifiers = new Set(this.modifiers).union(other.getModifiers())
    return new EdgeStylesContainer(newBase, newModifiers)
  }

  isEqualTo(other: EdgeStylesContainer): boolean {
    const sameBase = this.base === other.getBase()
    const sameModifiers =
      this.getModifiers().isSubsetOf(other.getModifiers()) &&
      other.getModifiers().isSubsetOf(this.getModifiers())
    return sameBase && sameModifiers
  }
}

/***************************
      Edge Style String
****************************/

export type EdgeStyle = BaseEdgeStyle | ModifierEdgeStyle
export type BaseEdgeStyle =
  | typeof HighlightedEdgeStyle
  | typeof NonHighlightedEdgeStyle
export type ModifierEdgeStyle = typeof FadedEdgeStyle

export const HighlightedEdgeStyle =
  'stroke: var(--color-highlighted-path-in-flow); stroke-width: var(--highlighted-stroke-width);' as const
/** This is in effect an 'mempty' for base edge style strings */
export const NonHighlightedEdgeStyle =
  'stroke: var(--ladder-stroke-color-default); stroke-width: var(--ladder-stroke-width-default);' as const

export const FadedEdgeStyle =
  'opacity: var(--opacity-ladder-incompatible);' as const
