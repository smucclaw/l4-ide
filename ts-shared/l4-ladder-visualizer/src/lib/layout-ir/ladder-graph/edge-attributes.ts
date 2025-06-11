import type { Eq } from '@repo/layout-ir'
import 'core-js/actual/set'

/********************************
      Attributes
*********************************/
// This isn't in principle limited to edges;
// it's just that we will prob use LirNode methods for node data

export interface EdgeAttributes extends Eq<EdgeAttributes> {
  // There used to be more attrs than just the label

  getLabel(): string
  setLabel(label: string): void

  isEqualTo<T extends EdgeAttributes>(other: T): boolean

  merge(other: EdgeAttributes): EdgeAttributes

  clone(): EdgeAttributes
}

export class DefaultEdgeAttributes implements EdgeAttributes {
  constructor(protected label: string = '') {}

  isEqualTo<T extends EdgeAttributes>(other: T): boolean {
    return this.getLabel() === other.getLabel()
  }

  clone(): EdgeAttributes {
    return new DefaultEdgeAttributes(this.label)
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
    mergeEdgeLabels(a1.getLabel(), a2.getLabel())
  )
}

/***************************
      Edge Label
****************************/

export const emptyEdgeLabel = ''

/***************************
      Edge Styles
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
  'opacity: var(--opacity-ladder-nonviable);' as const
