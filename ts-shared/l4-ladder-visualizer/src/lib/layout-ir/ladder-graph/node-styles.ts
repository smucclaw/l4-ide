export type UBoolValCSSClass =
  | typeof TrueValCSSClass
  | typeof FalseValCSSClass
  | typeof UnknownValCSSClass
export const TrueValCSSClass = 'true-val' as const
export const FalseValCSSClass = 'false-val' as const
export const UnknownValCSSClass = 'bg-white' as const

export type NodeStyleModifierCSSClass = typeof FadedNodeCSSClass
export const FadedNodeCSSClass = 'nonviable-ladder-element' as const

export const IrrelevantNodeCSSClass = 'irrelevant-ladder-element' as const
export const ShortCircuitedNodeCSSClass =
  'short-circuited-ladder-element' as const

export const HighlightedNodeCSSClass = 'highlighted-ladder-node' as const

export type LadderNodeCSSClass =
  | UBoolValCSSClass
  | typeof HighlightedNodeCSSClass
  | NodeStyleModifierCSSClass
