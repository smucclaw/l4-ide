export type UBoolValCSSClass =
  | typeof TrueValCSSClass
  | typeof FalseValCSSClass
  | typeof UnknownValCSSClass
export const TrueValCSSClass = 'true-val' as const
export const FalseValCSSClass = 'false-val' as const
export const UnknownValCSSClass = 'bg-white' as const

export type NodeStyleModifierCSSClass = typeof FadedNodeCSSClass
export const FadedNodeCSSClass = 'nonviable-ladder-element' as const

export type LadderNodeCSSClass = UBoolValCSSClass | NodeStyleModifierCSSClass
