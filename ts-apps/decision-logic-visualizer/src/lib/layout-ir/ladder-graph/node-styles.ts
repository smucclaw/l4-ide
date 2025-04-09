export type BoolValCSSClass = typeof TrueValCSSClass | typeof FalseValCSSClass
export const TrueValCSSClass = 'true-val' as const
export const FalseValCSSClass = 'false-val' as const

export type NodeStyleModifierCSSClass = typeof FadedNodeCSSClass
export const FadedNodeCSSClass = 'incompatible-ladder-element' as const

export type LadderNodeCSSClass = BoolValCSSClass | NodeStyleModifierCSSClass
