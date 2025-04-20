import { IRExpr } from '@repo/viz-expr'
import * as VE from '@repo/viz-expr'

import {
  TrueValCSSClass,
  FalseValCSSClass,
} from '$lib/layout-ir/ladder-graph/node-styles.js'
import type { BoolValCSSClass } from '$lib/layout-ir/ladder-graph/node-styles.js'

import { match } from 'ts-pattern'

/********************************
           UBoolVal
*********************************/

export type UBoolVal = TrueVal | FalseVal | UnknownVal
// export function isUBoolValue(val: Value): val is UBoolVal {
//   return (
//     val.$type === 'TrueVal' ||
//     val.$type === 'FalseVal' ||
//     val.$type === 'UnknownVal'
//   )
// }

interface UBoolV {
  $type: 'TrueVal' | 'FalseVal' | 'UnknownVal'
  getClasses(): BoolValCSSClass[]
  toPretty(): string
}

export function isTrueVal(val: UBoolVal): val is TrueVal {
  return val.$type === 'TrueVal'
}

export function isFalseVal(val: UBoolVal): val is FalseVal {
  return val.$type === 'FalseVal'
}

export class TrueVal implements UBoolV {
  $type: 'TrueVal' = 'TrueVal' as const
  constructor() {}

  getClasses() {
    return [TrueValCSSClass]
  }

  toPretty() {
    return 'TRUE'
  }
}

export class FalseVal implements UBoolV {
  $type: 'FalseVal' = 'FalseVal' as const
  constructor() {}

  getClasses() {
    return [FalseValCSSClass]
  }

  toPretty() {
    return 'FALSE'
  }
}

export function isUnknownVal(val: UBoolVal): val is UnknownVal {
  return val.$type === 'UnknownVal'
}

export class UnknownVal implements UBoolV {
  $type: 'UnknownVal' = 'UnknownVal' as const
  constructor() {}

  getClasses() {
    return []
  }

  toPretty() {
    return 'UNKNOWN'
  }
}

export function cycle(val: UBoolVal): UBoolVal {
  return match(val)
    .with({ $type: 'TrueVal' }, () => new FalseVal())
    .with({ $type: 'FalseVal' }, () => new UnknownVal())
    .with({ $type: 'UnknownVal' }, () => new TrueVal())
    .exhaustive()
}

/**********************************************************
            Expr
**********************************************************/

/** This is basically IRExpr, but where the UBoolVar doesn't have the `value` field,
 * since we want to use values from the user,
 * as opposed to the initial values.
 */
export type Expr = Exclude<IRExpr, { $type: 'UBoolVar' }> | EvUBoolVar

/** The IRExpr UBoolVar, except without the `value` field
 * (since we want to use values from the user, as opposed to the initial values) */
export type EvUBoolVar = Omit<VE.UBoolVar, 'value'>
export type Not = VE.Not
export type Or = VE.Or
export type And = VE.And

export function veExprToEvExpr(expr: IRExpr): Expr {
  return match(expr)
    .with({ $type: 'UBoolVar' }, (expr) => expr as EvUBoolVar)
    .otherwise(() => expr)
}
