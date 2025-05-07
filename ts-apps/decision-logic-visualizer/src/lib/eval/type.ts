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

export function toUBoolVal(value: VE.UBoolValue): UBoolVal {
  return match(value)
    .with('True', () => new TrueVal())
    .with('False', () => new FalseVal())
    .with('Unknown', () => new UnknownVal())
    .exhaustive()
}

export function toVEBoolValue(val: UBoolVal): VE.UBoolValue {
  return val.$type
}

interface UBoolV {
  $type: TrueVal['$type'] | FalseVal['$type'] | UnknownVal['$type']
  getClasses(): BoolValCSSClass[]
  toPretty(): string
}

export function isTrueVal(val: UBoolVal): val is TrueVal {
  return val.$type === 'True'
}

export function isFalseVal(val: UBoolVal): val is FalseVal {
  return val.$type === 'False'
}

export class TrueVal implements UBoolV {
  $type: 'True' = 'True' as const
  constructor() {}

  getClasses() {
    return [TrueValCSSClass]
  }

  toPretty() {
    return 'TRUE'
  }
}

export class FalseVal implements UBoolV {
  $type: 'False' = 'False' as const
  constructor() {}

  getClasses() {
    return [FalseValCSSClass]
  }

  toPretty() {
    return 'FALSE'
  }
}

export function isUnknownVal(val: UBoolVal): val is UnknownVal {
  return val.$type === 'Unknown'
}

export class UnknownVal implements UBoolV {
  $type: 'Unknown' = 'Unknown' as const
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
    .with({ $type: 'True' }, () => new FalseVal())
    .with({ $type: 'False' }, () => new UnknownVal())
    .with({ $type: 'Unknown' }, () => new TrueVal())
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
