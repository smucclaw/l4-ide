import { IRExpr } from '@repo/viz-expr'
import * as VE from '@repo/viz-expr'

import {
  TrueValCSSClass,
  FalseValCSSClass,
  UnknownValCSSClass,
} from '$lib/layout-ir/ladder-graph/node-styles.js'
import type { UBoolValCSSClass } from '$lib/layout-ir/ladder-graph/node-styles.js'

import { match } from 'ts-pattern'

/********************************
           UBoolVal
*********************************/

export type UBoolVal = TrueVal | FalseVal | UnknownVal

export function toUBoolVal(value: VE.UBoolValue): UBoolVal {
  return match(value)
    .with('TrueV', () => new TrueVal())
    .with('FalseV', () => new FalseVal())
    .with('UnknownV', () => new UnknownVal())
    .exhaustive()
}

export function toVEBoolValue(val: UBoolVal): VE.UBoolValue {
  return val.$type
}

interface UBoolV {
  $type: TrueVal['$type'] | FalseVal['$type'] | UnknownVal['$type']
  getClasses(): UBoolValCSSClass[]
  toPretty(): string
}

export function isTrueVal(val: UBoolVal): val is TrueVal {
  return val.$type === 'TrueV'
}

export function isFalseVal(val: UBoolVal): val is FalseVal {
  return val.$type === 'FalseV'
}

export class TrueVal implements UBoolV {
  $type: 'TrueV' = 'TrueV' as const
  constructor() {}

  getClasses() {
    return [TrueValCSSClass]
  }

  toPretty() {
    return 'TRUE'
  }
}

export class FalseVal implements UBoolV {
  $type: 'FalseV' = 'FalseV' as const
  constructor() {}

  getClasses() {
    return [FalseValCSSClass]
  }

  toPretty() {
    return 'FALSE'
  }
}

export function isUnknownVal(val: UBoolVal): val is UnknownVal {
  return val.$type === 'UnknownV'
}

export class UnknownVal implements UBoolV {
  $type: 'UnknownV' = 'UnknownV' as const
  constructor() {}

  getClasses() {
    return [UnknownValCSSClass]
  }

  toPretty() {
    return 'UNKNOWN'
  }
}

export function cycle(val: UBoolVal): UBoolVal {
  return match(val)
    .with({ $type: 'TrueV' }, () => new FalseVal())
    .with({ $type: 'FalseV' }, () => new UnknownVal())
    .with({ $type: 'UnknownV' }, () => new TrueVal())
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
export type TrueE = VE.TrueE
export type FalseE = VE.FalseE

export function veExprToEvExpr(expr: IRExpr): Expr {
  return match(expr)
    .with({ $type: 'UBoolVar' }, (expr) => expr as EvUBoolVar)
    .otherwise(() => expr)
}
