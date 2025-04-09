import { match } from 'ts-pattern'
import { IRExpr } from '@repo/viz-expr'
import * as VE from '@repo/viz-expr'

/********************************
          Value
*********************************/

export type Value = BoolVal

export type BoolVal = TrueVal | FalseVal | UnknownVal

export function isBoolVal(val: Value): val is BoolVal {
  return (
    val.$type === 'TrueVal' ||
    val.$type === 'FalseVal' ||
    val.$type === 'UnknownVal'
  )
}

interface BoolV {
  $type: 'TrueVal' | 'FalseVal' | 'UnknownVal'
  getClasses(): string[]
  toPretty(): string
}

export function isTrueVal(val: BoolVal): val is TrueVal {
  return val.$type === 'TrueVal'
}

export function isFalseVal(val: BoolVal): val is FalseVal {
  return val.$type === 'FalseVal'
}

export class TrueVal implements BoolV {
  $type: 'TrueVal' = 'TrueVal' as const
  constructor() {}

  getClasses() {
    return ['true-val']
  }

  toPretty() {
    return 'TRUE'
  }
}

export class FalseVal implements BoolV {
  $type: 'FalseVal' = 'FalseVal' as const
  constructor() {}

  getClasses() {
    return ['false-val']
  }

  toPretty() {
    return 'FALSE'
  }
}

export function isUnknownVal(val: BoolVal): val is UnknownVal {
  return val.$type === 'UnknownVal'
}

export class UnknownVal implements BoolV {
  $type: 'UnknownVal' = 'UnknownVal' as const
  constructor() {}

  getClasses() {
    return []
  }

  toPretty() {
    return 'UNKNOWN'
  }
}

export function cycle(val: BoolVal): BoolVal {
  return match(val)
    .with({ $type: 'TrueVal' }, () => new FalseVal())
    .with({ $type: 'FalseVal' }, () => new UnknownVal())
    .with({ $type: 'UnknownVal' }, () => new TrueVal())
    .exhaustive()
}

/**********************************************************
            Expr
**********************************************************/

/** IRExpr, but where the BoolVar doesn't have the `value` field,
 * since we want to use values from the user,
 * as opposed to the initial values.
 */
export type Expr = Exclude<IRExpr, { $type: 'BoolVar' }> | EVBoolVar

/** The IRExpr BoolVar, except without the `value` field
 * (since we want to use values from the user, as opposed to the initial values) */
export type EVBoolVar = Omit<VE.BoolVar, 'value'>
export type Not = VE.Not
export type Or = VE.Or
export type And = VE.And

export function veExprToEvExpr(expr: IRExpr): Expr {
  return match(expr)
    .with({ $type: 'BoolVar' }, (expr) => expr as EVBoolVar)
    .otherwise(() => expr)
}
