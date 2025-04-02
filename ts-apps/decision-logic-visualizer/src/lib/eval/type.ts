import { match } from 'ts-pattern'
import { Subst } from './environment.js'
import type { Unique } from '@repo/viz-expr'
import type { DirectedAcyclicGraph } from '$lib/algebraic-graphs/dag'
import type { LirId } from '../layout-ir/core.js'

/********************************
          Value
*********************************/

export type Value = BoolVal
// Actually not sure we need ResolvedValue after all!
// export type ResolvedValue = Omit<Value, 'UnknownVal'>

export type BoolVal = TrueVal | FalseVal | UnknownVal

interface BoolV {
  $type: 'TrueVal' | 'FalseVal' | 'UnknownVal'
  getClasses(): string[]
}

export class TrueVal implements BoolV {
  $type: 'TrueVal' = 'TrueVal' as const
  constructor() {}

  getClasses() {
    return ['true-val']
  }
}

export class FalseVal implements BoolV {
  $type: 'FalseVal' = 'FalseVal' as const
  constructor() {}

  getClasses() {
    return ['false-val']
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
}

export function cycle(val: BoolVal): BoolVal {
  return match(val)
    .with({ $type: 'TrueVal' }, () => new FalseVal())
    .with({ $type: 'FalseVal' }, () => new UnknownVal())
    .with({ $type: 'UnknownVal' }, () => new TrueVal())
    .exhaustive()
}

/** Aka ClosureV.
 *
 * TODO: Maybe don't bother with this and just use an Evaluator type that feels wrong but that is probably enough for our purposes? */
export class FunV {
  constructor(
    readonly params: Set<Unique>,
    readonly body: Expr,
    readonly env: Subst
  ) {}
}

/**********************************************************
                  FakeExpr
**********************************************************/

// See Note [WhatIf-style evaluation]

/** Using these fake expressions just because it feels
conceptually clearer (or maybe just more natural) to me
to set up the evaluation using this.*/
export type Expr = Lam | App | CompoundBoolE | BoolLit

export class App {
  $type: 'EVApp' = 'EVApp' as const
  constructor(
    readonly func: Expr,
    /** Possibly partial */
    readonly args: Map<Unique, Expr>
  ) {}
}

export class Lam {
  $type: 'EVLam' = 'EVLam' as const
  constructor(
    readonly params: Set<Unique>,
    readonly body: Expr
  ) {}
}

export class BoolLit {
  $type: 'EVBoolLit' = 'EVBoolLit' as const
  constructor(readonly value: BoolV) {}
}

export class CompoundBoolE {
  $type: 'EVCompoundBoolE' = 'EVCompoundBoolE' as const
  constructor(readonly expr: DirectedAcyclicGraph<LirId>) {}
}
