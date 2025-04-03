import { match } from 'ts-pattern'
import { Subst, Environment } from './environment.js'
import type { Unique } from '@repo/viz-expr'
import type { DirectedAcyclicGraph } from '$lib/algebraic-graphs/dag'
import type { LirId } from '../layout-ir/core.js'

/********************************
          Value
*********************************/

export type Value = BoolVal | FunV
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

/* There's an 'EV' prefix in the following $types to distinguish them
 from, e.g., the VizExpr exprs and values.
 I.e., the EV-prefixed exprs and values are, at least for now,
 used only to make the WhatIf-style, frontend-side evaluation
conceptually clearer.
*/

export function isFunVal(val: Value): val is FunV {
  return val.$type === 'EVFunV'
}

/** Aka ClosureV.
 *
 * TODO: Maybe don't bother with this and just use an Evaluator type that feels wrong but that is probably enough for our purposes? */
export class FunV {
  $type: 'EVFunV' = 'EVFunV' as const
  constructor(
    readonly params: Set<Unique>,
    readonly body: Expr,
    readonly env: Environment
  ) {}

  getParams() {
    return this.params
  }

  getBody() {
    return this.body
  }

  getEnv() {
    return this.env
  }
}

/**********************************************************
              EVAL Expr
**********************************************************/

// See Note [WhatIf-style evaluation]

/** Using these fake expressions just because it feels
conceptually clearer (or maybe just more natural) to me
to set up the evaluation using this.*/
export type Expr = Lam | App | CompoundBoolE | BoolLit

export function isApp(expr: Expr) {
  return expr.$type === 'EVApp'
}

export class App {
  $type: 'EVApp' = 'EVApp' as const
  constructor(
    private readonly func: Expr,
    /** Possibly partial */
    private readonly args: Map<Unique, Expr>
  ) {}

  getFunc() {
    return this.func
  }

  getArgs() {
    return this.args
  }
}

export function isLam(expr: Expr) {
  return expr.$type === 'EVLam'
}

export class Lam {
  $type: 'EVLam' = 'EVLam' as const
  constructor(
    private readonly params: Set<Unique>,
    private readonly body: Expr
  ) {}

  getBody() {
    return this.body
  }

  getParams() {
    return this.params
  }
}

export function isBoolLit(expr: Expr) {
  return expr.$type === 'EVBoolLit'
}

export class BoolLit {
  $type: 'EVBoolLit' = 'EVBoolLit' as const
  constructor(private readonly value: BoolVal) {}

  getValue() {
    return this.value
  }
}

export function isCompoundBoolE(expr: Expr) {
  return expr.$type === 'EVCompoundBoolE'
}

export class CompoundBoolE {
  $type: 'EVCompoundBoolE' = 'EVCompoundBoolE' as const
  constructor(private readonly expr: DirectedAcyclicGraph<LirId>) {}

  getExprGraph() {
    return this.expr
  }
}
