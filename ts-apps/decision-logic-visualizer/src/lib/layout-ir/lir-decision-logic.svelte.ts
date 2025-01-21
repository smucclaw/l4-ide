import type { BoolVar, BinOp } from '$lib/data/program-ir'
import type { LirId, LirNode, LirNodeInfo } from './core'
import { LirContext, DefaultLirNode } from './core'
import { match } from 'ts-pattern'

/*************************************************
 **************** Expr Lir Nodes *****************
 *************************************************/

type BoolValue = true | false | undefined

export type ExprLirNode = BinExprLirNode | NotLirNode | VarLirNode

export type VarLirNode = BoolVarLirNode

export class BoolVarLirNode extends DefaultLirNode implements LirNode {
  readonly #originalExpr: BoolVar
  #value: BoolValue = $state()!

  constructor(nodeInfo: LirNodeInfo, originalExpr: BoolVar) {
    super(nodeInfo)
    this.#originalExpr = originalExpr
    this.#value = match(originalExpr.value)
      .with('True', () => true)
      .with('False', () => false)
      .with('Unknown', () => undefined)
      .exhaustive()
  }

  getName() {
    return this.#originalExpr.name
  }

  getValue() {
    return this.#value
  }

  getChildren(_context: LirContext) {
    return []
  }

  toString(): string {
    return 'BOOL_VAR_LIR_NODE'
  }
}

export class NotLirNode extends DefaultLirNode implements LirNode {
  #negand: LirId

  constructor(nodeInfo: LirNodeInfo, negand: ExprLirNode) {
    super(nodeInfo)
    this.#negand = negand.getId()
  }

  getNegand(context: LirContext) {
    return context.get(this.#negand) as ExprLirNode
  }

  getChildren(context: LirContext) {
    return [this.getNegand(context)]
  }

  toString(): string {
    return 'NOT_LIR_NODE'
  }
}

export class BinExprLirNode extends DefaultLirNode implements LirNode {
  #op: BinOp
  #left: LirId
  #right: LirId

  constructor(
    nodeInfo: LirNodeInfo,
    op: BinOp,
    left: ExprLirNode,
    right: ExprLirNode
  ) {
    super(nodeInfo)
    this.#op = op
    this.#left = left.getId()
    this.#right = right.getId()
  }

  getOp() {
    return this.#op
  }

  getLeft(context: LirContext) {
    return context.get(this.#left) as ExprLirNode
  }

  getRight(context: LirContext) {
    return context.get(this.#right) as ExprLirNode
  }

  getChildren(context: LirContext) {
    return [this.getLeft(context), this.getRight(context)]
  }

  toString(): string {
    return 'BIN_EXPR_LIR_NODE'
  }
}
