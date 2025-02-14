/* eslint-disable @typescript-eslint/no-unused-vars */
// Temporarily disable eslint unused vars
// while I think more about how to handle LirContext params
import type { BoolVar } from '@repo/viz-expr'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import { match } from 'ts-pattern'

/*************************************************
 **************** Expr Lir Nodes *****************
 *************************************************/

type BoolValue = true | false | undefined

export type ExprLirNode = VarLirNode | NotLirNode | AndLirNode | OrLirNode

export type VarLirNode = BoolVarLirNode

export class BoolVarLirNode extends DefaultLirNode implements LirNode {
  readonly #originalExpr: BoolVar
  #value = $state<BoolValue>()

  constructor(nodeInfo: LirNodeInfo, originalExpr: BoolVar) {
    super(nodeInfo)
    this.#originalExpr = originalExpr
    this.#value = match(originalExpr.value)
      .with('True', () => true)
      .with('False', () => false)
      .with('Unknown', () => undefined)
      .exhaustive()
  }

  getName(_context: LirContext) {
    return this.#originalExpr.name
  }

  getValue(_context: LirContext) {
    return this.#value
  }

  setValue(_context: LirContext, value: BoolValue) {
    this.#value = value
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

export class AndLirNode extends DefaultLirNode implements LirNode {
  #args: LirId[]

  constructor(nodeInfo: LirNodeInfo, args: ExprLirNode[]) {
    super(nodeInfo)
    this.#args = args.map((n) => n.getId())
  }

  getArgs(context: LirContext) {
    return this.#args.map((id) => context.get(id) as ExprLirNode)
  }

  getChildren(context: LirContext) {
    return this.getArgs(context)
  }

  toString(): string {
    return 'AND_LIR_NODE'
  }
}

export class OrLirNode extends DefaultLirNode implements LirNode {
  #args: LirId[]

  constructor(nodeInfo: LirNodeInfo, args: ExprLirNode[]) {
    super(nodeInfo)
    this.#args = args.map((n) => n.getId())
  }

  getArgs(context: LirContext) {
    return this.#args.map((id) => context.get(id) as ExprLirNode)
  }

  getChildren(context: LirContext) {
    return this.getArgs(context)
  }

  toString(): string {
    return 'OR_LIR_NODE'
  }
}
