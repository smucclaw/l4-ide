/* eslint-disable @typescript-eslint/no-unused-vars */

import type { BoolVar, BoolValue, Name } from '@repo/viz-expr'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import { match } from 'ts-pattern'

/*************************************************
 **************** Decl Lir Node ******************
 *************************************************/

export type DeclLirNode = FunDeclLirNode

export class FunDeclLirNode extends DefaultLirNode implements LirNode {
  readonly #name: Name
  readonly #params: readonly Name[]
  readonly #body: LirId

  constructor(
    nodeInfo: LirNodeInfo,
    name: Name,
    params: readonly Name[],
    body: ExprLirNode
  ) {
    super(nodeInfo)

    this.#name = name
    this.#params = params
    this.#body = body.getId()
  }

  getLabel(_context: LirContext) {
    return this.#name.label
  }

  getUnique(_context: LirContext) {
    return this.#name.unique
  }

  getParams(_context: LirContext) {
    return this.#params
  }

  getBody(context: LirContext) {
    return context.get(this.#body) as ExprLirNode
  }

  getChildren(context: LirContext) {
    return [this.getBody(context)]
  }

  toString(): string {
    return 'FUN_DECL_LIR_NODE'
  }
}

/*************************************************
 **************** Expr Lir Nodes *****************
 *************************************************/

export type ExprLirNode = VarLirNode | NotLirNode | AndLirNode | OrLirNode

export type VarLirNode = BoolVarLirNode

export class BoolVarLirNode extends DefaultLirNode implements LirNode {
  readonly #originalExpr: BoolVar
  #value = $state<BoolValue>()!

  constructor(nodeInfo: LirNodeInfo, originalExpr: BoolVar) {
    super(nodeInfo)
    this.#originalExpr = originalExpr
    this.#value = originalExpr.value
  }

  getName(_context: LirContext) {
    return this.#originalExpr.name
  }

  getValue(_context: LirContext): BoolValue {
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
