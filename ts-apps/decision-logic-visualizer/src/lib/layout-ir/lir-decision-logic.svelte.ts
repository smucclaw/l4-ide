/* eslint-disable @typescript-eslint/no-unused-vars */
// TODO: Need to refactor BoolValue to add a method for getting styles!
import type { BoolVar, BoolValue, Name } from '@repo/viz-expr'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import type { Ord } from '$lib/utils.js'
import { ComparisonResult } from '$lib/utils.js'
import type { DirectedAcyclicGraph } from '../algebraic-graphs/dag.js'

/*************************************************
 **************** Decl Lir Node ******************
 *************************************************/

export type DeclLirNode = FunDeclLirNode

export class FunDeclLirNode extends DefaultLirNode implements LirNode {
  readonly #name: Name
  readonly #params: readonly Name[]
  readonly #body: DirectedAcyclicGraph<LirId>

  constructor(
    nodeInfo: LirNodeInfo,
    name: Name,
    params: readonly Name[],
    body: DirectedAcyclicGraph<LirId>
  ) {
    super(nodeInfo)

    this.#name = name
    this.#params = params
    this.#body = body
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
    return this.#body
  }

  getChildren(context: LirContext) {
    return getDagVertices(context, this.#body)
  }

  toString(): string {
    return 'FUN_DECL_LIR_NODE'
  }
}

/******************************************************
 ********* Useful dag-related functions ***************
 ******************************************************/

export function getDagVertices(
  context: LirContext,
  dag: DirectedAcyclicGraph<LirId>
): LadderLirNode[] {
  return Array.from(dag.getVertices()).map(
    (id) => context.get(id) as LadderLirNode
  )
}

// TODO: Add getDagEdges...

/******************************************************
 **************** Flow Lir Nodes **********************
 ******************************************************/

// There's a 1-1 correspondence between the Flow Lir Nodes and the SF Nodes that are fed to SvelteFlow
// (and similarly with Flow Lir Edges

type Position = { x: number; y: number }

const DEFAULT_INITIAL_POSITION = { x: 0, y: 0 }

export interface FlowLirNode extends LirNode, Ord<FlowLirNode> {
  getNeighbors(context: LirContext): FlowLirNode[]
  getPosition(context: LirContext): Position
  toPretty(context: LirContext): string
}

abstract class BaseFlowLirNode extends DefaultLirNode implements FlowLirNode {
  #position: Position

  constructor(
    nodeInfo: LirNodeInfo,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo)
    this.#position = position
  }

  getPosition(_context: LirContext): Position {
    return this.#position
  }

  isEqualTo<T extends LirNode>(other: T) {
    return this.getId().isEqualTo(other.getId())
  }

  getNeighbors(context: LirContext) {
    return context.getNeighbors(this.getId())
  }

  abstract toPretty(context: LirContext): string
}

export type LadderLirNode =
  | VarLirNode
  | NotStartLirNode
  | NotEndLirNode
  | BundlingFlowLirNode

export type VarLirNode = BoolVarLirNode

export class BoolVarLirNode extends BaseFlowLirNode implements FlowLirNode {
  readonly #originalExpr: BoolVar
  #value = $state<BoolValue>()!

  /** For the SF node we'll make from the BoolVarLirNode */
  #data: { name: Name; value: BoolValue }

  constructor(
    nodeInfo: LirNodeInfo,
    originalExpr: BoolVar,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
    this.#originalExpr = originalExpr
    this.#value = originalExpr.value // TO CHANGE
    this.#data = { name: originalExpr.name, value: this.#value }
  }

  getName(_context: LirContext) {
    return this.#originalExpr.name
  }

  getLabel(_context: LirContext) {
    return this.#data.name.label
  }

  getData(_context: LirContext) {
    return this.#data
  }

  // TODO: if we use an 'interpreter pattern' where we have an aux env structure that keeps track of the Name => Set<Node> mappings,
  // do we still need value getter/setters?
  // Maybe for the displayer / sf custom node?
  getValue(_context: LirContext): BoolValue {
    return this.#value
  }

  setValue(_context: LirContext, value: BoolValue) {
    this.#value = value
    // TODO: Will want to publish that value has been set!
  }

  toString(): string {
    return 'BOOL_VAR_LIR_NODE'
  }

  toPretty(_context: LirContext) {
    return this.getLabel(_context)
  }
}

export class NotStartLirNode extends BaseFlowLirNode implements FlowLirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
  }

  toPretty() {
    return 'NOT'
  }

  toString(): string {
    return 'NOT_START_LIR_NODE'
  }
}

export class NotEndLirNode extends BaseFlowLirNode implements FlowLirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
  }

  toPretty() {
    return ''
  }

  toString(): string {
    return 'NOT_END_LIR_NODE'
  }
}

/******************************
    Bundling Flow Lir Node
*******************************/

export function isBundlingFlowLirNode(
  node: FlowLirNode
): node is BundlingFlowLirNode {
  return node instanceof SourceLirNode || node instanceof SinkLirNode
}

/** A Flow Lir Node that's used solely to visually group or 'bundle' other nodes.
 *
 * Using `bundling` because `group` has a specific meaning in the React/SvelteFlow context.
 */
export type BundlingFlowLirNode = SourceLirNode | SinkLirNode

export class SourceLirNode extends BaseFlowLirNode implements FlowLirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
  }

  toPretty() {
    return ''
  }

  toString(): string {
    return 'SOURCE_LIR_NODE'
  }
}

export class SinkLirNode extends BaseFlowLirNode implements FlowLirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
  }

  toPretty() {
    return ''
  }

  toString(): string {
    return 'SINK_LIR_NODE'
  }
}

/************************************************
          Ladder Lir Edge
*************************************************/

export interface LadderLirEdge extends Ord<LadderLirEdge> {
  getId(): string
  getU(): string
  getV(): string
}

export class DefaultLadderLirEdge implements LadderLirEdge {
  id: string
  constructor(
    readonly source: string,
    readonly target: string
  ) {
    this.id = `(${source}, ${target})`
  }

  getId(): string {
    return this.id
  }

  getU() {
    return this.source
  }

  getV() {
    return this.target
  }

  isEqualTo<T extends LadderLirEdge>(other: T) {
    return this.getId() === other.getId()
  }

  /** Lexicographical comparison of IDs of nodes */
  compare(that: LadderLirEdge): ComparisonResult {
    if (this.getU() < that.getU()) {
      return ComparisonResult.LessThan
    } else if (this.getU() > that.getU()) {
      return ComparisonResult.GreaterThan
    }

    if (this.getV() < that.getV()) {
      return ComparisonResult.LessThan
    } else if (this.getV() > that.getV()) {
      return ComparisonResult.GreaterThan
    }

    return ComparisonResult.Equal
  }
}
