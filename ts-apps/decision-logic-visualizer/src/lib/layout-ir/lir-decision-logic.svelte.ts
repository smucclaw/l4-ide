/* eslint-disable @typescript-eslint/no-unused-vars */
// TODO: Need to refactor BoolValue to add a method for getting styles!
import type { BoolVar, BoolValue, Name } from '@repo/viz-expr'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import type { Ord } from '$lib/utils.js'
import { ComparisonResult } from '$lib/utils.js'
import type { DirectedAcyclicGraph } from '../algebraic-graphs/dag.js'
import { DirectedEdge } from '../algebraic-graphs/edge.js'

/*************************************************
 **************** Decl Lir Node ******************
 *************************************************/

export type DeclLirNode = FunDeclLirNode

export class FunDeclLirNode extends DefaultLirNode implements LirNode {
  readonly #name: Name
  readonly #params: readonly Name[]
  readonly #body: LadderGraphLirNode

  constructor(
    nodeInfo: LirNodeInfo,
    name: Name,
    params: readonly Name[],
    body: LadderGraphLirNode
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
    return [this.getBody(context)]
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
//

// getEdge(u: LirId, v: LirId): LirEdge | undefined {
//   const uNode = this.#dag.getAdjMap().get(u)
//   const vNode = this.#dag.getAdjMap().get(v)
//   if (!uNode || !vNode) return undefined

//   // TODO
//   // Make a LirEdge that also has any data associated with the edge

// }

/** The `id` should correspond to that of a LadderLirNode. */
export function getNeighbors(
  context: LirContext,
  dag: DirectedAcyclicGraph<LirId>,
  id: LirId
): LadderLirNode[] {
  const neighbors = dag.getAdjMap().get(id) || new Set()

  return Array.from(neighbors)
    .map((neighborId) => context.get(neighborId) as LadderLirNode)
    .filter((n) => !!n)
}

/******************************************************
 **************** Flow Lir Nodes **********************
 ******************************************************/

// There's a 1-1 correspondence between the Flow Lir Nodes and the SF Nodes that are fed to SvelteFlow
// (and similarly with Flow Lir Edges

type Position = { x: number; y: number }

const DEFAULT_INITIAL_POSITION = { x: 0, y: 0 }

export interface FlowLirNode extends LirNode, Ord<FlowLirNode> {
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

  abstract toPretty(context: LirContext): string
}

export type LadderLirNode =
  | VarLirNode
  | NotStartLirNode
  | NotEndLirNode
  | BundlingFlowLirNode

export class LadderGraphLirNode extends DefaultLirNode implements LirNode {
  #dag: DirectedAcyclicGraph<LirId>

  constructor(nodeInfo: LirNodeInfo, dag: DirectedAcyclicGraph<LirId>) {
    super(nodeInfo)
    this.#dag = dag
  }

  setDag(dag: DirectedAcyclicGraph<LirId>) {
    this.#dag = dag
  }

  getVertices(context: LirContext): LadderLirNode[] {
    return Array.from(this.#dag.getVertices()).map(
      (id) => context.get(id) as LadderLirNode
    )
  }

  // TODO
  getEdges(_context: LirContext): LadderLirEdge[] {
    return new Error('Not implemented yet') as any
    // return Array.from(this.#dag.getEdges())
  }

  getChildren(context: LirContext) {
    return this.getVertices(context)
  }

  toString(): string {
    return 'LADDER_GRAPH_LIR_NODE'
  }
}

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
  getU(): LirId
  getV(): LirId
}

export class DefaultLadderLirEdge implements LadderLirEdge {
  id: string
  #edge: DirectedEdge<LirId>
  constructor(source: LirId, target: LirId) {
    this.id = `(${source}, ${target})`
    this.#edge = new DirectedEdge(source, target)
  }

  getId(): string {
    return this.id
  }

  getU() {
    return this.#edge.getU()
  }

  getV() {
    return this.#edge.getV()
  }

  isEqualTo<T extends LadderLirEdge>(other: T) {
    return this.getId() === other.getId()
  }

  /** Lexicographical comparison of IDs of nodes */
  compare(that: LadderLirEdge): ComparisonResult {
    if (this.getU().compare(that.getU()) !== ComparisonResult.Equal) {
      return this.getU().compare(that.getU())
    }

    return this.getV().compare(that.getV())
  }
}
