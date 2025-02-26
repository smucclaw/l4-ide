/* eslint-disable @typescript-eslint/no-unused-vars */
// TODO: Need to refactor BoolValue to add a method for getting styles!
import type { BoolVar, BoolValue, Name } from '@repo/viz-expr'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import type { Ord } from '$lib/utils.js'
import { ComparisonResult } from '$lib/utils.js'
import type { DirectedAcyclicGraph } from '../algebraic-graphs/dag.js'
import {
  type Edge,
  DirectedEdge,
  type EdgeStyles,
  HighlightedEdgeStyles,
  EmptyEdgeStyles,
} from '../algebraic-graphs/edge.js'

/*
Design principles:
* The stuff here should not know about the concrete displayers/renderers (e.g. SvelteFlow),
since part of the point of having a Lir intermediate representation
is to make it easy to experiment with different displayers/renderers.
(That is why, e.g., there is no toSFPojo method on the following.)
*/

/*************************************************
                  Decl Lir Node 
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

  /** This differs from the Unique in that this is a name that's meant for display. */
  getFunName(_context: LirContext) {
    return this.#name.label
  }

  getUnique(_context: LirContext) {
    return this.#name.unique
  }

  getParams(_context: LirContext) {
    return this.#params
  }

  getBody(_context: LirContext) {
    return this.#body
  }

  getChildren(context: LirContext) {
    return [this.getBody(context)]
  }

  toString(): string {
    return 'FUN_DECL_LIR_NODE'
  }
}

export class PathLirNode {
  constructor(
    protected ladderGraph: LadderGraphLirNode,
    protected rawPath: DirectedAcyclicGraph<LirId>
  ) {}

  /** Helper */
  protected setStylesOnPathEdges(context: LirContext, styles: EdgeStyles) {
    const edges = this.rawPath.getEdges()
    edges.forEach((edge) => {
      this.ladderGraph.setEdgeStyles(context, edge, styles)
    })
  }

  highlight(context: LirContext) {
    this.setStylesOnPathEdges(context, new HighlightedEdgeStyles())
    // TODO: setbindings for the vars in the path too
  }

  unhighlight(context: LirContext) {
    this.setStylesOnPathEdges(context, new EmptyEdgeStyles())
  }

  getVertices(context: LirContext) {
    return this.rawPath
      .getVertices()
      .map((id) => context.get(id))
      .filter((n) => !!n) as LadderLirNode[]
  }

  toPretty(context: LirContext) {
    return this.getVertices(context)
      .map((n) => n.toPretty(context))
      .join(' ')
  }
}

/******************************************************
                  Flow Lir Nodes
 ******************************************************/

// There's a 1-1 correspondence between the Flow Lir Nodes and the SF Nodes that are fed to SvelteFlow
// (and similarly with Flow Lir Edges)

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

/**********************************************
          Ladder Graph Lir Node
***********************************************/

export class LadderGraphLirNode extends DefaultLirNode implements LirNode {
  #dag: DirectedAcyclicGraph<LirId>

  constructor(nodeInfo: LirNodeInfo, dag: DirectedAcyclicGraph<LirId>) {
    super(nodeInfo)
    this.#dag = dag
  }

  setDag(_context: LirContext, dag: DirectedAcyclicGraph<LirId>) {
    this.#dag = dag
  }

  /*****************************
      Basic graph ops
  ******************************/

  /** The `id` should correspond to that of a LadderLirNode. */
  getNeighbors(context: LirContext, id: LirId): LadderLirNode[] {
    const neighbors = this.#dag.getAdjMap().get(id) || new Set()

    return Array.from(neighbors)
      .map((neighborId) => context.get(neighborId) as LadderLirNode)
      .filter((n) => !!n)
  }

  getVertices(context: LirContext): LadderLirNode[] {
    return Array.from(this.#dag.getVertices()).map(
      (id) => context.get(id) as LadderLirNode
    )
  }

  getEdges(_context: LirContext): LadderLirEdge[] {
    return this.#dag.getEdges().map((edge) => {
      return new DefaultLadderLirEdge(edge)
    })
  }

  /** Get all simple paths through the Dag */
  getPaths(_context: LirContext) {
    return this.#dag
      .getAllPaths()
      .map((rawPath) => new PathLirNode(this, rawPath))
  }

  /*****************************
        Edge attributes
  ******************************/

  getEdgeStyles<T extends Edge<LirId>>(
    _context: LirContext,
    edge: T
  ): EdgeStyles {
    return this.#dag.getAttributesForEdge(edge).getStyles()
  }

  setEdgeStyles<T extends Edge<LirId>>(
    context: LirContext,
    edge: T,
    styles: EdgeStyles
  ) {
    this.#dag.getAttributesForEdge(edge).setStyles(styles)
    this.getRegistry().publish(context, this.getId())
  }

  getEdgeLabel<T extends Edge<LirId>>(_context: LirContext, edge: T): string {
    return this.#dag.getAttributesForEdge(edge).getLabel()
  }

  setEdgeLabel<T extends Edge<LirId>>(
    context: LirContext,
    edge: T,
    label: string
  ) {
    this.#dag.getAttributesForEdge(edge).setLabel(label)
    this.getRegistry().publish(context, this.getId())
  }

  /*****************************
        Bindings
  ******************************/

  // setBinding(context: LirContext, binding: { name: Name, value: Value })

  /*****************************
            Misc
  ******************************/

  getChildren(context: LirContext) {
    return this.getVertices(context)
  }

  toString(): string {
    return 'LADDER_GRAPH_LIR_NODE'
  }
}

/**********************************************
          Ladder Lir Node
***********************************************/

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

  getLabel(_context: LirContext) {
    return this.#data.name.label
  }

  getUnique(_context: LirContext) {
    return this.#originalExpr.name.unique
  }

  getData(_context: LirContext) {
    return this.#data
  }

  getValue(_context: LirContext): BoolValue {
    return this.#value
  }

  _setValue(context: LirContext, value: BoolValue) {
    this.#value = value
    this.getRegistry().publish(context, this.getId())
  }

  toPretty(_context: LirContext) {
    return this.getLabel(_context)
  }

  toString(): string {
    return 'BOOL_VAR_LIR_NODE'
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
  constructor(edge: DirectedEdge<LirId>) {
    this.id = `(${edge.getU()}, ${edge.getV()})`
    this.#edge = edge
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
