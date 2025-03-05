/* eslint-disable @typescript-eslint/no-unused-vars */
import {
  type Value,
  type BoolVal,
  TrueVal,
  FalseVal,
  UnknownVal,
} from './value.js'
import type { BoolVar, Unique, Name } from '@repo/viz-expr'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import type { Ord } from '$lib/utils.js'
import { ComparisonResult } from '$lib/utils.js'
import type { DirectedAcyclicGraph } from '../algebraic-graphs/dag.js'
import {
  type Edge,
  DirectedEdge,
  type EdgeStyles,
  // HighlightedEdgeStyles,
  // EmptyEdgeStyles,
  type EdgeAttributes,
} from '../algebraic-graphs/edge.js'
import type { Dimensions } from '$lib/displayers/flow/types.svelte.js'
import { match } from 'ts-pattern'

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

/*************************************************
              Path Lir Node 
 *************************************************/

// TODO: Might also need something like a container lir node for the Array<PathLirNode>

abstract class BasePathLirNode extends DefaultLirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    protected rawPath: DirectedAcyclicGraph<LirId>
  ) {
    super(nodeInfo)
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

/** For the first version, we'll take the LinearizedPaths to be wholly controlled by interactions on the ladder graph;
 * i.e., let's not worry about the linearized path -> ladder graph interactions for the v1,
 * and instead just focus on ladder graph -> linearized paths.
 *
 * The v1 could be as simple as:
 * - whenever new bindings are submitted on the ladder graph,
 *    re-compute (via `induce`) the subgraph that is compatible with the new scoreboard / env,
 *    then generate the linearized paths of that subgraph.
 */
export type PathLirNode = CompatiblePathLirNode | IncompatiblePathLirNode

export class CompatiblePathLirNode extends BasePathLirNode implements LirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    protected rawPath: DirectedAcyclicGraph<LirId>
  ) {
    super(nodeInfo, rawPath)
  }

  toString() {
    return 'COMPATIBLE_PATH_LIR_NODE'
  }

  /** Note: This is styling for the *linearized* paths below the ladder graph,
   * as opposed to styles for paths *in* the ladder graph */
  getPathStyles() {
    console.error('TODO')
  }
}

export class IncompatiblePathLirNode
  extends BasePathLirNode
  implements LirNode
{
  constructor(
    nodeInfo: LirNodeInfo,
    protected rawPath: DirectedAcyclicGraph<LirId>
  ) {
    super(nodeInfo, rawPath)
  }

  toString() {
    return 'INCOMPATIBLE_PATH_LIR_NODE'
  }

  /** Note: This is styling for the *linearized* paths below the ladder graph,
   * as opposed to styles for paths *in* the ladder graph */
  getPathStyles() {
    console.error('TODO')
  }
}

/******************************************************
                  Flow Lir Nodes
 ******************************************************/

// There's a 1-1 correspondence between the Flow Lir Nodes and the SF Nodes that are fed to SvelteFlow
// (and similarly with Flow Lir Edges)

/*
TO THINK ABT:
- do we even want to put Position on the lir nodes?
perhaps position should be handled entirely at the SF Node level?
*/

type Position = { x: number; y: number }

const DEFAULT_INITIAL_POSITION = { x: 0, y: 0 }

export interface FlowLirNode extends LirNode, Ord<FlowLirNode> {
  getDimensions(context: LirContext): Dimensions | undefined
  setDimensions(context: LirContext, dimensions: Dimensions): void

  getPosition(context: LirContext): Position
  setPosition(context: LirContext, position: Position): void

  toPretty(context: LirContext): string
}

abstract class BaseFlowLirNode extends DefaultLirNode implements FlowLirNode {
  protected position: Position
  protected dimensions?: Dimensions

  constructor(
    nodeInfo: LirNodeInfo,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo)
    this.position = position
  }

  getDimensions(_context: LirContext) {
    return this.dimensions
  }

  setDimensions(_context: LirContext, dimensions: Dimensions): void {
    this.dimensions = dimensions
  }

  getPosition(_context: LirContext): Position {
    return this.position
  }

  setPosition(_context: LirContext, position: Position): void {
    this.position = position
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

  // TODO: prob need to refactor: want to be able to
  // generate the linearized paths of the the subgraph that is compatible with the updated scoreboard / env,.
  /** Get all simple paths through the Dag */
  getPaths(_context: LirContext) {
    return this.#dag.getAllPaths()
  }

  /*****************************
        Edge attributes
  ******************************/

  getEdgeAttributes<T extends Edge<LirId>>(
    _context: LirContext,
    edge: T
  ): EdgeAttributes {
    return this.#dag.getAttributesForEdge(edge)
  }

  // TODO: Think more abt whether we really need the rest

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

  submitNewBinding(
    context: LirContext,
    binding: { name: Unique; value: Value }
  ) {
    console.log('submitNewBinding', binding)
    // TODO
  }

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
  #value = $state<BoolVal>()!

  /** For the SF node we'll make from the BoolVarLirNode */
  #data: { name: Name; value: BoolVal }

  constructor(
    nodeInfo: LirNodeInfo,
    originalExpr: BoolVar,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
    this.#originalExpr = originalExpr
    this.#value = match(originalExpr.value)
      .with('True', () => new TrueVal())
      .with('False', () => new FalseVal())
      .with('Unknown', () => new UnknownVal())
      .exhaustive()
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

  getValue(_context: LirContext): BoolVal {
    return this.#value
  }

  /** This will only be called by LadderGraphLirNode */
  _setValue(context: LirContext, value: BoolVal) {
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
