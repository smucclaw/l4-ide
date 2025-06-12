/* eslint-disable @typescript-eslint/no-unused-vars */
import type { IRExpr, UBoolVar, Unique, Name, IRId } from '@repo/viz-expr'
import {
  type UBoolVal,
  isFalseVal,
  UnknownVal,
  veExprToEvExpr,
  toUBoolVal,
  TrueVal,
  FalseVal,
} from '../../eval/type.js'
import { Assignment } from '../../eval/assignment.js'
import { Evaluator, type EvalResult } from '$lib/eval/eval.js'
import type { LirId, LirNode, LirNodeInfo } from '@repo/layout-ir'
import { LirContext, DefaultLirNode } from '@repo/layout-ir'
import type { Ord } from '@repo/type-utils'
import { ComparisonResult } from '@repo/type-utils'
import {
  empty,
  isVertex,
  overlays,
  vertex,
  type DirectedAcyclicGraph,
} from '../../algebraic-graphs/dag.js'
import { type Edge, DirectedEdge } from '../../algebraic-graphs/edge.js'
import type { EdgeAttributes } from './edge-attributes.js'
import type {
  Dimensions,
  BundlingNodeDisplayerData,
} from '$lib/displayers/flow/svelteflow-types.js'
import {
  LadderNodeSelectionTracker,
  PathsListLirNode,
  LinPathLirNode,
  type NoIntermediateBundlingNodeDag,
} from '../node-paths-selection.js'
import type { LadderEnv } from '$lib/ladder-env.js'
import { isNnf, getVerticesFromAlgaDag } from './ladder-dag-helpers.js'
import _ from 'lodash'

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

  dispose(context: LirContext) {
    this.getChildren(context).map((n) => n.dispose(context))

    context.clear(this.getId())
  }

  toString(): string {
    return 'FUN_DECL_LIR_NODE'
  }
}

/******************************************************
                  Flow Lir Nodes
 ******************************************************/

// There's a 1-1 correspondence between the Flow Lir Nodes and the SF Nodes that are fed to SvelteFlow
// (and similarly with Flow Lir Edges)

/* Why should Position be put on the Lir nodes, as opposed to being handled entirely at the SF Node level?

Main argument: for more complicated layouts,
we'll want to be able to use info that's more readily available at the level of the Lir LadderGraph.
*/

type Position = { x: number; y: number }

const DEFAULT_INITIAL_POSITION = { x: 0, y: 0 }

// TODO: Should this be renamed to LadderFlowLirNode or something like that?
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

  /***********************************************
         Position, Dimensions
  ***********************************************/

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

  /***********************************************
         isEqualTo, dispose, toPretty
  ***********************************************/

  isEqualTo<T extends LirNode>(other: T) {
    return this.getId().isEqualTo(other.getId())
  }

  dispose(context: LirContext): void {
    context.clear(this.getId())
  }

  abstract toPretty(context: LirContext): string
}

/**********************************************
          Ladder Graph Lir Node
***********************************************/

export type LadderGraphLirNode =
  | NNFLadderGraphLirNode
  | NonNNFLadderGraphLirNode

/*
* What is a 'ladder graph' in this context, conceptually speaking?
* ---------------------------------------------------
* It's basically the intermediate Lir representation of the 'ladder' visualization of a boolean expression.
* I.e., it is somewhere between the abstract data (the boolean expr) and the concrete UI (the SvelteFlow nodes and edges).
* 
* Invariants / Properties
* ------------------------
* Proposals to update (non-positional or non-dimensions) data associated with the nodes/edges
will go through the LadderGraphLirNode.
* Displayers should listen for updates to the LadderGraphLirNode,
* and re-render on changes.
*
* State ownership:
  - The Lir nodes only own, and publish changes to, state that's not about the positions
    or dimensions of the nodes.
  - For instance, we do not publish changes to the position of the nodes/edges --- that is state that
    is owned by SvelteFlow.
*/
abstract class BaseLadderGraphLirNode
  extends DefaultLirNode
  implements LirNode
{
  #ladderEnv: LadderEnv
  #dag: DirectedAcyclicGraph<LirId>

  #originalExpr: IRExpr
  #vizExprToLirDag: Map<IRId, DirectedAcyclicGraph<LirId>>

  /** Keeps track of what values the user has assigned to the various var ladder nodes */
  #bindings: Assignment
  #evalResult: EvalResult = {
    result: new UnknownVal(),
    intermediate: new Map(),
  }
  // It's easier to work with the *non*-viable subgraph
  #nonViableSubgraph: DirectedAcyclicGraph<LirId> = empty()

  protected constructor(
    nodeInfo: LirNodeInfo,
    dag: DirectedAcyclicGraph<LirId>,
    vizExprToLirGraph: Map<IRId, DirectedAcyclicGraph<LirId>>,
    originalExpr: IRExpr,
    ladderEnv: LadderEnv
  ) {
    super(nodeInfo)
    this.#dag = dag
    this.#originalExpr = originalExpr
    this.#vizExprToLirDag = vizExprToLirGraph
    this.#ladderEnv = ladderEnv
    // console.log(
    //   'vizExprToLirGraph',
    //   vizExprToLirGraph.entries().forEach(([_, dag]) => {
    //     console.log(dag.toString())
    //   })
    // )

    const children = this.getChildren(nodeInfo.context)
    const varNodes = children.filter(isUBoolVarLirNode)

    // Make the initial args / assignment
    const initialAssignmentAssocList: Array<[Unique, UBoolVal]> = varNodes.map(
      (varN) => [
        varN.getUnique(nodeInfo.context),
        varN.getInitialValue(nodeInfo.context),
      ]
    )
    this.#bindings = Assignment.fromEntries(initialAssignmentAssocList)
  }

  getVizExprToLirGraph() {
    return this.#vizExprToLirDag
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
    return getVerticesFromAlgaDag(context, this.#dag)
  }

  getEdges(_context: LirContext): LadderLirEdge[] {
    return this.#dag.getEdges().map((edge) => {
      return new DefaultLadderLirEdge(edge)
    })
  }

  getOverallSource(context: LirContext): undefined | SourceLirNode {
    const source = this.#dag.getSource()
    if (!isVertex(source)) return undefined
    return context.get(source.getValue()) as SourceLirNode
  }

  getOverallSink(context: LirContext): undefined | SinkLirNode {
    const sink = this.#dag.getSink()
    if (!isVertex(sink)) return undefined
    return context.get(sink.getValue()) as SinkLirNode
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

  /** internal helper method */
  protected setEdgeAttributes<T extends Edge<LirId>>(
    _context: LirContext,
    edge: T,
    attrs: EdgeAttributes
  ) {
    this.#dag.setEdgeAttributes(edge, attrs)
  }

  getEdgeLabel<T extends Edge<LirId>>(_context: LirContext, edge: T): string {
    return this.#dag.getAttributesForEdge(edge).getLabel()
  }

  setEdgeLabel<T extends Edge<LirId>>(
    context: LirContext,
    edge: T,
    label: string
  ) {
    const attrs = this.#dag.getAttributesForEdge(edge)
    attrs.setLabel(label)
    this.#dag.setEdgeAttributes(edge, attrs)

    this.getRegistry().publish(context, this.getId())
  }

  /**********************************
     Viable vs non-viable Subgraph
  ***********************************/

  nodeIsInNonViableSubgraph(_context: LirContext, node: LadderLirNode) {
    return this.#nonViableSubgraph.hasVertex(node.getId())
  }

  edgeIsInNonViableSubgraph(_context: LirContext, edge: LadderLirEdge) {
    return this.#nonViableSubgraph.hasEdge(edge.getU(), edge.getV())
  }

  /** Helper: Compute the subgraph that is no longer viable in light of user's choices */
  private computeNonViableSubgraph(
    _context: LirContext,
    evalResult: EvalResult
  ) {
    const nonViableIRIds = Array.from(evalResult.intermediate.entries())
      .filter(([_id, val]) => isFalseVal(val))
      .map(([irId, _val]) => irId)
    const nonviableSubgraph = nonViableIRIds
      .map((irId) => this.#vizExprToLirDag.get(irId))
      .filter((dag) => !!dag)
      .reduceRight((acc, curr) => acc.overlay(curr), empty())
    return nonviableSubgraph
  }

  /*****************************
        Eval, Bindings
  ******************************/

  async doEvalLadderExprWithVarBindings(context: LirContext) {
    const result = await Evaluator.eval(
      this.#ladderEnv.getL4Connection(),
      this.#ladderEnv.getVersionedTextDocIdentifier(),
      veExprToEvExpr(this.#originalExpr),
      this.#bindings
    )
    this.setEvalResult(context, result)
    this.#nonViableSubgraph = this.computeNonViableSubgraph(context, result)
  }

  getValueOfUnique(_context: LirContext, unique: Unique): UBoolVal {
    const val = this.#bindings.get(unique)
    if (!val) {
      throw new Error(`Internal error: No value found for unique ${unique}`)
    }
    return val
  }

  /** This is what gets called when the user clicks on a node */
  async submitNewBinding(
    context: LirContext,
    binding: { unique: Unique; value: UBoolVal }
  ) {
    // Update the args with the new binding
    this.#bindings.set(binding.unique, binding.value)
    /*
    Try #WhatIf-style evaluation.
    ---------
    Note that
    * the `bindings` map will have a Unique key for every Var node,
      including Var nodes that the user hasn't specified a value for
    * the default value for a VarNode is UnknownV
    */
    await this.doEvalLadderExprWithVarBindings(context)

    this.getRegistry().publish(context, this.getId())
  }

  /*************************************
         Result
  **************************************/

  getResult(_context: LirContext) {
    return this.#evalResult.result
  }

  private setEvalResult(_context: LirContext, result: EvalResult) {
    this.#evalResult = result
  }

  /**************************************
      Zen mode,
    which is currently being treated as
    being equivalent to whether
    explanatory annotations are visible
  ***************************************/

  toggleZenModeStatus(context: LirContext) {
    const currZenModeStatus = this.#ladderEnv.shouldEnableZenMode()
    if (currZenModeStatus) {
      this.#ladderEnv.disableZenMode()
    } else {
      this.#ladderEnv.enableZenMode()
    }

    this.getRegistry().publish(context, this.getId())
  }

  /*****************************
            Misc
  ******************************/

  getChildren(context: LirContext) {
    const topVertices = this.getVertices(context)
    return [
      ...topVertices,
      ...topVertices
        .filter(isAppLirNode)
        .flatMap((appNode) => appNode.getArgs(context)),
    ]
  }

  dispose(context: LirContext) {
    this.getVertices(context).map((n) => n.dispose(context))
    this.#dag.dispose()
    context.clear(this.getId())
  }
}

export async function makeLadderGraphLirNode(
  nodeInfo: LirNodeInfo,
  dag: DirectedAcyclicGraph<LirId>,
  vizExprToLirGraph: Map<IRId, DirectedAcyclicGraph<LirId>>,
  originalExpr: IRExpr,
  ladderEnv: LadderEnv,
  noIntermediateBundlingNodeGraph: NoIntermediateBundlingNodeDag
) {
  augmentEdgesWithExplanatoryLabel(nodeInfo.context, ladderEnv, dag)

  const ladderGraph = isNnf(nodeInfo.context, dag)
    ? NNFLadderGraphLirNode.make(
        nodeInfo,
        dag,
        vizExprToLirGraph,
        originalExpr,
        ladderEnv,
        noIntermediateBundlingNodeGraph
      )
    : NonNNFLadderGraphLirNode.make(
        nodeInfo,
        dag,
        vizExprToLirGraph,
        originalExpr,
        ladderEnv
      )
  await ladderGraph.doEvalLadderExprWithVarBindings(nodeInfo.context)
  return ladderGraph
}

export function isNNFLadderGraphLirNode(
  node: LadderGraphLirNode
): node is NNFLadderGraphLirNode {
  return node instanceof NNFLadderGraphLirNode
}

export class NNFLadderGraphLirNode extends BaseLadderGraphLirNode {
  #nodeSelectionTracker: LadderNodeSelectionTracker
  /** The pathsList will have to be updated if (and only if) we change the structure of the graph.
   * No need to update it, tho, if changing edge attributes. */
  #pathsList: PathsListLirNode
  #highlightedSubgraph: DirectedAcyclicGraph<LirId> = empty()

  private constructor(
    nodeInfo: LirNodeInfo,
    dag: DirectedAcyclicGraph<LirId>,
    vizExprToLirGraph: Map<IRId, DirectedAcyclicGraph<LirId>>,
    originalExpr: IRExpr,
    ladderEnv: LadderEnv,
    nodeSelectionTracker: LadderNodeSelectionTracker,
    pathsList: PathsListLirNode
  ) {
    super(nodeInfo, dag, vizExprToLirGraph, originalExpr, ladderEnv)

    this.#nodeSelectionTracker = nodeSelectionTracker
    this.#pathsList = pathsList
  }

  static make(
    nodeInfo: LirNodeInfo,
    dag: DirectedAcyclicGraph<LirId>,
    vizExprToLirGraph: Map<IRId, DirectedAcyclicGraph<LirId>>,
    originalExpr: IRExpr,
    ladderEnv: LadderEnv,
    noIntermediateBundlingNodeGraph: NoIntermediateBundlingNodeDag
  ): NNFLadderGraphLirNode {
    const pathsList = new PathsListLirNode(
      nodeInfo,
      dag.getAllPaths().map((p) => new LinPathLirNode(nodeInfo, ladderEnv, p))
    )
    const nodeSelectionTracker = LadderNodeSelectionTracker.make(
      nodeInfo,
      dag,
      noIntermediateBundlingNodeGraph,
      pathsList
    )

    return new NNFLadderGraphLirNode(
      nodeInfo,
      dag,
      vizExprToLirGraph,
      originalExpr,
      ladderEnv,
      nodeSelectionTracker,
      pathsList
    )
  }

  nodeIsSelected(_context: LirContext, node: SelectableLadderLirNode) {
    return this.#nodeSelectionTracker.nodeIsSelected(node) ?? false
  }

  edgeIsInHighlightedSubgraph(_context: LirContext, edge: LadderLirEdge) {
    return this.#highlightedSubgraph.hasEdge(edge.getU(), edge.getV())
  }

  /** Helper: Update state that depends on what nodes user has selected */
  private updatePathsListAndHighlightedSubgraph(
    context: LirContext,
    selected: SelectableLadderLirNode[]
  ) {
    /* Every time there is a change in the user's node selection on the ladder graph --- which
    in effect corresponds to a change in what 
    nodes of noIntermediateBundlingNodeDag are selected ---
    we update our projections (a and b below). */

    // a. check if the user has selected nodes corresponding to lin path(s) in the ladder dag,
    // and select the corresponding path(s) in the PathsList.
    const correspondingLinPaths =
      this.#nodeSelectionTracker.findCorrespondingLinPaths(selected)
    this.#pathsList.selectPaths(context, correspondingLinPaths)

    // b. update the highlighted subgraph
    // Exploits the property that (G, +, ε) is an idempotent monoid
    this.#highlightedSubgraph = overlays(
      correspondingLinPaths.map((linPath) => linPath.getRawPathGraph())
    )

    // console.log('\n=== Lin Paths ===')
    // correspondingLinPaths.forEach((path, index) => {
    //   const nodeLabels = path.getVertices(context).map((node) => {
    //     return `${node.getId().toString()} (${node.toPretty(context)})`
    //   })
    //   console.log(`\nLin Path ${index + 1}:`)
    //   console.log(nodeLabels.join(' -> '))
    // })
    // console.log('=========================\n')
  }

  toggleNodeSelection(context: LirContext, node: SelectableLadderLirNode) {
    this.#nodeSelectionTracker.toggleNodeSelection(context, node)
    this.updatePathsListAndHighlightedSubgraph(
      context,
      this.#nodeSelectionTracker.getSelectedForHighlightPaths(context)
    )
    this.getRegistry().publish(context, this.getId())
  }

  selectNodes(context: LirContext, nodes: Array<SelectableLadderLirNode>) {
    this.#nodeSelectionTracker.selectNodes(context, nodes)
    this.updatePathsListAndHighlightedSubgraph(
      context,
      this.#nodeSelectionTracker.getSelectedForHighlightPaths(context)
    )
    this.getRegistry().publish(context, this.getId())
  }

  getSelectedNodes(context: LirContext) {
    return this.#nodeSelectionTracker.getSelectedForHighlightPaths(context)
  }

  getPathsList(_context: LirContext) {
    return this.#pathsList
  }

  toString(): string {
    return 'NNF_LADDER_GRAPH_LIR_NODE'
  }
}

export class NonNNFLadderGraphLirNode extends BaseLadderGraphLirNode {
  private constructor(
    nodeInfo: LirNodeInfo,
    dag: DirectedAcyclicGraph<LirId>,
    vizExprToLirGraph: Map<IRId, DirectedAcyclicGraph<LirId>>,
    originalExpr: IRExpr,
    ladderEnv: LadderEnv
  ) {
    super(nodeInfo, dag, vizExprToLirGraph, originalExpr, ladderEnv)
  }

  static make(
    nodeInfo: LirNodeInfo,
    dag: DirectedAcyclicGraph<LirId>,
    vizExprToLirGraph: Map<IRId, DirectedAcyclicGraph<LirId>>,
    originalExpr: IRExpr,
    ladderEnv: LadderEnv
  ): NonNNFLadderGraphLirNode {
    return new NonNNFLadderGraphLirNode(
      nodeInfo,
      dag,
      vizExprToLirGraph,
      originalExpr,
      ladderEnv
    )
  }

  toString(): string {
    return 'NON_NNF_LADDER_GRAPH_LIR_NODE'
  }
}

/**********************************************
          Ladder Lir Node
***********************************************/

/** All the LirNodes that can appear in the Ladder graph */
export type LadderLirNode =
  | UBoolVarLirNode
  | AppLirNode
  | NotStartLirNode
  | NotEndLirNode
  | BundlingFlowLirNode
  | TrueExprLirNode
  | FalseExprLirNode

/** The name is perhaps misleading: these are nodes that can be given a highlighted / selected style;
 * but not all the nodes here can be selected for highlighting by the user.
 * E.g., you will not be able to select a NotEnd node in the graph
 * by right-clicking on it --- the NOT subgraph
 * is selected by right-clicking on the NotSTART node.
 * TODO: Would be good to come up with a better name.
 */
export type SelectableLadderLirNode =
  | UBoolVarLirNode
  | AppLirNode
  | NotStartLirNode
  | NotEndLirNode
  | TrueExprLirNode
  | FalseExprLirNode

export function isSelectableLadderLirNode(
  node: LadderLirNode
): node is SelectableLadderLirNode {
  return (
    isUBoolVarLirNode(node) ||
    isAppLirNode(node) ||
    isNotStartLirNode(node) ||
    isNotEndLirNode(node) ||
    isTrueExprLirNode(node) ||
    isFalseExprLirNode(node)
  )
}

/**********************************************
        Bool Lit Lir Nodes
***********************************************/

export function isTrueExprLirNode(
  node: LadderLirNode
): node is TrueExprLirNode {
  return node instanceof TrueExprLirNode
}

export function isFalseExprLirNode(
  node: LadderLirNode
): node is FalseExprLirNode {
  return node instanceof FalseExprLirNode
}

export class FalseExprLirNode extends BaseFlowLirNode implements FlowLirNode {
  #name: Name
  #value = new FalseVal()

  constructor(
    nodeInfo: LirNodeInfo,
    name: Name,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
    this.#name = name
  }

  getValue(_context: LirContext) {
    return this.#value
  }

  toPretty(_context: LirContext) {
    return this.#name.label
  }

  toString(): string {
    return 'FALSE_EXPR_LIR_NODE'
  }
}

export class TrueExprLirNode extends BaseFlowLirNode implements FlowLirNode {
  #name: Name
  #value = new TrueVal()

  constructor(
    nodeInfo: LirNodeInfo,
    name: Name,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
    this.#name = name
  }

  getValue(_context: LirContext) {
    return this.#value
  }

  toPretty(_context: LirContext) {
    return this.#name.label
  }

  toString(): string {
    return 'TRUE_EXPR_LIR_NODE'
  }
}

/**********************************************
        UBoolVar Lir Node
***********************************************/

// TODO: Prob just remove this
export interface VarLirNode extends FlowLirNode {
  getUnique(context: LirContext): Unique

  /** This should be used only for UI purposes. This is just a thin wrapper over the LadderGraphLirNode's getValueOfUnique. */
  getValue(context: LirContext, ladderGraph: LadderGraphLirNode): UBoolVal
}

export function isUBoolVarLirNode(
  node: LadderLirNode
): node is UBoolVarLirNode {
  return node instanceof UBoolVarLirNode
}

/* For now, changes to the data associated with BoolVarLirNodes will be published
by the LadderGraphLirNode, as opposed to the BoolVarLirNode itself.
*/
export class UBoolVarLirNode extends BaseFlowLirNode implements VarLirNode {
  #originalExpr: UBoolVar
  #initialValue: UBoolVal

  constructor(
    nodeInfo: LirNodeInfo,
    originalExpr: UBoolVar,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
    this.#originalExpr = originalExpr
    this.#initialValue = toUBoolVal(originalExpr.value)
  }

  getLabel(_context: LirContext) {
    return this.#originalExpr.name.label
  }

  getUnique(_context: LirContext) {
    return this.#originalExpr.name.unique
  }

  canInline(_context: LirContext) {
    return this.#originalExpr.canInline
  }

  getInitialValue(_context: LirContext): UBoolVal {
    return this.#initialValue
  }

  getValue(context: LirContext, ladderGraph: LadderGraphLirNode): UBoolVal {
    return ladderGraph.getValueOfUnique(context, this.getUnique(context))
  }

  toPretty(context: LirContext) {
    return this.getLabel(context)
  }

  toString(): string {
    return 'UBOOL_VAR_LIR_NODE'
  }
}

/**********************************************
        NotStart, NotEnd Lir Nodes
***********************************************/

export function isNotStartLirNode(
  node: LadderLirNode
): node is NotStartLirNode {
  return node instanceof NotStartLirNode
}

export class NotStartLirNode extends BaseFlowLirNode implements FlowLirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    private readonly negand: DirectedAcyclicGraph<LirId>,
    private readonly originalExpr: IRExpr,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
  }

  getNegand(_context: LirContext) {
    return this.negand
  }

  getOriginalExpr(_context: LirContext) {
    return this.originalExpr
  }

  getWholeNotGraph(_context: LirContext, ladderGraph: LadderGraphLirNode) {
    return ladderGraph.getVizExprToLirGraph().get(this.originalExpr.id)
  }

  toPretty(_context: LirContext) {
    return 'NOT ('
  }

  toString(): string {
    return 'NOT_START_LIR_NODE'
  }
}

export function isNotEndLirNode(node: LadderLirNode): node is NotEndLirNode {
  return node instanceof NotEndLirNode
}

export class NotEndLirNode extends BaseFlowLirNode implements FlowLirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
  }

  toPretty(_context: LirContext) {
    return ')'
  }

  toString(): string {
    return 'NOT_END_LIR_NODE'
  }
}

/**********************************************
        App Lir Node
***********************************************/

/** Temporarily restricting args to something really simple. */
export type AppArgLirNode = UBoolVarLirNode

export function isAppLirNode(node: LadderLirNode): node is AppLirNode {
  return node instanceof AppLirNode
}

export class AppLirNode extends BaseFlowLirNode implements FlowLirNode {
  #fnName: Name
  #args: LirId[]

  constructor(
    nodeInfo: LirNodeInfo,
    fnName: Name,
    args: AppArgLirNode[],
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
    this.#fnName = fnName
    this.#args = args.map((arg) => arg.getId())
  }

  getFnName(_context: LirContext) {
    return this.#fnName
  }

  getArgs(context: LirContext) {
    return this.#args.map((arg) => context.get(arg) as AppArgLirNode)
  }

  getChildren(context: LirContext) {
    return this.getArgs(context)
  }

  toPretty(context: LirContext): string {
    return `${this.#fnName.label} OF ${this.#args
      .map((arg) => context.get(arg) as LadderLirNode)
      .map((arg) => arg.toPretty(context))
      .join(', ')}`
  }

  toString() {
    return 'APP_LIR_NODE'
  }
}

/******************************
    Bundling Flow Lir Node
*******************************/

export type BundlingNodeAnno = Pick<BundlingNodeDisplayerData, 'annotation'>

export const emptyBundlingNodeAnno: BundlingNodeAnno = { annotation: '' }

export function isBundlingFlowLirNode(
  node: LadderLirNode
): node is BundlingFlowLirNode {
  return isSourceLirNode(node) || isSinkLirNode(node)
}

/** A Flow Lir Node that's used solely to visually group or 'bundle' other nodes.
 *
 * Using `bundling` because `group` has a specific meaning in the React/SvelteFlow context.
 */
export type BundlingFlowLirNode = SourceLirNode | SinkLirNode
export type SourceLirNode = SourceWithOrAnnoLirNode | SourceNoAnnoLirNode

export function isSourceLirNode(node: LadderLirNode): node is SourceLirNode {
  return isSourceNoAnnoLirNode(node) || isSourceWithOrAnnoLirNode(node)
}
export function isSinkLirNode(node: LadderLirNode): node is SinkLirNode {
  return node instanceof SinkLirNode
}

abstract class BaseBundlingFlowLirNode extends BaseFlowLirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    /** TODO: Think about whether to have the param be a `data` plain object / record instead */
    protected readonly annotation: BundlingNodeDisplayerData['annotation'],
    position: Position
  ) {
    super(nodeInfo, position)
  }

  getData(_context: LirContext) {
    return { annotation: this.annotation }
  }
}

export function isSourceNoAnnoLirNode(
  node: LadderLirNode
): node is SourceNoAnnoLirNode {
  return node instanceof SourceNoAnnoLirNode
}

export class SourceNoAnnoLirNode
  extends BaseBundlingFlowLirNode
  implements FlowLirNode
{
  constructor(
    nodeInfo: LirNodeInfo,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, emptyBundlingNodeAnno.annotation, position)
  }

  toPretty(_context: LirContext) {
    return ''
  }

  toString(): string {
    return 'SOURCE_NO_ANNO_LIR_NODE'
  }
}

export function isSourceWithOrAnnoLirNode(
  node: LadderLirNode
): node is SourceWithOrAnnoLirNode {
  return node instanceof SourceWithOrAnnoLirNode
}

export class SourceWithOrAnnoLirNode
  extends BaseBundlingFlowLirNode
  implements FlowLirNode
{
  constructor(
    nodeInfo: LirNodeInfo,
    annotation: BundlingNodeDisplayerData['annotation'],
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, annotation, position)
  }

  toPretty(_context: LirContext) {
    return ''
  }

  toString(): string {
    return 'SOURCE_WITH_OR_ANNO_LIR_NODE'
  }
}

export class SinkLirNode
  extends BaseBundlingFlowLirNode
  implements FlowLirNode
{
  constructor(
    nodeInfo: LirNodeInfo,
    annotation: BundlingNodeDisplayerData['annotation'] = emptyBundlingNodeAnno.annotation,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, annotation, position)
  }

  toPretty(_context: LirContext) {
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

export function augmentEdgesWithExplanatoryLabel(
  context: LirContext,
  env: LadderEnv,
  graph: DirectedAcyclicGraph<LirId>
) {
  const edges = graph.getEdges()

  const isEdgeToAddAndLabel = (edge: DirectedEdge<LirId>) => {
    const edgeU = context.get(edge.getU()) as LadderLirNode
    const edgeV = context.get(edge.getV()) as LadderLirNode

    const targetIsEligible =
      isUBoolVarLirNode(edgeV) ||
      isNotStartLirNode(edgeV) ||
      isSourceWithOrAnnoLirNode(edgeV) ||
      isAppLirNode(edgeV)

    const sourceIsNotOverallSource =
      graph.getSource() && !vertex(edge.getU()).isEqualTo(graph.getSource())
    const sourceIsEligible =
      !isSourceWithOrAnnoLirNode(edgeU) &&
      sourceIsNotOverallSource &&
      !isNotStartLirNode(edgeU)

    return sourceIsEligible && targetIsEligible
  }

  const edgesToAddLabel = edges.filter(isEdgeToAddAndLabel)
  edgesToAddLabel.forEach((edge) => {
    const attrs = graph.getAttributesForEdge(edge)
    attrs.setLabel(env.getExplanatoryAndEdgeLabel())
    graph.setEdgeAttributes(edge, attrs)
  })
}
