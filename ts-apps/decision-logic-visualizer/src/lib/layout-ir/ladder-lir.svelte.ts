/* eslint-disable @typescript-eslint/no-unused-vars */
import {
  type Value,
  type BoolVal,
  TrueVal,
  FalseVal,
  UnknownVal,
} from './value.js'
import type { BoolVar, Unique, Name } from '@repo/viz-expr'
import { Environment } from './environment.js'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import type { Ord } from '$lib/utils.js'
import { ComparisonResult } from '$lib/utils.js'
import {
  isEmpty,
  isVertex,
  isOverlay,
  isConnect,
  overlay,
  empty,
  foldg,
  type Overlay,
  type Connect,
  type Vertex,
  type DirectedAcyclicGraph,
} from '../algebraic-graphs/dag.js'
import {
  type Edge,
  DirectedEdge,
  EmptyEdgeStyles,
  HighlightedEdgeStyles,
  type EdgeStyles,
  type EdgeAttributes,
} from '../algebraic-graphs/edge.js'
import type {
  Dimensions,
  BundlingNodeDisplayerData,
} from '$lib/displayers/flow/svelteflow-types.js'
import { match, P } from 'ts-pattern'
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

  dispose(context: LirContext) {
    this.getChildren(context).map((n) => n.dispose(context))

    context.clear(this.getId())
  }

  toString(): string {
    return 'FUN_DECL_LIR_NODE'
  }
}

/*************************************************
              Path Lir Node
 *************************************************/

export class PathsListLirNode extends DefaultLirNode implements LirNode {
  private paths: Array<LirId>

  constructor(
    nodeInfo: LirNodeInfo,
    protected ladderGraph: LadderGraphLirNode,
    paths: Array<LinPathLirNode>
  ) {
    super(nodeInfo)
    this.paths = paths.map((n) => n.getId())
  }

  getPaths(context: LirContext) {
    return this.paths.map((id) => context.get(id)) as Array<LinPathLirNode>
  }

  highlightPaths(context: LirContext, paths: LirId[]) {
    const pathLirNodes = paths.map((id) =>
      context.get(id)
    ) as Array<LinPathLirNode>

    // 1. Get the subgraph to be highlighted
    // Exploits the property that (G, +, ε) is an idempotent monoid
    const graphToHighlight = pathLirNodes
      .map((p) => p._getRawPath())
      .reduceRight(overlay, empty())

    // 2. Reset all edge styles on ladder graph, then highlight the subgraph
    this.ladderGraph.clearAllEdgeStyles(context)
    this.setStylesOnLadderSubgraph(
      context,
      new HighlightedEdgeStyles(),
      graphToHighlight
    )
  }

  protected setStylesOnLadderSubgraph(
    context: LirContext,
    styles: EdgeStyles,
    subgraph: DirectedAcyclicGraph<LirId>
  ) {
    const pathEdges = subgraph.getEdges()
    pathEdges.forEach((edge) => {
      this.ladderGraph.setEdgeStyles(context, edge, styles)
    })
  }

  getChildren(context: LirContext) {
    return this.getPaths(context)
  }

  dispose(context: LirContext) {
    // Dispose members
    this.getChildren(context).map((n) => n.dispose(context))
    this.paths = []

    // Dispose self
    context.clear(this.getId())
  }

  toString(): string {
    return 'PATHS_LIST_LIR_NODE'
  }
}

/** The simplest version of the LinPathLirNode -- no distinguishing between
 * compatible and incompatible linearized paths
 */
export class LinPathLirNode extends DefaultLirNode implements LirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    protected rawPath: DirectedAcyclicGraph<LirId>
  ) {
    super(nodeInfo)
  }

  /** To be called only by the PathsListLirNode */
  _getRawPath() {
    return this.rawPath
  }

  getVertices(context: LirContext) {
    return this.rawPath
      .getVertices()
      .map((id) => context.get(id))
      .filter((n) => !!n) as LadderLirNode[]
  }

  dispose(context: LirContext) {
    this.rawPath.dispose()
    context.clear(this.getId())
  }

  toPretty(context: LirContext) {
    return pprintPathGraph(context, this.rawPath)
  }

  toString() {
    return 'LIN_PATH_LIR_NODE'
  }
}

// TODO: Differentiate between compatible and incompatible linearized paths

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

  dispose(context: LirContext): void {
    context.clear(this.getId())
  }

  abstract toPretty(context: LirContext): string
}

/**********************************************
          Ladder Graph Lir Node
***********************************************/

/*
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
export class LadderGraphLirNode extends DefaultLirNode implements LirNode {
  #dag: DirectedAcyclicGraph<LirId>
  /** This will have to be updated if (and only if) we change the structure of the graph.
   * No need to update it, tho, if changing edge attributes.
   */
  #pathsList?: PathsListLirNode
  #environment: Environment

  constructor(nodeInfo: LirNodeInfo, dag: DirectedAcyclicGraph<LirId>) {
    super(nodeInfo)
    this.#dag = dag

    // Make the initial environment
    const varNodes = getVerticesFromAlgaDag(nodeInfo.context, this.#dag).filter(
      (n) => n instanceof BoolVarLirNode
    )
    const initialEnv = varNodes.map((n) => n.getValue(nodeInfo.context))
    const initialCoreferents: Array<Set<LirId>> = []
    varNodes.forEach((n) => {
      const unique = n.getUnique(nodeInfo.context)
      if (!initialCoreferents[unique]) {
        initialCoreferents[unique] = new Set<LirId>([n.getId()])
      } else {
        initialCoreferents[unique].add(n.getId())
      }
    })
    this.#environment = new Environment(initialEnv, initialCoreferents)
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

  // TODO: differentiate between subgraph that is compatible with the updated env and subgraph that isn't
  /** Get list of all simple paths through the Dag */
  getPathsList(context: LirContext) {
    if (!this.#pathsList) {
      const rawPaths = this.#dag.getAllPaths()
      const paths = rawPaths.map(
        (rawPath) => new LinPathLirNode(this.makeNodeInfo(context), rawPath)
      )
      this.#pathsList = new PathsListLirNode(
        this.makeNodeInfo(context),
        this,
        paths
      )
    }

    return this.#pathsList
  }

  getOverallSource(context: LirContext): undefined | SourceLirNode {
    const source = this.#dag.getSource()
    if (!isVertex(source)) return undefined
    return context.get(source.getValue()) as SourceLirNode
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
    const attrs = this.#dag.getAttributesForEdge(edge)
    // Ok to mutate because getAttributesForEdge returns a cloned object
    attrs.setStyles(styles)
    this.#dag.setEdgeAttributes(edge, attrs)

    this.getRegistry().publish(context, this.getId())
  }

  clearAllEdgeStyles(context: LirContext) {
    const edges = this.#dag.getEdges()
    edges.forEach((edge) => {
      const attrs = this.#dag.getAttributesForEdge(edge)
      // Ok to mutate because getAttributesForEdge returns a cloned object
      attrs.setStyles(new EmptyEdgeStyles())
      this.#dag.setEdgeAttributes(edge, attrs)
    })

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
    const attrs = this.#dag.getAttributesForEdge(edge)
    attrs.setLabel(label)
    this.#dag.setEdgeAttributes(edge, attrs)

    this.getRegistry().publish(context, this.getId())
  }

  /*****************************
          Bindings
  ******************************/

  /** This sets off the 'compound logic' */
  submitNewBinding(
    context: LirContext,
    binding: { unique: Unique; value: Value }
  ) {
    // Set the new binding
    this.#environment.set(binding)

    // The key step: Update all VarLirNodes with this Unique with the new Value
    const corefs = Array.from(this.#environment.getCoreferents(binding.unique))
    // console.log('corefs: ', corefs)
    corefs.forEach((coref) => {
      const node = context.get(coref) as VarLirNode
      node._setValue(context, binding.value)
    })

    this.getRegistry().publish(context, this.getId())
  }

  /**************************************
      Zen mode,
    which is currently being treated as
    being equivalent to whether
    explanatory annotations are visible
  ***************************************/

  toggleZenModeStatus(context: LirContext) {
    const currZenModeStatus = context.shouldEnableZenMode()
    if (currZenModeStatus) {
      context.disableZenMode()
    } else {
      context.enableZenMode()
    }

    this.getRegistry().publish(context, this.getId())
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

  dispose(context: LirContext) {
    this.getVertices(context).map((n) => n.dispose(context))
    this.#environment.dispose()
    this.#dag.dispose()
    context.clear(this.getId())
  }
}

/** Helper function */
function getVerticesFromAlgaDag(
  context: LirContext,
  dag: DirectedAcyclicGraph<LirId>
): LadderLirNode[] {
  return Array.from(dag.getVertices()).map(
    (id) => context.get(id) as LadderLirNode
  )
}

/**********************************************
          Ladder Lir Node
***********************************************/

export type LadderLirNode =
  | BoolVarLirNode
  | NotStartLirNode
  | NotEndLirNode
  | BundlingFlowLirNode

export interface VarLirNode extends FlowLirNode {
  getUnique(context: LirContext): Unique

  getValue(context: LirContext): Value
  /** For displayers.
   *
   * This will only be invoked by LadderGraphLirNode. */
  _setValue(context: LirContext, value: Value): void
}

export function isBoolVarLirNode(node: LadderLirNode): node is BoolVarLirNode {
  return node instanceof BoolVarLirNode
}

/* For now, changes to the data associated with BoolVarLirNodes will be published
by the LadderGraphLirNode, as opposed to the BoolVarLirNode itself.
*/
export class BoolVarLirNode extends BaseFlowLirNode implements VarLirNode {
  #value: BoolVal
  #name: Name

  constructor(
    nodeInfo: LirNodeInfo,
    originalExpr: BoolVar,
    position: Position = DEFAULT_INITIAL_POSITION
  ) {
    super(nodeInfo, position)
    this.#value = match(originalExpr.value)
      .with('True', () => new TrueVal())
      .with('False', () => new FalseVal())
      .with('Unknown', () => new UnknownVal())
      .exhaustive()
    this.#name = originalExpr.name
  }

  getLabel(_context: LirContext) {
    return this.#name.label
  }

  getUnique(_context: LirContext) {
    return this.#name.unique
  }

  getData(_context: LirContext) {
    return { name: this.#name, value: this.#value }
  }

  getValue(_context: LirContext): BoolVal {
    return this.#value
  }

  /** This will only be invoked by LadderGraphLirNode */
  _setValue(_context: LirContext, value: BoolVal) {
    this.#value = value
  }

  toPretty(_context: LirContext) {
    return this.getLabel(_context)
  }

  toString(): string {
    return 'BOOL_VAR_LIR_NODE'
  }
}

export function isNotStartLirNode(
  node: LadderLirNode
): node is NotStartLirNode {
  return node instanceof NotStartLirNode
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

export type BundlingNodeAnno = Pick<BundlingNodeDisplayerData, 'annotation'>

export const emptyBundlingNodeAnno: BundlingNodeAnno = { annotation: '' }

export function isBundlingFlowLirNode(
  node: LadderLirNode
): node is BundlingFlowLirNode {
  return isSourceLirNode(node) || node instanceof SinkLirNode
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

abstract class BaseBundlingFlowLirNode extends BaseFlowLirNode {
  constructor(
    nodeInfo: LirNodeInfo,
    /** TODO: Think about whether to have the param be a `data` plain object / record instead */
    protected readonly annotation: BundlingNodeDisplayerData['annotation'],
    position: Position
  ) {
    super(nodeInfo, position)
  }

  getData() {
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

  toPretty() {
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

  toPretty() {
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

export function augmentEdgesWithExplanatoryLabel(
  context: LirContext,
  ladderGraph: LadderGraphLirNode
) {
  const edges = ladderGraph.getEdges(context)

  const isEdgeToAddAndLabel = (edge: LadderLirEdge) => {
    const edgeU = context.get(edge.getU()) as LadderLirNode
    const edgeV = context.get(edge.getV()) as LadderLirNode

    const edgeSourceIsNotOverallSource =
      ladderGraph.getOverallSource(context) &&
      !edge
        .getU()
        .isEqualTo(ladderGraph.getOverallSource(context)?.getId() as LirId)

    const edgeSourceIsAnyOfAnnoSource = isSourceWithOrAnnoLirNode(edgeU)
    return (
      !edgeSourceIsAnyOfAnnoSource &&
      edgeSourceIsNotOverallSource &&
      !isNotStartLirNode(edgeU) &&
      (isBoolVarLirNode(edgeV) ||
        isNotStartLirNode(edgeV) ||
        isSourceWithOrAnnoLirNode(edgeV))
    )
  }

  const edgesToAddLabel = edges.filter(isEdgeToAddAndLabel)
  edgesToAddLabel.forEach((edge) => {
    ladderGraph.setEdgeLabel(
      context,
      edge,
      context.getExplanatoryAndEdgeLabel()
    )
  })
}

/** Bit hacky? */
function pprintPathGraph(
  context: LirContext,
  initialGraph: DirectedAcyclicGraph<LirId>
): string {
  const processed = new Set<LirId>()

  function pprintHelper(
    context: LirContext,
    g: DirectedAcyclicGraph<LirId>
  ): string {
    return match(g)
      .with(P.when(isEmpty<LirId>), () => '')
      .with(P.when(isVertex<LirId>), (v: Vertex<LirId>) => {
        if (processed.has(v.getValue())) return ''
        processed.add(v.getValue())
        return (context.get(v.getValue()) as LadderLirNode).toPretty(context)
      })
      .with(P.when(isOverlay<LirId>), (o: Overlay<LirId>) => {
        return (
          pprintHelper(context, o.getLeft()) +
          ' ' +
          pprintHelper(context, o.getRight())
        )
      })
      .with(P.when(isConnect<LirId>), (c: Connect<LirId>) => {
        const from = pprintHelper(context, c.getFrom())
        const to = pprintHelper(context, c.getTo())

        if (isVertex(c.getFrom()) && isVertex(c.getTo())) {
          const edgeAttrs = initialGraph.getAttributesForEdge(
            new DirectedEdge(
              (c.getFrom() as Vertex<LirId>).getValue(),
              (c.getTo() as Vertex<LirId>).getValue()
            )
          )
          const label = edgeAttrs.getLabel()
          return `${from} ${label} ${to}`
        } else {
          return `${from} ${to}`
        }
      })
      .exhaustive()
  }

  return pprintHelper(context, initialGraph)
}
