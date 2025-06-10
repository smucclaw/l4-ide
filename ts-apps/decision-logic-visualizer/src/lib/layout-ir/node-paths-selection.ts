import {
  type LadderLirNode,
  type SelectableLadderLirNode,
  LinPathLirNode,
  isSelectableLadderLirNode,
  isBundlingFlowLirNode,
} from './ladder-graph/ladder.svelte.js'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode, LirRegistry } from './core.js'
import { type DirectedAcyclicGraph, vertex } from '../algebraic-graphs/dag.js'
import ArrayKeyedMap from 'array-keyed-map'
import type { Branded } from '../utils.js'

/************************************************
          NoIntermediateBundlingNodeDag
*************************************************/

/** A variant of the main ladder dag that (i) has the same source and sink as the main ladder dag,
 * but (ii) does not have *intermediate* bundling nodes.
 * (I.e., this dag does have bundling nodes: namely, the overall source and sink nodes.)
 */
export type NoIntermediateBundlingNodeDag = Branded<
  DirectedAcyclicGraph<LirId>,
  'NoIntermediateBundlingNodeDag'
>

export function makeNoIntermediateBundlingNodeDag(
  context: LirContext,
  dag: DirectedAcyclicGraph<LirId>
): NoIntermediateBundlingNodeDag {
  const intermediateVertices = dag
    .getVertices()
    .filter(
      (v) =>
        !dag.getSource().isEqualTo(vertex(v)) &&
        !dag.getSink().isEqualTo(vertex(v))
    )
  if (
    intermediateVertices.some((v) =>
      isBundlingFlowLirNode(context.get(v) as LadderLirNode)
    )
  ) {
    throw new Error(
      'NoIntermediateBundlingNodeDag cannot have intermediate bundling nodes'
    )
  }
  return dag as NoIntermediateBundlingNodeDag
}

/************************************************
          PathsListLirNode
*************************************************/

export function isPathsListLirNode(
  node: PathsListLirNode
): node is PathsListLirNode {
  return node instanceof PathsListLirNode
}

/** Assumes that the original ladder graph is in NNF.
 *
 * Tracks what the linearized paths through the ladder graph are,
 * as well as what paths are selected by the user
 * (whether by interacting with the main graph UI or with the paths list UI).
 *
 * This is the abstract LirNode that gets rendered by {@link $lib/displayers/paths-list.svelte}
 */
export class PathsListLirNode extends DefaultLirNode implements LirNode {
  private paths: Array<LirId>
  /** What lin paths in the paths list are selected.
   * Paths can be selected, not just via interaction with the PathsList UI,
   * but also indirectly, by selecting nodes on the main graph UI.
   */
  private selected: Set<LirId> = new Set()

  constructor(nodeInfo: LirNodeInfo, paths: Array<LinPathLirNode>) {
    super(nodeInfo)
    this.paths = paths.map((n) => n.getId())
  }

  getPaths(context: LirContext) {
    return this.paths.map((id) => context.get(id)) as Array<LinPathLirNode>
  }

  getSelectedPaths(context: LirContext) {
    return Array.from(this.selected)
      .map((id) => context.get(id) as LinPathLirNode)
      .toSorted((a, b) => a.compare(b))
  }

  /** Select the linearized paths in the PathsList (*without* updating any related state) */
  selectPaths(context: LirContext, paths: LinPathLirNode[]) {
    this.selected = new Set(paths.map((p) => p.getId()))

    this.getRegistry().publish(context, this.getId())
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

/************************************************
          LadderNodeSelectionTracker
*************************************************/

type PathInNoIntermediateBundlingNodeDag = LirId[]

function linPathToPathInNoIntermediateBundlingNodeDag(
  context: LirContext,
  originalLadderDag: DirectedAcyclicGraph<LirId>,
  linPath: LinPathLirNode
): PathInNoIntermediateBundlingNodeDag {
  return linPath
    .getRawPathGraph()
    .getTopSort()
    .filter((v) => {
      const isOverallSource = originalLadderDag.getSource().isEqualTo(vertex(v))
      const isOverallSink = originalLadderDag.getSink().isEqualTo(vertex(v))
      return (
        isOverallSource ||
        isOverallSink ||
        isSelectableLadderLirNode(context.get(v) as LadderLirNode)
      )
    })
}

/**
 * Tracks what nodes the user has selected on the ladder graph,
 * and how the nodes that the user has selected on the ladder graph
 * might correspond to lin paths through the ladder graph.
 *
 * Has all the info needed to update the projections
 * (which lin paths are selected in the PathsList; which edges on the ladder graph are highlighted)
 *
 * Assumes that the original ladder graph is in NNF.
 */
export class LadderNodeSelectionTracker {
  /** What nodes on the ladder graph, if any, the user has selected for highlighting *by interacting with the main graph UI*.
   *
   * (This is different from the selected paths in the PathsList --- those are the paths that the user has selected by interacting with the PathsList UI.)
   * The LirIds will correspond to those of SelectableFlowLirNodes.
   */
  #selected: Set<LirId> = new Set()

  static make(
    nodeInfo: LirNodeInfo,
    /** The ladder graph dag. Assumes that this is in NNF. */
    dag: DirectedAcyclicGraph<LirId>,
    noIntermediateBundlingNodeDag: NoIntermediateBundlingNodeDag,
    pathsList: PathsListLirNode
  ) {
    /* 
     * We are in effect maintaining two representations of the ladder graph:
    *
    * 1. The main ladder graph
    * 2. The noIntermediateBundlingNodeDag: The ladder graph, where the only bundling nodes are the overall source and sink (i.e., no intermediate bundling nodes)
    
    The noIntermediateBundlingNodeDag still has bundling nodes --- the overall source and sink nodes.
    It's just that it doesn't have *intermediate* bundling nodes.

    This gets used to figure out what paths on the main graph the user is trying to highlight,
    when they select nodes on the main graph.

    See l4-ide/doc/dev/frontend/no-intermediate-bundling-node-dag.md
    for more on how to debug and understand this better.
    */

    const noBundlingNodePathToLadderLinPath = new ArrayKeyedMap<
      PathInNoIntermediateBundlingNodeDag,
      LinPathLirNode
    >(
      pathsList
        .getPaths(nodeInfo.context)
        .map(
          (linPath): [PathInNoIntermediateBundlingNodeDag, LinPathLirNode] => {
            return [
              linPathToPathInNoIntermediateBundlingNodeDag(
                nodeInfo.context,
                dag,
                linPath
              ),
              linPath,
            ]
          }
        )
    )
    // console.log('\n=== noBundlingNodePathToLadderLinPath ===')
    // noBundlingNodePathToLadderLinPath.forEach(
    //   (_, noIntermedBundlingNodePath) => {
    //     const nodeLabels = noIntermedBundlingNodePath.map((id) => {
    //       return `${id.toString()}(${(nodeInfo.context.get(id) as LadderLirNode).toPretty(nodeInfo.context)})`
    //     })
    //     console.log(nodeLabels.join(' -> '))
    //   }
    // )
    // console.log('===================================================\n')

    return new LadderNodeSelectionTracker(
      nodeInfo.registry,
      noIntermediateBundlingNodeDag,
      noBundlingNodePathToLadderLinPath,
      pathsList
    )
  }

  constructor(
    private lirRegistry: LirRegistry,
    private noIntermediateBundlingNodeDag: NoIntermediateBundlingNodeDag,
    /** Map from path in the noIntermediateBundlingNodeDag to the corresponding lin path on the ladder graph */
    private noBundlingNodePathToLadderLinPath: ArrayKeyedMap<
      PathInNoIntermediateBundlingNodeDag,
      LinPathLirNode
    >,
    private pathsList: PathsListLirNode
  ) {}

  /** Get the nodes on the ladder graph that the user has selected for highlighting by interacting with the main graph UI */
  getSelectedForHighlightPaths(context: LirContext) {
    return Array.from(this.#selected).map(
      (id) => context.get(id) as SelectableLadderLirNode
    )
  }

  nodeIsSelected(node: SelectableLadderLirNode) {
    return this.#selected.has(node.getId())
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  clearSelectedNodes(_context: LirContext) {
    this.#selected = new Set()
  }

  selectNodes(context: LirContext, nodes: Array<SelectableLadderLirNode>) {
    this.clearSelectedNodes(context)

    this.#selected = new Set(nodes.map((node) => node.getId()))
    nodes.forEach((node) => this.lirRegistry.publish(context, node.getId()))
  }

  /** Toggle whether a specific node is selected for highlighting (and update derived state) */
  toggleNodeSelection(context: LirContext, node: SelectableLadderLirNode) {
    if (this.#selected.has(node.getId())) {
      this.#selected.delete(node.getId())
    } else {
      this.#selected.add(node.getId())
    }
    this.lirRegistry.publish(context, node.getId())
  }

  /** Given the selected nodes, figure out what lin paths through the ladder graph, if any, these correspond to */
  findCorrespondingLinPaths(selected: Array<SelectableLadderLirNode>) {
    /** 1. Compute the paths through the selected subgraph of the noIntermediateBundlingNodeDag
     * (These paths will start from noIntermediateBundlingNodeDag's source.)
     */
    const selectedIds = new Set(selected.map((node) => node.getId()))

    const pathsSelectedSubgraphOfNoBundlingNodeGraph =
      this.noIntermediateBundlingNodeDag
        .induce((nodeId: LirId) => {
          const isSource = this.noIntermediateBundlingNodeDag
            .getSource()
            .isEqualTo(vertex(nodeId))
          const isSink = this.noIntermediateBundlingNodeDag
            .getSink()
            .isEqualTo(vertex(nodeId))
          return selectedIds.has(nodeId) || isSource || isSink
        })
        .getAllPaths()

    // console.log(
    //   '=======================  computePathsThroughSelectedSubgraphOfNoBundlingNodeGraph =====================\n'
    // )
    // console.log(
    //   'selected',
    //   Array.from(selectedIds).map((id) => id.toString())
    // )
    // console.log(
    //   'noIntermediateBundlingNodeDag',
    //   this.noIntermediateBundlingNodeDag.toString()
    // )
    // pathsSelectedSubgraphOfNoBundlingNodeGraph.forEach((p, index) =>
    //   console.log(
    //     `---- pathsSelectedSubgraphOfNoBundlingNodeGraph path ${index}: `,
    //     p.toString()
    //   )
    // )

    /** 2. Get the lin paths / subgraph of #dag that correspond to the (computed) paths through the selected subgraph of the noIntermediateBundlingNodeDag */
    return pathsSelectedSubgraphOfNoBundlingNodeGraph
      .map((nbnPathGraph) =>
        this.noBundlingNodePathToLadderLinPath.get(nbnPathGraph.getTopSort())
      )
      .filter((linPath) => !!linPath)
  }
}
