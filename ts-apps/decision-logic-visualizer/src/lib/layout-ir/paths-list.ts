import {
  type LadderLirNode,
  type SelectableLadderLirNode,
  LadderGraphLirNode,
  LinPathLirNode,
  isSelectableLadderLirNode,
} from './ladder-graph/ladder.svelte.js'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import {
  type DirectedAcyclicGraph,
  overlays,
  vertex,
} from '../algebraic-graphs/dag.js'
import ArrayKeyedMap from 'array-keyed-map'

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
    return Array.from(this.selected).map(
      (id) => context.get(id) as LinPathLirNode
    )
  }

  getSelectableNodesOfSelectedPaths(
    context: LirContext
  ): Array<SelectableLadderLirNode> {
    return this.getSelectedPaths(context).flatMap((p) =>
      p.getSelectableVertices(context)
    )
  }

  /** Select the linearized paths in the PathsList */
  selectPaths(context: LirContext, paths: LinPathLirNode[]) {
    this.selected = new Set(paths.map((p) => p.getId()))

    this.getRegistry().publish(context, this.getId())
  }

  // /** Helper: Highlight the paths on the ladder graph that correspond to the given LinPathLirNodes */
  // highlightPathsOnLadderGraph(
  //   context: LirContext,
  //   ladderGraph: LadderGraphLirNode,
  //   paths: LinPathLirNode[]
  // ) {
  //   // 1. Get the subgraph to be highlighted
  //   // Exploits the property that (G, +, ε) is an idempotent monoid
  //   const graphToHighlight = overlays(paths.map((p) => p.getRawPathGraph()))

  //   // 2. Reset edge styles wrt highlighting on ladder graph, then add highlight style to the subgraph
  //   ladderGraph.clearHighlightEdgeStyles(context)
  //   ladderGraph.highlightSubgraphEdges(context, graphToHighlight)
  // }

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

// export function contractIntermediateBundlingNodes(
//   context: LirContext,
//   dag: DirectedAcyclicGraph<LirId>
// ): DirectedAcyclicGraph<LirId> {
//   const overallSource = dag.getSource()
//   const overallSink = dag.getSink()

//   const isIntermediateBundlingNode = (node: LadderLirNode): boolean => {
//     const isOverallSource = vertex(node.getId()).isEqualTo(overallSource)
//     const isOverallSink = vertex(node.getId()).isEqualTo(overallSink)
//     return isBundlingFlowLirNode(node) && !isOverallSource && !isOverallSink
//   }
//   // TODO
// }

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
  #selectedForHighlightPaths: Set<LirId> = new Set()

  static make(
    nodeInfo: LirNodeInfo,
    /** The ladder graph dag. Assumes that this is in NNF. */
    dag: DirectedAcyclicGraph<LirId>,
    // TODO: should use a branded type for this
    noIntermediateBundlingNodeDag: DirectedAcyclicGraph<LirId>,
    pathsList: PathsListLirNode
  ) {
    console.log('ladder graph dag: ', dag.toString())

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
    console.log(
      'noIntermediateBundlingNodeDag: ',
      noIntermediateBundlingNodeDag.toString()
    )
    console.log('===================================================\n')

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
    console.log('\n=== noBundlingNodePathToLadderLinPath ===')
    noBundlingNodePathToLadderLinPath.forEach(
      (_, noIntermedBundlingNodePath) => {
        const nodeLabels = noIntermedBundlingNodePath.map((id) => {
          return `${id.toString()}(${(nodeInfo.context.get(id) as LadderLirNode).toPretty(nodeInfo.context)})`
        })
        console.log(nodeLabels.join(' -> '))
      }
    )
    console.log('===================================================\n')

    return new LadderNodeSelectionTracker(
      noIntermediateBundlingNodeDag,
      noBundlingNodePathToLadderLinPath,
      pathsList
    )
  }

  constructor(
    private noIntermediateBundlingNodeDag: DirectedAcyclicGraph<LirId>,
    /** Map from path in the noIntermediateBundlingNodeDag to the corresponding lin path on the ladder graph */
    private noBundlingNodePathToLadderLinPath: ArrayKeyedMap<
      PathInNoIntermediateBundlingNodeDag,
      LinPathLirNode
    >,
    private pathsList: PathsListLirNode
  ) {}

  /** Get the nodes on the ladder graph that the user has selected for highlighting by interacting with the main graph UI */
  getSelectedForHighlightPaths(context: LirContext) {
    return Array.from(this.#selectedForHighlightPaths).map(
      (id) => context.get(id) as SelectableLadderLirNode
    )
  }

  /** Reset the set of nodes selected on the main ladder graph UI for highlighting */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  resetSelectedForHighlightPaths(_context: LirContext) {
    this.#selectedForHighlightPaths = new Set()
  }

  nodeIsSelected(node: SelectableLadderLirNode) {
    return this.#selectedForHighlightPaths.has(node.getId())
  }

  private updateProjections(
    context: LirContext,
    ladderGraph: LadderGraphLirNode
  ) {
    /* 
    We are in effect maintaining two representations of the ladder graph:

    1. The main ladder graph
    2. The noIntermediateBundlingNodeDag: The ladder graph, where the only bundling nodes are the overall source and sink

    Every time there is a change in the user's node selection on the ladder graph --- which
    in effect corresponds to a change in what nodes of noIntermediateBundlingNodeDag are selected ---
    we update our projections (a and b below).
    */

    // a. check if the user has selected nodes corresponding to lin path(s) in the ladder dag,
    // and select the corresponding path(s) in the PathsList.
    const correspondingLinPaths = this.findCorrespondingLinPaths(
      this.getSelectedForHighlightPaths(context)
    )
    this.pathsList.selectPaths(context, correspondingLinPaths)
    console.log('\n=== Lin Paths ===')
    correspondingLinPaths.forEach((path, index) => {
      const nodeLabels = path.getVertices(context).map((node) => {
        return `${node.getId().toString()} (${node.toPretty(context)})`
      })
      console.log(`\nLin Path ${index + 1}:`)
      console.log(nodeLabels.join(' -> '))
    })
    console.log('=========================\n')

    // b. update ladder graph edge highlighting
    ladderGraph.highlightSubgraphEdges(
      context,
      overlays(
        correspondingLinPaths.map((linPath) => linPath.getRawPathGraph())
      )
    )
  }

  // selectNodes(
  //   context: LirContext,
  //   nodes: Array<SelectableLadderLirNode>,
  //   ladderGraph: LadderGraphLirNode
  // ) {
  //   this.#selectedForHighlightPaths = new Set(nodes.map((node) => node.getId()))
  //   // new Set([
  //   //   ...this.#selectedForHighlightPaths,
  //   //   ...nodes.map((node) => node.getId()),
  //   // ])
  // }

  /** Toggle whether a specific node is selected for highlighting (and update projections) */
  toggleNodeSelection(
    context: LirContext,
    node: SelectableLadderLirNode,
    ladderGraph: LadderGraphLirNode
  ) {
    if (this.#selectedForHighlightPaths.has(node.getId())) {
      this.#selectedForHighlightPaths.delete(node.getId())
    } else {
      this.#selectedForHighlightPaths.add(node.getId())
    }
    this.updateProjections(context, ladderGraph)
  }

  /** Given the selected nodes, figure out what lin paths through the ladder graph, if any, these correspond to */
  findCorrespondingLinPaths(selected: Array<SelectableLadderLirNode>) {
    /** 1. Compute the paths through the selected subgraph of the noBundlingNode graph
     * (These paths will start from noIntermediateBundlingNodeDag's source.)
     */
    const selectedIds = new Set(selected.map((node) => node.getId()))

    console.log(
      '=======================  computePathsThroughSelectedSubgraphOfNoBundlingNodeGraph =====================\n'
    )
    console.log(
      'selected',
      Array.from(selectedIds).map((id) => id.toString())
    )
    console.log(
      'noIntermediateBundlingNodeDag',
      this.noIntermediateBundlingNodeDag.toString()
    )

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
    pathsSelectedSubgraphOfNoBundlingNodeGraph.forEach((p, index) =>
      console.log(
        `---- pathsSelectedSubgraphOfNoBundlingNodeGraph path ${index}: `,
        p.toString()
      )
    )

    /** 2. Get the lin paths / subgraph of #dag that correspond to the (computed) paths through the selected subgraph of the noIntermediateBundlingNodeDag */
    return pathsSelectedSubgraphOfNoBundlingNodeGraph
      .map((nbnPathGraph) =>
        this.noBundlingNodePathToLadderLinPath.get(nbnPathGraph.getTopSort())
      )
      .filter((linPath) => !!linPath)
  }
}
