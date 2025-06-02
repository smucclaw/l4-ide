import {
  type LadderLirNode,
  type SelectableNode,
  LadderGraphLirNode,
  LinPathLirNode,
  isSourceLirNode,
  isSinkLirNode,
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

  /** Select the linearized paths in the PathsList and highlight the corresponding paths on the ladder graph */
  selectPaths(
    context: LirContext,
    paths: LinPathLirNode[],
    ladderGraph: LadderGraphLirNode
  ) {
    this.selected = new Set(paths.map((p) => p.getId()))
    this.highlightPathsOnLadderGraph(context, ladderGraph, paths)

    this.getRegistry().publish(context, this.getId())
  }

  /** Helper: Highlight the paths on the ladder graph that correspond to the given LinPathLirNodes */
  private highlightPathsOnLadderGraph(
    context: LirContext,
    ladderGraph: LadderGraphLirNode,
    paths: LinPathLirNode[]
  ) {
    // 1. Get the subgraph to be highlighted
    // Exploits the property that (G, +, ε) is an idempotent monoid
    const graphToHighlight = overlays(paths.map((p) => p.getRawPathGraph()))

    // 2. Reset edge styles wrt highlighting on ladder graph, then add highlight style to the subgraph
    ladderGraph.clearHighlightEdgeStyles(context)
    ladderGraph.highlightSubgraph(context, graphToHighlight)
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
          PathsTracker
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
 * This stores the PathsList (the paths through the main ladder graph), as well as what nodes the user has selected on the ladder graph.
 * It also tracks how the nodes that the user has selected on the ladder graph
 * might correspond to lin paths through the ladder graph.
 *
 * Assumes that the original dag is in NNF.
 */
export class PathsTracker {
  /** What nodes on the ladder graph, if any, the user has selected for highlighting *by interacting with the main graph UI*.
   *
   * (This is different from the selected paths in the PathsList --- those are the paths that the user has selected by interacting with the PathsList UI.)
   * The LirIds will correspond to those of SelectableLadderLirNodes.
   */
  #selectedForHighlightPaths: Set<LirId> = new Set()

  static make(
    nodeInfo: LirNodeInfo,
    /** The ladder graph dag. Assumes that this is in NNF. */
    dag: DirectedAcyclicGraph<LirId>
  ) {
    console.log('ladder graph dag: ', dag.toString())

    /* 
     * We are in effect maintaining two representations of the ladder graph:
    *
    * 1. The main ladder graph
    * 2. The noIntermediateBundlingNodeDag: The ladder graph, where the only bundling nodes are the overall source and sink (i.e., no intermediate bundling nodes)
    
    Here, we make the noIntermediateBundlingNodeDag, 
    by replacing intermediate source nodes 
    with their immediate predecessors
    and intermediate sink nodes with their immediate successors
    (We only have to consider the *immedate* precedessors / successors because of 
    the structure of the *Ladder* dag; i.e., the logic here won't work for *any* arbitrary dag.)

    The noIntermediateBundlingNodeDag still has bundling nodes --- the overall source and sink nodes.
    It's just that it doesn't have *intermediate* bundling nodes.

    This gets used to figure out what paths on the main graph the user is trying to highlight,
    when they select nodes on the main graph.

    See l4-ide/doc/dev/frontend/no-intermediate-bundling-node-dag.md
    for more on how to debug and understand this better.
    */
    const vertices = dag
      .getVertices()
      .map((v) => nodeInfo.context.get(v) as LadderLirNode)
    const sources = new Set(
      vertices.filter(isSourceLirNode).map((n) => n.getId())
    )
    const sinks = new Set(vertices.filter(isSinkLirNode).map((n) => n.getId()))

    const toSourceEdges = dag.getEdges().filter((e) => sources.has(e.getV()))
    const fromSinkEdges = dag.getEdges().filter((e) => sinks.has(e.getU()))
    const noIntermediateBundlingNodeDag = fromSinkEdges.reduceRight(
      (acc, edge) => acc.replaceVertexWithNeighbor(edge.getU(), edge.getV()),
      toSourceEdges.reduceRight(
        (acc, edge) => acc.replaceVertexWithNeighbor(edge.getV(), edge.getU()),
        dag
      )
    )

    // Make pathsList and a map from paths in the noIntermediateBundlingNodeDag to the corresponding lin paths on the main graph
    const pathsList = new PathsListLirNode(
      nodeInfo,
      dag.getAllPaths().map((p) => new LinPathLirNode(nodeInfo, p))
    )
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
    // noBundlingNodePathToLadderLinPath.forEach((_path, key) => {
    //   const nodeLabels = key.map((id) => {
    //     return `${id.toString()}(${(nodeInfo.context.get(id) as LadderLirNode).toPretty(nodeInfo.context)})`
    //   })
    //   console.log(nodeLabels.join(' → '))
    // })
    // console.log('===================================================\n')

    return new PathsTracker(
      noIntermediateBundlingNodeDag,
      pathsList,
      noBundlingNodePathToLadderLinPath
    )
  }

  constructor(
    private noIntermediateBundlingNodeDag: DirectedAcyclicGraph<LirId>,
    /** The pathsList will have to be updated if (and only if) we change the structure of the graph.
     * No need to update it, tho, if changing edge attributes. */
    private pathsList: PathsListLirNode,
    /** Map from path in the noIntermediateBundlingNodeDag to the corresponding lin path on the ladder graph */
    private noBundlingNodePathToLadderLinPath: ArrayKeyedMap<
      PathInNoIntermediateBundlingNodeDag,
      LinPathLirNode
    >
  ) {}

  /** Get the nodes on the ladder graph that the user has selected for highlighting by interacting with the main graph UI */
  getSelectedForHighlightPaths(context: LirContext) {
    return Array.from(this.#selectedForHighlightPaths).map(
      (id) => context.get(id) as SelectableNode
    )
  }

  /** Reset the set of nodes selected on the main ladder graph UI for highlighting */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  resetSelectedForHighlightPaths(_context: LirContext) {
    this.#selectedForHighlightPaths = new Set()
  }

  /** Toggle whether a specific node is selected for highlighting */
  toggleNodeSelection(
    context: LirContext,
    node: SelectableNode,
    ladderGraph: LadderGraphLirNode
  ) {
    if (this.#selectedForHighlightPaths.has(node.getId())) {
      this.#selectedForHighlightPaths.delete(node.getId())
      node.unhighlight(context)
    } else {
      this.#selectedForHighlightPaths.add(node.getId())
      node.highlight(context)
    }

    /* 
    We are in effect maintaining two representations of the ladder graph:

    1. The main ladder graph
    2. The noIntermediateBundlingNodeDag: The ladder graph, where the only bundling nodes are the overall source and sink

    Every time there is a change in the user's node selection on the ladder graph --- which
    in effect corresponds to a change in what nodes of noIntermediateBundlingNodeDag are selected ---
    we check if the user has selected nodes corresponding to lin path(s) in the ladder dag,
    and select the corresponding path(s) in the PathsList.
    */
    const linPaths = this.findCorrespondingLinPaths(
      this.getSelectedForHighlightPaths(context)
    )
    // console.log('\n=== Lin Paths ===')
    // linPaths.forEach((path, index) => {
    //   const nodeLabels = path.getVertices(context).map((node) => {
    //     return `${node.getId().toString()} (${node.toPretty(context)})`
    //   })
    //   console.log(`\nLin Path ${index + 1}:`)
    //   console.log(nodeLabels.join(' -> '))
    // })
    // console.log('=========================\n')

    // Highlight the graph union of the corresponding lin paths and the selected nodes on the ladder graph
    // (We don't just highlight the lin paths b/c also want to be able to see if we've highlighted specific nodes.)
    this.pathsList.selectPaths(context, linPaths, ladderGraph)
    ladderGraph.highlightSubgraph(
      context,
      overlays([
        ...linPaths.map((linPath) => linPath.getRawPathGraph()),
        ...this.getSelectedForHighlightPaths(context).map((node) =>
          vertex(node.getId())
        ),
      ])
    )
  }

  /** Given the selected nodes, figure out what lin paths through the ladder graph, if any, these correspond to */
  findCorrespondingLinPaths(selected: Array<SelectableNode>) {
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

  getPathsList() {
    return this.pathsList
  }
}
