import {
  type LadderLirNode,
  type SelectableLadderLirNode,
  LadderGraphLirNode,
  LinPathLirNode,
  isSourceLirNode,
  isSinkLirNode,
  isSelectableLadderLirNode,
} from './ladder-graph/ladder.svelte.js'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import {
  overlay,
  empty,
  type DirectedAcyclicGraph,
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

/** Assumes that the original dag is in NNF */
export class PathsListLirNode extends DefaultLirNode implements LirNode {
  private paths: Array<LirId>

  constructor(nodeInfo: LirNodeInfo, paths: Array<LinPathLirNode>) {
    super(nodeInfo)
    this.paths = paths.map((n) => n.getId())
  }

  getPaths(context: LirContext) {
    return this.paths.map((id) => context.get(id)) as Array<LinPathLirNode>
  }

  highlightPaths(
    context: LirContext,
    ladderGraph: LadderGraphLirNode,
    paths: LinPathLirNode[]
  ) {
    // 1. Get the subgraph to be highlighted
    // Exploits the property that (G, +, ε) is an idempotent monoid
    const graphToHighlight = paths
      .map((p) => p.getRawPathGraph())
      .reduceRight(overlay, empty())

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

/**
 * We are in effect maintaining two representations of the ladder graph:
 *
 * 1. The main ladder graph
 * 2. The noIntermediateBundlingNodeDag: The ladder graph, where the only bundling nodes are the overall source and sink (i.e., no intermediate bundling nodes)
 *
 * This stores the paths through the main ladder graph,
 * and tracks how the nodes that the user has selected on the ladder graph
 * might correspond to lin paths through the ladder graph.
 *
 * Assumes that the original dag is in NNF.
 */
export class PathsTracker {
  static make(
    nodeInfo: LirNodeInfo,
    /** The ladder graph dag. Assumes that this is in NNF. */
    dag: DirectedAcyclicGraph<LirId>
  ) {
    console.log('ladder graph dag: ', dag.toString())

    /* Make the noIntermediateBundlingNodeDag, 
    by replacing intermediate source nodes 
    with their immediate predecessors
    and intermediate sink nodes with their immediate successors
    (We only have to consider the *immedate* precedessors / successors because of the structure of the *Ladder* dag; i.e., the logic here won't work for *any* arbitrary dag.)

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
        .map((path): [LirId[], LinPathLirNode] => {
          const topSort = path
            .getRawPathGraph()
            .getTopSort()
            .filter((v) => {
              const isOverallSource = dag.getSource().isEqualTo(vertex(v))
              const isOverallSink = dag.getSink().isEqualTo(vertex(v))
              return (
                isOverallSource ||
                isOverallSink ||
                isSelectableLadderLirNode(
                  nodeInfo.context.get(v) as LadderLirNode
                )
              )
            })
          return [topSort, path]
        })
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
