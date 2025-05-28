import {
  LadderGraphLirNode,
  LinPathLirNode,
} from './ladder-graph/ladder.svelte.js'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import { overlay, empty } from '../algebraic-graphs/dag.js'

export function isPathsListLirNode(
  node: PathsListLirNode
): node is PathsListLirNode {
  return node instanceof PathsListLirNode
}

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
      .map((p) => p._getRawPath())
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
