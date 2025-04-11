/* eslint-disable @typescript-eslint/no-unused-vars */
import {
  LadderGraphLirNode,
  LinPathLirNode,
} from './ladder-graph/ladder.svelte.js'
import type { LirId, LirNode, LirNodeInfo } from './core.js'
import { LirContext, DefaultLirNode } from './core.js'
import { overlay, empty } from '../algebraic-graphs/dag.js'

export type PathsListLirNode = InvalidPathsListLirNode | ValidPathsListLirNode

export function isInvalidPathsListLirNode(
  node: PathsListLirNode
): node is InvalidPathsListLirNode {
  return node instanceof InvalidPathsListLirNode
}

export class InvalidPathsListLirNode extends DefaultLirNode implements LirNode {
  constructor(nodeInfo: LirNodeInfo) {
    super(nodeInfo)
  }

  toString(): string {
    return 'INVALID_PATHS_LIST_LIR_NODE'
  }

  getChildren(_context: LirContext) {
    return []
  }

  dispose(context: LirContext) {
    context.clear(this.getId())
  }
}

export function isValidPathsListLirNode(
  node: PathsListLirNode
): node is ValidPathsListLirNode {
  return node instanceof ValidPathsListLirNode
}

export class ValidPathsListLirNode extends DefaultLirNode implements LirNode {
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

    // 2. Reset edge styles wrt highlighting on ladder graph, then add highlight style to the subgraph
    this.ladderGraph.clearHighlightEdgeStyles(context)
    this.ladderGraph.highlightSubgraph(context, graphToHighlight)
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
    return 'VALID_PATHS_LIST_LIR_NODE'
  }
}
