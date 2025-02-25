import type { LirContext, LirId } from '$lib/layout-ir/core.js'
import type {
  LadderLirNode,
  LadderLirEdge,
} from '$lib/layout-ir/ladder-lir.svelte.js'
import { LadderGraphLirNode } from '$lib/layout-ir/ladder-lir.svelte.js'
/* IMPT: Cannot currently use $lib for the following import,
because of how the functions were defined */
import {
  BoolVarLirNode,
  NotStartLirNode,
  NotEndLirNode,
  SourceLirNode,
  SinkLirNode,
} from '$lib/layout-ir/ladder-lir.svelte.js'
import {
  boolVarNodeType,
  notStartNodeType,
  notEndNodeType,
  sourceNodeType,
  sinkNodeType,
} from './types.svelte.js'
import * as SF from '@xyflow/svelte'
import { match, P } from 'ts-pattern'

export function ladderGraphToSFGraph(
  context: LirContext,
  ladderGraph: LadderGraphLirNode
) {
  const nodes = (ladderGraph.getVertices(context) as LadderLirNode[])
    .toSorted((v1, v2) => v2.compare(v1))
    .map(ladderLirNodeToSfNode.bind(null, context))

  // TODO: May want to sort as well
  const edges = ladderGraph
    .getEdges(context)
    .map(ladderLirEdgeToSfEdge.bind(null, context))

  return {
    nodes,
    edges,
  }
}

/******************************************************
 ************* LadderLirNode to SF.Node ***************
 ******************************************************/

export function lirIdToSFId(id: LirId): string {
  return id.toString()
}

/**
 * Converts a LadderLirNode into an SF.Node object.
 */
export function ladderLirNodeToSfNode(
  context: LirContext,
  node: LadderLirNode
): SF.Node {
  return match(node)
    .with(P.instanceOf(BoolVarLirNode), (n) => {
      return {
        id: lirIdToSFId(n.getId()),
        type: boolVarNodeType,
        position: n.getPosition(context),
        data: n.getData(context),
      } as SF.Node
    })
    .with(P.instanceOf(NotStartLirNode), (n: NotStartLirNode) => {
      return {
        id: lirIdToSFId(n.getId()),
        type: notStartNodeType,
        position: n.getPosition(context),
        data: {},
      } as SF.Node
    })
    .with(P.instanceOf(NotEndLirNode), (n: NotEndLirNode) => {
      return {
        id: lirIdToSFId(n.getId()),
        type: notEndNodeType,
        position: n.getPosition(context),
        data: {},
      } as SF.Node
    })
    .with(P.instanceOf(SourceLirNode), (n: SourceLirNode) => {
      return {
        id: lirIdToSFId(n.getId()),
        type: sourceNodeType,
        position: n.getPosition(context),
        data: {},
      } as SF.Node
    })
    .with(P.instanceOf(SinkLirNode), (n: SinkLirNode) => {
      return {
        id: lirIdToSFId(n.getId()),
        type: sinkNodeType,
        position: n.getPosition(context),
        data: {},
      } as SF.Node
    })
    .exhaustive()
}

/******************************************************
 ************* Ladder Lir Edge to SF.Edge **************
 ******************************************************/

export function ladderLirEdgeToSfEdge(
  _context: LirContext,
  edge: LadderLirEdge
): SF.Edge {
  return {
    id: edge.getId(),
    source: edge.getU().toString(),
    target: edge.getV().toString(),
  }
}
