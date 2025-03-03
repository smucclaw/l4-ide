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
  type SfGraph,
  boolVarNodeType,
  notStartNodeType,
  notEndNodeType,
  sourceNodeType,
  sinkNodeType,
  ladderEdgeType,
} from './types.svelte.js'
import * as SF from '@xyflow/svelte'
import { match, P } from 'ts-pattern'

export function ladderGraphToSFGraph(
  context: LirContext,
  ladderGraph: LadderGraphLirNode
): SfGraph {
  const nodes = (ladderGraph.getVertices(context) as LadderLirNode[])
    .toSorted((v1, v2) => v2.compare(v1))
    .map(ladderLirNodeToSfNode.bind(null, context))

  // TODO: May want to sort as well
  const edges = ladderGraph
    .getEdges(context)
    .map(ladderLirEdgeToSfEdge.bind(null, context, ladderGraph))

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
  const defaults = {
    id: lirIdToSFId(node.getId()),
    originalLirId: node.getId(),
    position: node.getPosition(context),
    initialWidth: node.getDimensions(context)?.width,
    initialHeight: node.getDimensions(context)?.height,
  }

  return match(node)
    .with(P.instanceOf(BoolVarLirNode), (n) => {
      return {
        ...defaults,
        type: boolVarNodeType,
        data: n.getData(context),
      }
    })
    .with(P.instanceOf(NotStartLirNode), () => {
      return {
        ...defaults,
        type: notStartNodeType,
        data: {},
      }
    })
    .with(P.instanceOf(NotEndLirNode), () => {
      return {
        ...defaults,
        type: notEndNodeType,
        data: {},
      }
    })
    .with(P.instanceOf(SourceLirNode), () => {
      return {
        ...defaults,
        type: sourceNodeType,
        data: {},
      }
    })
    .with(P.instanceOf(SinkLirNode), () => {
      return {
        ...defaults,
        type: sinkNodeType,
        data: {},
      }
    })
    .exhaustive()
}

/******************************************************
 ************* Ladder Lir Edge to SF.Edge **************
 ******************************************************/

export function ladderLirEdgeToSfEdge(
  context: LirContext,
  graph: LadderGraphLirNode,
  edge: LadderLirEdge
): SF.Edge {
  const label = graph.getEdgeLabel(context, edge)
  const strokeColorCSSVar = graph.getEdgeStyles(context, edge).getStrokeColor()

  return {
    id: edge.getId(),
    type: ladderEdgeType,
    data: {
      label,
      strokeColorCSSVar,
    },
    source: edge.getU().toString(),
    target: edge.getV().toString(),
  }
}
