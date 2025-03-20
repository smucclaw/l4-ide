import type { LirContext, LirId } from '$lib/layout-ir/core.js'
import type {
  LadderLirNode,
  LadderLirEdge,
  SourceNoAnnoLirNode,
  SourceWithOrAnnoLirNode,
} from '$lib/layout-ir/ladder-lir.svelte.js'
import {
  isSourceNoAnnoLirNode,
  isSourceWithOrAnnoLirNode,
  LadderGraphLirNode,
} from '$lib/layout-ir/ladder-lir.svelte.js'
/* IMPT: Cannot currently use $lib for the following import,
because of how the functions were defined */
import {
  isBoolVarLirNode,
  BoolVarLirNode,
  NotStartLirNode,
  NotEndLirNode,
  SinkLirNode,
} from '$lib/layout-ir/ladder-lir.svelte.js'
import {
  type LadderSFGraph,
  type LadderSFNode,
  boolVarNodeType,
  notStartNodeType,
  notEndNodeType,
  sourceNoAnnoNodeType,
  sourceWithOrAnnoNodeType,
  sinkNodeType,
  ladderEdgeType,
} from './svelteflow-types.js'
import * as SF from '@xyflow/svelte'
import { match, P } from 'ts-pattern'
import _ from 'lodash'

export function ladderGraphToSFGraph(
  context: LirContext,
  ladderGraph: LadderGraphLirNode
): LadderSFGraph {
  const ladderNodes = (
    ladderGraph.getVertices(context) as LadderLirNode[]
  ).toSorted((v1, v2) => v2.compare(v1))
  const nodes = ladderNodes.map(ladderLirNodeToSfNode.bind(null, context))
  const idsAssocList = _.zip(nodes, ladderNodes)
    .filter(
      (pair): pair is [LadderSFNode, LadderLirNode] => !!pair[0] && !!pair[1]
    )
    .map(([sfNode, ladderNode]): [string, LirId] => [
      sfNode.id,
      ladderNode.getId(),
    ])
  // TODO: Refactor to use an array instead of a map
  const sfIdToLirIdMap = new Map(idsAssocList)
  const sfIdToLirId = (sfId: string) => {
    const lirId = sfIdToLirIdMap.get(sfId)
    if (!lirId) {
      throw new Error(`Internal Error: No LIR ID found for SF ID: ${sfId}`)
    }
    return lirId
  }

  // TODO: May want to sort as well
  const edges = ladderGraph
    .getEdges(context)
    .map(ladderLirEdgeToSfEdge.bind(null, context, ladderGraph))

  return {
    nodes,
    edges,
    sfIdToLirId,
    lirIdToSFId,
  }
}

/******************************************************
 ************* LadderLirNode to SF.Node ***************
 ******************************************************/

function lirIdToSFId(id: LirId): string {
  return id.toString()
}

/**
 * Converts a LadderLirNode into an SF.Node object.
 */
export function ladderLirNodeToSfNode(
  context: LirContext,
  node: LadderLirNode
): LadderSFNode {
  const defaults = {
    id: lirIdToSFId(node.getId()),
    position: node.getPosition(context),
    initialWidth: node.getDimensions(context)?.width,
    initialHeight: node.getDimensions(context)?.height,
  }

  const defaultData = {
    context,
    originalLirId: node.getId(),
  }

  return match(node)
    .with(P.when(isBoolVarLirNode), (n: BoolVarLirNode) => {
      return {
        ...defaults,
        type: boolVarNodeType,
        data: { ...defaultData, ...n.getData(context) },
      }
    })
    .with(P.instanceOf(NotStartLirNode), () => {
      return {
        ...defaults,
        type: notStartNodeType,
        data: defaultData,
      }
    })
    .with(P.instanceOf(NotEndLirNode), () => {
      return {
        ...defaults,
        type: notEndNodeType,
        data: defaultData,
      }
    })
    .with(P.when(isSourceNoAnnoLirNode), (n: SourceNoAnnoLirNode) => {
      return {
        ...defaults,
        type: sourceNoAnnoNodeType,
        data: { ...defaultData, ...n.getData() },
      }
    })
    .with(P.when(isSourceWithOrAnnoLirNode), (n: SourceWithOrAnnoLirNode) => {
      return {
        ...defaults,
        type: sourceWithOrAnnoNodeType,
        data: { ...defaultData, ...n.getData() },
      }
    })
    .with(P.instanceOf(SinkLirNode), (n: SinkLirNode) => {
      return {
        ...defaults,
        type: sinkNodeType,
        data: { ...defaultData, ...n.getData() },
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

  return {
    id: edge.getId(),
    type: ladderEdgeType,
    data: {
      context,
      label,
      edgeStyles: graph.getEdgeStyles(context, edge).getStyleString(),
    },
    source: edge.getU().toString(),
    target: edge.getV().toString(),
  }
}
