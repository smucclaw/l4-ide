import type { LirContext, LirId } from '$lib/layout-ir/core.js'
import type {
  LadderLirNode,
  LadderLirEdge,
  SourceNoAnnoLirNode,
  SourceWithOrAnnoLirNode,
  LadderGraphLirNode,
} from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
import {
  isTrueExprLirNode,
  isFalseExprLirNode,
  isUBoolVarLirNode,
  isNotStartLirNode,
  isSinkLirNode,
  isSourceNoAnnoLirNode,
  isSourceWithOrAnnoLirNode,
  NotEndLirNode,
  SinkLirNode,
  isAppLirNode,
} from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
import {
  type LadderSFGraph,
  type LadderSFNode,
  trueExprNodeType,
  falseExprNodeType,
  uBoolVarNodeType,
  notStartNodeType,
  notEndNodeType,
  sourceNoAnnoNodeType,
  sourceWithOrAnnoNodeType,
  sinkNodeType,
  ladderEdgeType,
  appNodeType,
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
    node,
  }

  return match(node)
    .with(P.when(isTrueExprLirNode), () => {
      return {
        ...defaults,
        type: trueExprNodeType,
        data: defaultData,
      }
    })
    .with(P.when(isFalseExprLirNode), () => {
      return {
        ...defaults,
        type: falseExprNodeType,
        data: defaultData,
      }
    })
    .with(P.when(isUBoolVarLirNode), () => {
      return {
        ...defaults,
        type: uBoolVarNodeType,
        data: defaultData,
      }
    })
    .with(P.when(isAppLirNode), () => {
      return {
        ...defaults,
        type: appNodeType,
        data: defaultData,
      }
    })
    .with(P.when(isNotStartLirNode), () => {
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
    }) // TODO: Continue refactoring the following
    .with(P.when(isSourceNoAnnoLirNode), (n: SourceNoAnnoLirNode) => {
      return {
        ...defaults,
        type: sourceNoAnnoNodeType,
        data: { ...defaultData, ...n.getData(context) },
      }
    })
    .with(P.when(isSourceWithOrAnnoLirNode), (n: SourceWithOrAnnoLirNode) => {
      return {
        ...defaults,
        type: sourceWithOrAnnoNodeType,
        data: { ...defaultData, ...n.getData(context) },
      }
    })
    .with(P.when(isSinkLirNode), (n: SinkLirNode) => {
      return {
        ...defaults,
        type: sinkNodeType,
        data: { ...defaultData, ...n.getData(context) },
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
  const styles = graph.getEdgeStyles(context, edge)

  return {
    id: edge.getId(),
    type: ladderEdgeType,
    data: {
      context,
      label,
      edgeStyles: styles.getCombinedEdgeStyleString(),
      labelStyles: styles.getLabelStyleString(),
    },
    source: edge.getU().toString(),
    target: edge.getV().toString(),
  }
}
