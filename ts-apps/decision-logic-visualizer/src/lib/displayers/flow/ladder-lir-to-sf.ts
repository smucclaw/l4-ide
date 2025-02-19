import type { LirId, LirContext } from '$lib/layout-ir/core.js'
import type {
  LadderLirNode,
  LadderLirEdge,
} from '$lib/layout-ir/lir-decision-logic.svelte.js'
import { DefaultLadderLirEdge } from '$lib/layout-ir/lir-decision-logic.svelte.js'
import { getDagVertices } from '$lib/layout-ir/lir-decision-logic.svelte.js'
import type { DirectedAcyclicGraph } from '../../algebraic-graphs/dag.js'
/* IMPT: Cannot currently use $lib for the following import,
because of how the functions were defined */
import {
  BoolVarLirNode,
  NotStartLirNode,
  NotEndLirNode,
  SourceLirNode,
  SinkLirNode,
} from '$lib/layout-ir/lir-decision-logic.svelte.js'
import {
  boolVarNodeType,
  notStartNodeType,
  notEndNodeType,
  sourceNodeType,
  sinkNodeType,
} from './types.svelte.js'
import * as SF from '@xyflow/svelte'
import { match, P } from 'ts-pattern'

export function dagToSFGraph(
  context: LirContext,
  dag: DirectedAcyclicGraph<LirId>
) {
  const nodes = getDagVertices(context, dag)
    .toSorted((v1, v2) => v2.compare(v1))
    .map(ladderLirNodeToSfNode.bind(null, context))

  // TODO: May want to sort as well
  const edges = dag
    .getEdges()
    .map(
      (edge) =>
        new DefaultLadderLirEdge(edge.getU().toString(), edge.getV().toString())
    )
    .map(ladderLirEdgeToSfEdge.bind(null, context))

  return {
    nodes,
    edges,
  }
}

/******************************************************
 ************* LadderLirNode to SF.Node ***************
 ******************************************************/

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
        id: n.getId().toString(),
        type: boolVarNodeType,
        position: n.getPosition(context),
        data: n.getData(context),
      } as SF.Node
    })
    .with(P.instanceOf(NotStartLirNode), (n: NotStartLirNode) => {
      return {
        id: n.getId().toString(),
        type: notStartNodeType,
        position: n.getPosition(context),
        data: {},
      } as SF.Node
    })
    .with(P.instanceOf(NotEndLirNode), (n: NotEndLirNode) => {
      return {
        id: n.getId().toString(),
        type: notEndNodeType,
        position: n.getPosition(context),
        data: {},
      } as SF.Node
    })
    .with(P.instanceOf(SourceLirNode), (n: SourceLirNode) => {
      return {
        id: n.getId().toString(),
        type: sourceNodeType,
        position: n.getPosition(context),
        data: {},
      } as SF.Node
    })
    .with(P.instanceOf(SinkLirNode), (n: SinkLirNode) => {
      return {
        id: n.getId().toString(),
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
  context: LirContext,
  edge: LadderLirEdge
): SF.Edge {
  return {
    id: edge.getId(),
    source: edge.getU(),
    target: edge.getV(),
  }
}
