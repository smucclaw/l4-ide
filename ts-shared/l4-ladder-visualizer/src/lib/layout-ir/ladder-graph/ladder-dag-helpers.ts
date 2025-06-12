import type { LirId } from '@repo/layout-ir'
import { LirContext } from '@repo/layout-ir'
import {
  isVertex,
  type DirectedAcyclicGraph,
} from '../../algebraic-graphs/dag.js'
import type { LadderLirNode, NotStartLirNode } from './ladder.svelte.js'
import { isNotStartLirNode, isUBoolVarLirNode } from './ladder.svelte.js'

/************************************************
        Vertices from alga dag
*************************************************/

/** Helper function */
export function getVerticesFromAlgaDag(
  context: LirContext,
  dag: DirectedAcyclicGraph<LirId>
): LadderLirNode[] {
  return Array.from(dag.getVertices()).map(
    (id) => context.get(id) as LadderLirNode
  )
}

/************************************************
          isNnf
*************************************************/

export function isNnf(
  context: LirContext,
  dag: DirectedAcyclicGraph<LirId>
): boolean {
  const negandIsSimpleVar = (notStart: NotStartLirNode) => {
    // TODO: Will have to update this when we add more complicated Lir Nodes
    const negand = notStart.getNegand(context)
    return (
      isVertex(negand) &&
      isUBoolVarLirNode(context.get(negand.getValue()) as LadderLirNode)
    )
  }

  const notStartVertices = getVerticesFromAlgaDag(context, dag).filter(
    isNotStartLirNode
  )
  return notStartVertices.every(negandIsSimpleVar)
}
