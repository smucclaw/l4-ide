import type { LirId } from '../core.js'
import { LirContext } from '../core.js'
import {
  isEmpty,
  isVertex,
  isOverlay,
  isConnect,
  type Overlay,
  type Connect,
  type Vertex,
  type DirectedAcyclicGraph,
} from '../../algebraic-graphs/dag.js'
import { DirectedEdge } from '../../algebraic-graphs/edge.js'
import type { LadderLirNode, NotStartLirNode } from './ladder.svelte.js'
import { isNotStartLirNode, isUBoolVarLirNode } from './ladder.svelte.js'
import { match, P } from 'ts-pattern'

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

/************************************************
          Pretty print path graph
*************************************************/

export function pprintPathGraph(
  context: LirContext,
  initialGraph: DirectedAcyclicGraph<LirId>
): string {
  /** Each node should only be pprinted once in the linearization of the dag */
  const processed = new Set<LirId>()

  function pprintHelper(
    context: LirContext,
    g: DirectedAcyclicGraph<LirId>
  ): string {
    return match(g)
      .with(P.when(isEmpty<LirId>), () => '')
      .with(P.when(isVertex<LirId>), (v: Vertex<LirId>) => {
        if (processed.has(v.getValue())) return ''

        processed.add(v.getValue())
        return (context.get(v.getValue()) as LadderLirNode).toPretty(context)
      })
      .with(P.when(isOverlay<LirId>), (o: Overlay<LirId>) => {
        return `${pprintHelper(context, o.getLeft())} ${pprintHelper(context, o.getRight())}`
      })
      .with(P.when(isConnect<LirId>), (c: Connect<LirId>) => {
        const from = pprintHelper(context, c.getFrom())
        const to = pprintHelper(context, c.getTo())

        if (isVertex(c.getFrom()) && isVertex(c.getTo())) {
          const edgeAttrs = initialGraph.getAttributesForEdge(
            new DirectedEdge(
              (c.getFrom() as Vertex<LirId>).getValue(),
              (c.getTo() as Vertex<LirId>).getValue()
            )
          )
          const edgeLabel = edgeAttrs.getLabel()
          return `${from} ${edgeLabel} ${to}`
        } else {
          return `${from} ${to}`
        }
      })
      .exhaustive()
  }

  return pprintHelper(context, initialGraph)
}
