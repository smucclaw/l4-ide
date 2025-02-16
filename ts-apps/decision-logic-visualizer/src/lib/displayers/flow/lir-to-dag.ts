import type { FlowGraph, FlowNode } from './types.svelte.js'
import {
  BoolVarFlowNode,
  SourceFlowNode,
  SinkFlowNode,
  NotStartFlowNode,
  NotEndFlowNode,
  FlowEdge,
} from './types.svelte.js'
import type { LirContext } from '$lib/layout-ir/core.js'
import type { ExprLirNode } from '$lib/layout-ir/lir-decision-logic.svelte.js'
import {
  BoolVarLirNode,
  NotLirNode,
  AndLirNode,
  OrLirNode,
} from '$lib/layout-ir/lir-decision-logic.svelte.js'
import { match, P } from 'ts-pattern'
import type { DirectedAcyclicGraph } from '../../algebraic-graphs/dag.js'
/* IMPT: Cannot currently use $lib for the following import,
because of how the functions were defined */
import { vertex, overlay } from '../../algebraic-graphs/dag.js'

/**
* Preconditions / assumptions:
* - To reduce visual clutter in the generated graph, the input ANDs/ORs should
    have already been 'flattened'; i.e.,
    nested exprs like (AND [a (AND [b])]) should have already been flattened to (AND [a b]).
*/
export function exprLirNodeToAlgaDag(
  context: LirContext,
  expr: ExprLirNode
): DirectedAcyclicGraph<FlowNode> {
  const overallSource = vertex(
    new SourceFlowNode([expr.getId()])
  ) as DirectedAcyclicGraph<FlowNode>
  const overallSink = vertex(
    new SinkFlowNode([expr.getId()])
  ) as DirectedAcyclicGraph<FlowNode>

  const middle = transform(context, expr)
  return overallSource
    .connect(middle.getSource())
    .overlay(middle)
    .overlay(middle.getSink().connect(overallSink))
}

// TODO2: Attach a group id to the label for the flownode to make it easier to debug
// TODO3: Return the number of groups as metadata in the FlowGraph

/** Internal helper */
function transform(
  context: LirContext,
  expr: ExprLirNode
): DirectedAcyclicGraph<FlowNode> {
  /*
  One of the insights behind the ladder diagram, in particular, behind Meng's `layman`:
    Reduce visual clutter, by creating dummy source and sink nodes between which to nest the subgraphs, and only connecting those.
    That is, we don't connect nodes in different OR/AND groups --- we only connect the source and sink nodes of each group.

  In more detail, the algorithm implemented below is:

  For AND:
  * For each of the child subgraphs, connect the sink of the previous child to the source of the next child.

  For OR:
  * Sandwich the children between a source and sink vertex.
  * For each of the child subgraphs, connect the source of the child to the source of the overall graph; and likewise with the sinks.
  *
  * In "Scalable Drawing of Nested Directed Acyclic Graphs With Gates and Ports",
  * Thomas Leu calls this kind of sandwiching 'bundling'. Leu's layouter and visualizer
  * can do this kind of bundling automatically; it's a pity
  * we need to do it more manually.
  *
  * TODO: I haven't looked at how Leu does the bundling yet --- there might be useful info there too.
  */
  return match(expr)
    .with(P.instanceOf(BoolVarLirNode), (node) => {
      const flowNode = new BoolVarFlowNode(
        { label: node.getName(context) },
        node.getId()
      )
      return vertex(flowNode)
    })
    .with(P.instanceOf(NotLirNode), (node) => {
      const negand = transform(context, node.getNegand(context))
      const notStart = vertex(
        new NotStartFlowNode(node.getId())
      ) as DirectedAcyclicGraph<FlowNode>
      const notEnd = vertex(
        new NotEndFlowNode(node.getId())
      ) as DirectedAcyclicGraph<FlowNode>
      return notStart
        .connect(negand.getSource())
        .overlay(negand)
        .overlay(negand.getSink().connect(notEnd))
    })
    .with(P.instanceOf(AndLirNode), (node) => {
      const childGraphs = node
        .getArgs(context)
        .map((n) => transform(context, n))

      return childGraphs.reduceRight((acc, left) => {
        const accSource = acc.getSource()
        const leftSink = left.getSink()
        return leftSink.connect(accSource).overlay(left).overlay(acc)
      })
    })
    .with(P.instanceOf(OrLirNode), (node) => {
      const children = node.getArgs(context).map((n) => transform(context, n))

      const overallSource = vertex(
        new SourceFlowNode([node.getId()])
      ) as DirectedAcyclicGraph<FlowNode>
      const overallSink = vertex(
        new SinkFlowNode([node.getId()])
      ) as DirectedAcyclicGraph<FlowNode>

      const leftEdges = children.map((child) =>
        overallSource.connect(child.getSource())
      )
      const rightEdges = children.map((child) =>
        child.getSink().connect(overallSink)
      )

      return [overallSource, ...leftEdges, ...children, ...rightEdges].reduce(
        overlay
      )
    })
    .exhaustive()
}

export function algaUndirectedGraphToFlowGraph(
  graph: DirectedAcyclicGraph<FlowNode>
): FlowGraph {
  const nodes: FlowNode[] = graph.getVertices()
  const edges: FlowEdge[] = graph
    .getEdges()
    .map((edge) => new FlowEdge(edge.getU().getId(), edge.getV().getId()))
  return {
    nodes,
    edges,
  }
}
