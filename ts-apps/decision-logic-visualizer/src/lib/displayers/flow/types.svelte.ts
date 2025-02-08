import * as SF from '@xyflow/svelte'
import type { LirId, LirContext } from '$lib/layout-ir/core.js'
import type { ExprLirNode } from '$lib/layout-ir/lir-decision-logic.svelte.js'
import {
  BoolVarLirNode,
  AndLirNode,
  OrLirNode,
} from '$lib/layout-ir/lir-decision-logic.svelte.js'
import { match, P } from 'ts-pattern'
import type { DirectedAcyclicGraph } from '../../algebraic-graphs/dag.js'
import type { Ord } from '$lib/utils.js'
import { ComparisonResult } from '$lib/utils.js'
/* IMPT: Cannot currently use $lib for the following import,
because of how the functions were defined */
import {
  connect,
  // empty,
  vertex,
  overlay,
} from '../../algebraic-graphs/dag.js'
// import { over } from 'lodash'

const DEFAULT_INITIAL_POSITION = { x: 0, y: 0 }

export interface ExprFlowDisplayerProps {
  context: LirContext
  node: ExprLirNode
}

export interface FlowGraph {
  nodes: FlowNode[]
  edges: FlowEdge[]
}

/** A simplified version of the SvelteFlow Node interface,
 * for the v0 prototype.
 * We can distinguish between different kinds of FlowNodes in the future. */
export class FlowNode implements Ord<FlowNode> {
  private static counter = 0
  protected id: string
  // TODO: Let's only add more variants of data, e.g. a value, in the next prototype
  private data: {
    label: string
  }

  constructor(
    protected readonly label: string,
    /** The LirIds of the LirNodes that correspond in some sense to this FlowNode */
    protected readonly origLirIds: LirId[],
    protected readonly position: {
      x: number
      y: number
    } = DEFAULT_INITIAL_POSITION
  ) {
    this.data = { label }
    this.id = (FlowNode.counter++).toString()
  }

  getData() {
    return this.data
  }

  getId() {
    return this.id
  }

  /** Required for SF nodes */
  getPosition() {
    return this.position
  }

  getOrigLirIds() {
    return this.origLirIds
  }

  toSFPojo(): SF.Node {
    return {
      id: this.id,
      position: this.position,
      data: this.data,
    }
  }

  isEqualTo<T extends FlowNode>(other: T) {
    return this.id === other.id
  }

  /** Lexicographical comparison */
  compare(that: this) {
    const intId = parseInt(this.getId())
    const thatId = parseInt(that.getId())
    if (intId === thatId) {
      return ComparisonResult.Equal
    } else if (intId < thatId) {
      return ComparisonResult.LessThan
    } else {
      return ComparisonResult.GreaterThan
    }
  }
}

// TODO: Add a new FlowNode type only if it looks like the transformations do work

export class FlowEdge implements Ord<FlowEdge> {
  id: string
  constructor(
    readonly source: string,
    readonly target: string
  ) {
    this.id = `(${source}, ${target})`
  }

  getU() {
    return this.source
  }

  getV() {
    return this.target
  }

  isEqualTo<T extends FlowEdge>(other: T) {
    return this.id === other.id
  }

  compare(that: FlowEdge): ComparisonResult {
    if (this.getU() < that.getU()) {
      return ComparisonResult.LessThan
    } else if (this.getU() > that.getU()) {
      return ComparisonResult.GreaterThan
    }

    if (this.getV() < that.getV()) {
      return ComparisonResult.LessThan
    } else if (this.getV() > that.getV()) {
      return ComparisonResult.GreaterThan
    }

    return ComparisonResult.Equal
  }

  toSFPojo(): SF.Edge {
    return {
      id: this.id,
      source: this.source,
      target: this.target,
    }
  }
}

// TODO1: Make a new kind of FlowNode for source/sink nodes
// TODO2: Attach a group id to the label for the flownode to make it easier to debug
// TODO3: Return the number of groups as metadata in the FlowGraph

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
  /*
  One of the insights behind the ladder diagram, in particular, behind Meng's `layman`:
    Reduce visual clutter, by creating dummy source and sink nodes between which to nest the subgraphs, and only connecting those.
    That is, we don't connect nodes in different OR/AND groups --- we only connect the source and sink nodes of each group.

  In more detail, the algorithm implemented below is:

  For AND:
  * For each of the child subgraphs, connect the sink of the previous child to the source of the next child.
  * Sandwich the children between a source and sink vertex:
    Connect the source vertex to the source of first child
    and the sink of the last child to the final overall sink vertex.

  For OR:
  * Sandwich the children between a source and sink vertex.
  * For each of the child subgraphs, connect the source of the child to the source of the overall graph; and likewise with the sinks.
  */
  return match(expr)
    .with(P.instanceOf(BoolVarLirNode), (node) => {
      const flowNode = new FlowNode(node.getName(context), [node.getId()])
      return vertex(flowNode)
    })
    .with(P.instanceOf(AndLirNode), (node) => {
      const overallSource = vertex(
        new FlowNode('AND_SOURCE', [node.getId()])
      ) as DirectedAcyclicGraph<FlowNode>
      const overallSink = vertex(
        new FlowNode('AND_SINK', [node.getId()])
      ) as DirectedAcyclicGraph<FlowNode>

      const childGraphs = node
        .getArgs(context)
        .map((n) => exprLirNodeToAlgaDag(context, n))

      return [overallSource, ...childGraphs, overallSink].reduceRight(
        (acc, left) => {
          const accSource = acc.getSource()
          const leftSink = left.getSink()
          return leftSink.connect(accSource).overlay(left).overlay(acc)
        }
      )
    })
    .with(P.instanceOf(OrLirNode), (node) => {
      const children = node
        .getArgs(context)
        .map((n) => exprLirNodeToAlgaDag(context, n))

      const overallSource = vertex(
        new FlowNode('OR_SOURCE', [node.getId()])
      ) as DirectedAcyclicGraph<FlowNode>
      const overallSink = vertex(
        new FlowNode('OR_SINK', [node.getId()])
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
