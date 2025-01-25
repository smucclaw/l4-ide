import * as SF from '@xyflow/svelte'
import type {
  LirId,
  LirContext,
  ExprLirNode,
  BinOp,
} from '$lib/layout-ir/index.ts'
import { BoolVarLirNode, BinExprLirNode } from '$lib/layout-ir/index.ts'
import { match, P } from 'ts-pattern'
import type { UndirectedGraph } from '$lib/algebraic-graphs/alga.ts'
import type { Ord } from '$lib/utils'
import { ComparisonResult } from '$lib/utils'
import {
  connect,
  // empty,
  vertex,
  overlay,
} from '$lib/algebraic-graphs/alga.ts'

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
  // Let's only add more variants of data, e.g. a value, in the next prototype
  private data: {
    label: string
  }

  constructor(
    protected readonly label: string,
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

export function exprLirNodeToAlgaUndirectedGraph(
  context: LirContext,
  expr: ExprLirNode
): UndirectedGraph<FlowNode> {
  // The following exploits the analogy between And with connect
  // and Or with overlay
  return match(expr)
    .with(P.instanceOf(BoolVarLirNode), (node) => {
      const flowNode = new FlowNode(node.getName(), [node.getId()])
      return vertex(flowNode)
    })
    .with(P.instanceOf(BinExprLirNode), (node) => {
      const leftGraph = exprLirNodeToAlgaUndirectedGraph(
        context,
        node.getLeft(context)
      )
      const rightGraph = exprLirNodeToAlgaUndirectedGraph(
        context,
        node.getRight(context)
      )
      return match<BinOp>(node.getOp())
        .with('And', () => connect(leftGraph, rightGraph))
        .with('Or', () => overlay(leftGraph, rightGraph))
        .exhaustive()
    })
    .exhaustive()
}

export function algaUndirectedGraphToFlowGraph(
  graph: UndirectedGraph<FlowNode>
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
