import type { Name, BoolValue } from '@repo/viz-expr'
import type { LirId, LirContext } from '$lib/layout-ir/core.js'
import type {
  DeclLirNode,
  LadderLirNode,
} from '$lib/layout-ir/lir-decision-logic.svelte.js'
import {
  BoolVarLirNode,
  NotStartLirNode,
  NotEndLirNode,
  SourceLirNode,
  SinkLirNode,
} from '$lib/layout-ir/lir-decision-logic.svelte.js'
import type { Ord } from '$lib/utils.js'
import { ComparisonResult } from '$lib/utils.js'
import * as SF from '@xyflow/svelte'
import BoolVarSFNode from './sf-custom-nodes/bool-var.svelte'
import NotStartSFNode from './sf-custom-nodes/not-start.svelte'
import NotEndSFNode from './sf-custom-nodes/not-end.svelte'
import SourceSFNode from './sf-custom-nodes/bundling-source.svelte'
import SinkSFNode from './sf-custom-nodes/bundling-sink.svelte'

export interface LadderFlowDisplayerProps {
  context: LirContext
  node: DeclLirNode
}

/************************************************
            Flow Graph
*************************************************/

// export interface FlowGraph {
//   nodes: FlowNode[]
//   edges: FlowEdge[]
// }

/************************************************
          SF Nodes
*************************************************/

export type NodeDimensions = {
  width: number
  height: number
}

export type SFNodeWithMeasuredDimensions = SF.Node & {
  measured: {
    width: number
    height: number
  }
}

export const boolVarNodeType = 'boolVarNode' as const
export const notStartNodeType = 'notStartNode' as const
export const notEndNodeType = 'notEndNode' as const
export const sourceNodeType = 'sourceNode' as const
export const sinkNodeType = 'sinkNode' as const

export type SFNode<T extends string> = SF.Node & { type: T }
function isSFNode<T extends string>(node: SF.Node, type: T): node is SFNode<T> {
  return node.type === type
}

export type SFSourceNode = SFNode<typeof sourceNodeType>
export type SFSinkNode = SFNode<typeof sinkNodeType>

export const isSFSourceNode = (node: SF.Node): node is SFSourceNode =>
  isSFNode(node, sourceNodeType)
export const isSFSinkNode = (node: SF.Node): node is SFSinkNode =>
  isSFNode(node, sinkNodeType)
export const isSFGroupingNode = (
  node: SF.Node
): node is SFSourceNode | SFSinkNode =>
  isSFSourceNode(node) || isSFSinkNode(node)

/************************************************
            Flow Edges
*************************************************/

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

  /** Lexicographical comparison of IDs of nodes */
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

/************************************************
          SvelteFlow Custom Nodes
*************************************************/

/** This is where we declare all the custom nodes for Svelte Flow */
export const sfNodeTypes: SF.NodeTypes = {
  boolVarNode: BoolVarSFNode,
  notStartNode: NotStartSFNode,
  notEndNode: NotEndSFNode,
  sourceNode: SourceSFNode,
  sinkNode: SinkSFNode,
}

export interface SFHandlesInfo {
  sourcePosition: SF.Position
  targetPosition: SF.Position
}

export const defaultSFHandlesInfo: SFHandlesInfo = {
  sourcePosition: SF.Position.Right,
  targetPosition: SF.Position.Left,
}

export interface BoolVarDisplayerProps extends SF.NodeProps {
  data: { name: Name; value: BoolValue }
  // TODO: Will add Value in the next version
}
