import type { Name } from '@repo/viz-expr'
import type { BoolVal } from '$lib/layout-ir/value.js'
import type {
  RootDisplayerProps,
  DisplayerProps,
  LirContext,
} from '$lib/layout-ir/core.js'
import type { DeclLirNode } from '$lib/layout-ir/ladder-lir.svelte.js'
import * as SF from '@xyflow/svelte'
import BoolVarSFNode from './sf-custom-nodes/bool-var.svelte'
import NotStartSFNode from './sf-custom-nodes/not-start.svelte'
import NotEndSFNode from './sf-custom-nodes/not-end.svelte'
import SourceSFNode from './sf-custom-nodes/bundling-source.svelte'
import SinkSFNode from './sf-custom-nodes/bundling-sink.svelte'
import LadderEdge from './sf-custom-edges/ladder-edge.svelte'
import type { StrokeColorCSSVar } from '../../algebraic-graphs/edge.js'
import { emptyEdgeLabel, EmptyEdgeStyles } from '../../algebraic-graphs/edge.js'
import type { LirId } from '$lib/layout-ir/core.js'

export interface LadderFlowDisplayerProps extends RootDisplayerProps {
  node: DeclLirNode
}

export interface BaseLadderFlowDisplayerProps extends DisplayerProps {
  node: LadderFlowDisplayerProps['node']
}

export interface LadderSFGraph {
  nodes: LadderSFNode[]
  edges: LadderSFEdge[]
  sfIdToLirId(sfId: string): LirId
  lirIdToSFId(lirId: LirId): string
}

/************************************************
          SF Nodes
*************************************************/

export interface Dimensions {
  width: number
  height: number
}

export type LadderSFNode = SF.Node

export type LadderSFNodeWithDims = LadderSFNode & {
  measured: Dimensions
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

export const isBoolVarSFNode = (
  node: SF.Node
): node is SFNode<typeof boolVarNodeType> => isSFNode(node, boolVarNodeType)

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
          Custom Nodes
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

// TODO: Not sure we want this after all
export function getOriginalLirIdFromSfNode(node: LadderSFNode): LirId {
  if (!node.data.originalLirId) throw new Error('No originalLirId in node data')

  return node.data.originalLirId as LirId
}

// TODO: Not sure we need this after all
export interface LadderSFNodeData {
  context: LirContext
  originalLirId: LirId
}

export interface BoolVarDisplayerData extends LadderSFNodeData {
  name: Name
  value: BoolVal
}

export interface BoolVarDisplayerProps {
  data: BoolVarDisplayerData
}

/************************************************
          SF Edges
*************************************************/

export type LadderSFEdge = SF.Edge

export const ladderEdgeType = 'ladderEdge' as const

/************************************************
        Custom Edges
*************************************************/

/** This is where we declare all the custom edges for Svelte Flow */
export const sfEdgeTypes: SF.EdgeTypes = {
  ladderEdge: LadderEdge,
}

/* xyflow does this `extends Record<string, unknown>` thing
with their EdgeData */

/** A ladder (SvelteFlow) edge is an edge that is highlightable
 * and can have a label */
export interface LadderEdgeAttrs extends Record<string, unknown> {
  label: string
  strokeColorCSSVar: StrokeColorCSSVar
}

export const defaultLadderEdgeAttrs = {
  label: emptyEdgeLabel,
  strokeColorCSSVar: new EmptyEdgeStyles().getStrokeColor(),
}

export interface LadderEdgeProps extends SF.EdgeProps {
  attrs?: LadderEdgeAttrs
}
