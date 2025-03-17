// Types and util functions for the Svelte Flow graph

import type { Name } from '@repo/viz-expr'
import type { BoolVal } from '$lib/layout-ir/value.js'
import type {
  RootDisplayerProps,
  DisplayerProps,
  LirContext,
  LirId,
} from '$lib/layout-ir/core.js'
import type {
  DeclLirNode,
  PathListLirNode,
} from '$lib/layout-ir/ladder-lir.svelte.js'
import { emptyEdgeLabel, EmptyEdgeStyles } from '../../algebraic-graphs/edge.js'
import * as SF from '@xyflow/svelte'
// SF custom node components
import BoolVarSFNode from './sf-custom-nodes/bool-var.svelte'
import NotStartSFNode from './sf-custom-nodes/not-start.svelte'
import NotEndSFNode from './sf-custom-nodes/not-end.svelte'
import SourceSFNode from './sf-custom-nodes/bundling-source.svelte'
import SinkSFNode from './sf-custom-nodes/bundling-sink.svelte'
import LadderEdge from './sf-custom-edges/ladder-edge.svelte'

/**
 * The result type of the ladder lir graph to SF graph conversion.
 *
 * This should have everything you need to render the SF graph
 * and to translate changes back and forth between the intermediate Ladder Lir representation
 * and the concrete SF gui.
 */
export interface LadderSFGraph {
  nodes: LadderSFNode[]
  edges: LadderSFEdge[]
  sfIdToLirId(sfId: string): LirId
  lirIdToSFId(lirId: LirId): string
}

/************************************************
        Displayer Props
*************************************************/

export interface LadderFlowDisplayerProps extends RootDisplayerProps {
  node: DeclLirNode
}

export interface BaseLadderFlowDisplayerProps extends DisplayerProps {
  node: LadderFlowDisplayerProps['node']
}

export interface PathListDisplayerProps extends DisplayerProps {
  node: PathListLirNode
}

/************************************************
          SF Nodes
*************************************************/

export type LadderSFNode = SF.Node

export function getSFNodeId(node: LadderSFNode): string {
  return node.id
}

export type LadderSFNodeWithDims = LadderSFNode & {
  measured: Dimensions
}

export interface Dimensions {
  width: number
  height: number
}

/************************************************
    Custom node types and type guards
*************************************************/

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
           Custom node type map
*************************************************/

/** This is where we declare all the custom nodes for Svelte Flow */
export const sfNodeTypes: SF.NodeTypes = {
  boolVarNode: BoolVarSFNode,
  notStartNode: NotStartSFNode,
  notEndNode: NotEndSFNode,
  sourceNode: SourceSFNode,
  sinkNode: SinkSFNode,
}

/************************************************
        SF Node data, displayer props
*************************************************/

// Displayer props

export interface BoolVarDisplayerProps {
  data: BoolVarDisplayerData
}

export interface BundlingNodeDisplayerProps {
  data: BundlingNodeDisplayerData
}

// Node data

// TODO: Not sure we need this after all
export interface LadderSFNodeData {
  context: LirContext
  originalLirId: LirId
}

export interface BoolVarDisplayerData extends LadderSFNodeData {
  name: Name
  value: BoolVal
}

export interface BundlingNodeDisplayerData extends LadderSFNodeData {
  /** Currently used for the explanatory labels */
  annotation: string
}

/************************************************
        SF Node handles
*************************************************/

export interface SFHandlesInfo {
  sourcePosition: SF.Position
  targetPosition: SF.Position
}

// TODO: Improve how this is used -- took a shortcut here
export const defaultSFHandlesInfo: SFHandlesInfo = {
  sourcePosition: SF.Position.Right,
  targetPosition: SF.Position.Left,
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
  edgeStyles: string
}

export const defaultLadderEdgeAttrs = {
  label: emptyEdgeLabel,
  edgeStyles: new EmptyEdgeStyles().getStyleString(),
}

export interface LadderEdgeData extends LadderEdgeAttrs {
  context: LirContext
}

export interface LadderEdgeProps extends SF.EdgeProps {
  data?: LadderEdgeData
}
