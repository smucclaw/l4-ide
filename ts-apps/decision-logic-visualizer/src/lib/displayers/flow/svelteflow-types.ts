// Types and util functions for the Svelte Flow graph
import type { LadderLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte'
import type { LirContext, LirId } from '$lib/layout-ir/core.js'
import * as SF from '@xyflow/svelte'
// SF custom node components
import TrueExprSFNode from './sf-custom-nodes/true-expr.svelte'
import FalseExprSFNode from './sf-custom-nodes/false-expr.svelte'
import UBoolVarSFNode from './sf-custom-nodes/ubool-var.svelte'
import AppSFNode from './sf-custom-nodes/app.svelte'
import NotStartSFNode from './sf-custom-nodes/not-start.svelte'
import NotEndSFNode from './sf-custom-nodes/not-end.svelte'
import SourceSFNode from './sf-custom-nodes/bundling-source.svelte'
import SinkSFNode from './sf-custom-nodes/bundling-sink.svelte'
import LadderEdge from './sf-custom-edges/ladder-edge.svelte'

import {
  emptyEdgeLabel,
  EdgeStylesContainer,
} from '$lib/layout-ir/ladder-graph/edge-attributes.js'

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
          SF Nodes
*************************************************/

export type LadderSFNode = SF.Node & { data: LadderSFNodeData }

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

export const uBoolVarNodeType = 'uBoolVarNode' as const
export const appNodeType = 'appNode' as const
export const notStartNodeType = 'notStartNode' as const
export const notEndNodeType = 'notEndNode' as const
export const sourceNoAnnoNodeType = 'sourceNoAnnoNode' as const
export const sourceWithOrAnnoNodeType = 'sourceWithOrAnnoNode' as const
export const sinkNodeType = 'sinkNode' as const
export const trueExprNodeType = 'trueExprNode' as const
export const falseExprNodeType = 'falseExprNode' as const

export type SFNode<T extends string> = SF.Node & { type: T }
function isSFNode<T extends string>(node: SF.Node, type: T): node is SFNode<T> {
  return node.type === type
}

export const isBoolVarSFNode = (
  node: SF.Node
): node is SFNode<typeof uBoolVarNodeType> => isSFNode(node, uBoolVarNodeType)

export const isTrueExprSFNode = (
  node: SF.Node
): node is SFNode<typeof trueExprNodeType> => isSFNode(node, trueExprNodeType)
export const isFalseExprSFNode = (
  node: SF.Node
): node is SFNode<typeof falseExprNodeType> => isSFNode(node, falseExprNodeType)

export const isSFAppNode = (node: SF.Node): node is SFAppNode =>
  isSFNode(node, appNodeType)
export type SFAppNode = SFNode<typeof appNodeType>

export type SFSourceNoAnnoNode = SFNode<typeof sourceNoAnnoNodeType>
export type SFSinkNode = SFNode<typeof sinkNodeType>

export const isSFSourceNoAnnoNode = (
  node: SF.Node
): node is SFSourceNoAnnoNode => isSFNode(node, sourceNoAnnoNodeType)
export const isSFSinkNode = (node: SF.Node): node is SFSinkNode =>
  isSFNode(node, sinkNodeType)
export const isSFBundlingNode = (
  node: SF.Node
): node is SFSourceNoAnnoNode | SFSinkNode =>
  isSFSourceNoAnnoNode(node) || isSFSinkNode(node)

/************************************************
           Custom node type map
*************************************************/

/** This is where we declare all the custom nodes for Svelte Flow */
export const sfNodeTypes: SF.NodeTypes = {
  [uBoolVarNodeType]: UBoolVarSFNode,
  [appNodeType]: AppSFNode,
  [notStartNodeType]: NotStartSFNode,
  [notEndNodeType]: NotEndSFNode,
  [sourceNoAnnoNodeType]: SourceSFNode,
  [sourceWithOrAnnoNodeType]: SourceSFNode,
  [sinkNodeType]: SinkSFNode,
  [trueExprNodeType]: TrueExprSFNode,
  [falseExprNodeType]: FalseExprSFNode,
}

/************************************************
        SF Node data, displayer props
*************************************************/

// Displayer props

export interface LadderNodeDisplayerProps {
  data: LadderSFNodeData
}

export interface BundlingNodeDisplayerProps {
  data: BundlingNodeDisplayerData
}

// Node data

export interface LadderSFNodeData {
  context: LirContext
  node: LadderLirNode
}

// TODO: refactor bundling node displayers to just use LadderSFNodeData as well
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
  labelStyles: string
}

export const defaultLadderEdgeAttrs = {
  label: emptyEdgeLabel,
  edgeStyles: new EdgeStylesContainer().getCombinedEdgeStyleString(),
  labelStyles: new EdgeStylesContainer().getLabelStyleString(),
}

export interface LadderEdgeData extends LadderEdgeAttrs {
  context: LirContext
}

export interface LadderEdgeProps extends SF.EdgeProps {
  /** `data` has to be optional because of current SF limitations.
   * But I've been told by a SF developer that this will be improved in the future
   * (i.e., we should be able to specify that it's non-optional in the future)
   */
  data?: LadderEdgeData
}
