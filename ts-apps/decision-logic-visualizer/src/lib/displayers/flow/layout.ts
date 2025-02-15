import dagre from '@dagrejs/dagre'
import type { Edge, Node } from '@xyflow/svelte'
import { Position } from '@xyflow/svelte'
import {
  isSFGroupingNode,
  type NodeWithMeasuredDimensions,
  type NodeDimensions,
  type SFHandlesInfo,
} from './types.svelte.js'
import partition from 'lodash/partition'

export interface DagreConfig {
  // TODO: Clean this up in the future!
  // eslint-disable-next-line @typescript-eslint/no-empty-object-type
  dagreGraph: dagre.graphlib.Graph<{}>
  graph: {
    direction: 'TB' | 'LR'
    defaultNodeWidth: number
    defaultNodeHeight: number
  }
}

// TODO-IMPT: Check if Dagre / SF is able to get the right viewport size when this is opened in VSC

// TODO: May want the layout function to work with the intermediate DAG representation,
// as opposed to working with the SF nodes/edges directly
/**
 * Assumes that the input nodes have already been measured (i.e., have a measured.width and measured.height).
 *
 * Adapted from SvelteFlow lib's sample code. */
export function getLayoutedElements(
  config: DagreConfig,
  nodes: NodeWithMeasuredDimensions[],
  edges: Edge[]
) {
  config.dagreGraph.setGraph({ rankdir: config.graph.direction })

  // Node dimensions

  const [groupingNodes, nonGroupingNodes] = partition(nodes, isSFGroupingNode)

  // nonGroupingNodes
  nonGroupingNodes.forEach((node) => {
    console.log(
      'node width, height',
      node.measured?.width,
      node.measured?.height
    )
    config.dagreGraph.setNode(node.id, {
      width: node.measured?.width ?? config.graph.defaultNodeWidth,
      height: node.measured?.height ?? config.graph.defaultNodeHeight,
    })
  })

  // groupingNodes
  // TODO in the future: See if constant height would actually be fine -- prob overthinking this
  const halfOfmedianHeight =
    getPercentileNodeDimensions(
      nonGroupingNodes as NodeWithMeasuredDimensions[],
      50
    ).height / 2
  const exemplarWidth = 10
  const groupingNodeDims = { width: exemplarWidth, height: halfOfmedianHeight }

  console.log('groupingNodeDims', groupingNodeDims)
  groupingNodes.forEach((node) => {
    config.dagreGraph.setNode(node.id, {
      width: groupingNodeDims.width,
      height: groupingNodeDims.height,
    })
  })

  // Edges
  edges.forEach((edge) => {
    config.dagreGraph.setEdge(edge.source, edge.target)
  })

  /* https://github.com/dagrejs/dagre/wiki#configuring-the-layout
  
  Experiment log
  ====================
  * The biggest difference / issue so far: the anchor positions for the grouping nodes were indeed not being set correctly!
    Setting them correctly (i.e., not doing the shift) for the grouping nodes got them to be at the positions one'd naturally expect,
    at least in the demo +page.svelte in DLV.

  * Currently experimenting with nodeSep and rankSep, but it doesn't seem to make much of a difference
  * Setting grouping node dims to {width: 40, height: 40} still isn't enough to get the edges working properly
  * Next big thing to try: compound graphs (which also requires modifying the traversal)
  */
  dagre.layout(config.dagreGraph, { nodesep: 30, marginx: 8 })

  const layoutedNodes = nodes.map((node) => {
    const nodeWithPosition = config.dagreGraph.node(node.id)
    // these positions are what get recommended for a horizontal direction
    node.targetPosition = Position.Left
    node.sourcePosition = Position.Right

    return {
      ...node,
      // TODO: Refactor the stuff below to use a helper function
      // Shift the dagre node position (anchor=center center) for NON-grouping nodes to the top left
      // so it matches the React Flow node anchor point (top left).
      position: isSFGroupingNode(node)
        ? {
            x: nodeWithPosition.x,
            y: nodeWithPosition.y,
          }
        : {
            x: nodeWithPosition.x - nodeWithPosition.width / 2,
            y: nodeWithPosition.y - nodeWithPosition.height / 2,
          },
    }
  })

  return { nodes: layoutedNodes, edges }
}

/***********************************
            Helpers
************************************/

/**
 * Calculates the specified percentile dimensions from an array of nodes.
 *
 * @param nodes - Non-empty array of nodes with measured dimensions.
 * @param percentile - The desired percentile (e.g., 35 for the 35th percentile).
 * @returns An object containing the width and height at the specified percentile.
 */
export function getPercentileNodeDimensions(
  nodes: NodeWithMeasuredDimensions[],
  percentile: number
): NodeDimensions {
  // Guards
  if (nodes.length === 0) {
    throw new Error('The nodes array must contain at least one node.')
  }
  // Ensure the percentile value is between 0 and 100
  if (percentile < 0 || percentile > 100) {
    throw new Error('Percentile must be between 0 and 100.')
  }

  const widths = nodes.map((node) => node.measured!.width).sort((a, b) => a - b)
  const heights = nodes
    .map((node) => node.measured!.height)
    .sort((a, b) => a - b)

  const lastIdxInInputArray = nodes.length - 1
  const index = (percentile / 100) * lastIdxInInputArray

  const widthPercentile = getPercentileValue(widths, index)
  const heightPercentile = getPercentileValue(heights, index)

  return {
    width: widthPercentile,
    height: heightPercentile,
  }
}

// Helper function to compute the percentile value with interpolation
function getPercentileValue(sortedValues: number[], idx: number): number {
  if (Number.isInteger(idx)) {
    // Exact index, no interpolation needed
    return sortedValues[idx]
  } else {
    // Linear interpolation between the two surrounding values
    const lowerIdx = Math.floor(idx)
    const upperIdx = Math.ceil(idx)

    const weight = idx - lowerIdx

    return (
      sortedValues[lowerIdx] +
      (sortedValues[upperIdx] - sortedValues[lowerIdx]) * weight
    )
  }
}
