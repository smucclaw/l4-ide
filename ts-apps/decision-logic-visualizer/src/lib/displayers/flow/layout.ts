import dagre from '@dagrejs/dagre'
import type { Edge } from '@xyflow/svelte'
import { Position } from '@xyflow/svelte'
import {
  isSFGroupingNode,
  type NodeWithMeasuredDimensions,
} from './types.svelte.js'

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

  nodes.forEach((node) => {
    console.log(
      'node width, height',
      node.measured?.width,
      node.measured?.height
    )
    config.dagreGraph.setNode(node.id, {
      // including the fallback options because I don't fully trust TS' type system
      width: node.measured?.width ?? config.graph.defaultNodeWidth,
      height: node.measured?.height ?? config.graph.defaultNodeHeight,
    })
  })

  // Edges
  edges.forEach((edge) => {
    config.dagreGraph.setEdge(edge.source, edge.target)
  })

  dagre.layout(config.dagreGraph, { nodesep: 30, marginx: 8 })

  const layoutedNodes = nodes.map((node) => {
    const nodeWithPosition = config.dagreGraph.node(node.id)
    // these positions are what get recommended for a horizontal direction
    node.targetPosition = Position.Left
    node.sourcePosition = Position.Right

    return {
      ...node,

      // TODO: Clean up code below
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

/* https://github.com/dagrejs/dagre/wiki#configuring-the-layout

  Experiment log re layouting
  ===========================
  * The biggest difference / issue so far: the anchor positions for the grouping nodes were indeed not being set correctly!
    Setting them correctly (i.e., not doing the shift) for the grouping nodes got them to be at the positions one'd naturally expect,
    at least in the demo +page.svelte in DLV.

  * Experimented a bit with nodeSep and rankSep, but those don't seem to make much of a difference
  * Was going to look also in to compound graphs (which also requires modifying the traversal), but I don't think that's necessary now.
  */
