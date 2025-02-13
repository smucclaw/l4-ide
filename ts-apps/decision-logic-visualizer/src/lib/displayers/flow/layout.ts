import dagre from '@dagrejs/dagre'
import type { Node, Edge } from '@xyflow/svelte'
import { Position } from '@xyflow/svelte'

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

// TODO: May want the layout function to work with the intermediate DAG representation,
// as opposed to working with the SF nodes/edges directly
/** Adapted from sample code */
export function getLayoutedElements(
  config: DagreConfig,
  nodes: Node[],
  edges: Edge[]
) {
  config.dagreGraph.setGraph({ rankdir: config.graph.direction })

  nodes.forEach((node) => {
    config.dagreGraph.setNode(node.id, {
      width: node.measured?.width ?? config.graph.defaultNodeWidth,
      height: node.measured?.height ?? config.graph.defaultNodeHeight,
    })
  })

  edges.forEach((edge) => {
    config.dagreGraph.setEdge(edge.source, edge.target)
  })

  dagre.layout(config.dagreGraph)

  const layoutedNodes = nodes.map((node) => {
    const nodeWithPosition = config.dagreGraph.node(node.id)
    // these positions are what get recommended for a horizontal direction
    node.targetPosition = Position.Left
    node.sourcePosition = Position.Right

    return {
      ...node,
      // Shift the dagre node position (anchor=center center) to the top left
      // so it matches the React Flow node anchor point (top left).
      position: {
        x: nodeWithPosition.x - config.graph.defaultNodeWidth / 2,
        y: nodeWithPosition.y - config.graph.defaultNodeHeight / 2,
      },
    }
  })

  return { nodes: layoutedNodes, edges }
}
