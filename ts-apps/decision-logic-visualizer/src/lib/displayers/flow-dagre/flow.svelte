<script lang="ts">
  import dagre from '@dagrejs/dagre'
  import {
    SvelteFlow,
    Background,
    Position,
    ConnectionLineType,
    type Node,
    type Edge,
  } from '@xyflow/svelte'
  import { onMount } from 'svelte'

  import '@xyflow/svelte/dist/style.css'

  import { initialNodes, initialEdges } from './nodes-and-edges'

  const dagreGraph = new dagre.graphlib.Graph()
  dagreGraph.setDefaultEdgeLabel(() => ({}))

  let NODES = $state.raw<Node[]>([])
  let EDGES = $state.raw<Edge[]>([])

  // CONFIG / CONSTS
  const graphDirection = 'LR' // horizontal
  // qn: how did they decide on these numbers?
  const defaultNodeWidth = 172
  const defaultNodeHeight = 36

  function getLayoutedElements(nodes: Node[], edges: Edge[]) {
    dagreGraph.setGraph({ rankdir: graphDirection })

    nodes.forEach((node) => {
      dagreGraph.setNode(node.id, {
        width: node.measured?.width ?? defaultNodeWidth,
        height: node.measured?.height ?? defaultNodeHeight,
      })
    })

    edges.forEach((edge) => {
      dagreGraph.setEdge(edge.source, edge.target)
    })

    dagre.layout(dagreGraph)

    const layoutedNodes = nodes.map((node) => {
      const nodeWithPosition = dagreGraph.node(node.id)
      // these positions are what get recommended for a horizontal direction
      node.targetPosition = Position.Left
      node.sourcePosition = Position.Right

      return {
        ...node,
        // We are shifting the dagre node position (anchor=center center) to the top left
        // so it matches the React Flow node anchor point (top left).
        position: {
          x: nodeWithPosition.x - defaultNodeWidth / 2,
          y: nodeWithPosition.y - defaultNodeHeight / 2,
        },
      }
    })

    return { nodes: layoutedNodes, edges }
  }

  onMount(() => {
    const layoutedElements = getLayoutedElements(initialNodes, initialEdges)
    NODES = layoutedElements.nodes
    EDGES = layoutedElements.edges
  })
</script>

<div style="height:100vh;">
  <SvelteFlow
    bind:nodes={NODES}
    bind:edges={EDGES}
    fitView
    connectionLineType={ConnectionLineType.SmoothStep}
    defaultEdgeOptions={{ type: 'smoothstep', animated: false }}
  >
    <Background />
  </SvelteFlow>
</div>
