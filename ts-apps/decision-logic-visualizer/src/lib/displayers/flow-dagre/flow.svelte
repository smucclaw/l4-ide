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

  import '@xyflow/svelte/dist/style.css'

  import { initialNodes, initialEdges } from './nodes-and-edges'

  const dagreGraph = new dagre.graphlib.Graph()
  dagreGraph.setDefaultEdgeLabel(() => ({}))

  // CONFIG / CONSTS
  const graphDirection = 'LR' // horizontal
  // qn: how did they decide on these numbers?
  const nodeWidth = 172
  const nodeHeight = 36

  function getLayoutedElements(nodes: Node[], edges: Edge[]) {
    dagreGraph.setGraph({ rankdir: graphDirection })

    nodes.forEach((node) => {
      dagreGraph.setNode(node.id, { width: nodeWidth, height: nodeHeight })
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
          x: nodeWithPosition.x - nodeWidth / 2,
          y: nodeWithPosition.y - nodeHeight / 2,
        },
      }
    })

    return { nodes: layoutedNodes, edges }
  }

  const { nodes: layoutedNodes, edges: layoutedEdges } = getLayoutedElements(
    initialNodes,
    initialEdges
  )

  let NODES = $state.raw<Node[]>(layoutedNodes)
  let EDGES = $state.raw<Edge[]>(layoutedEdges)

  // function onLayout() {
  //   const layoutedElements = getLayoutedElements(NODES, EDGES)

  //   NODES = layoutedElements.nodes
  //   EDGES = layoutedElements.edges
  // }
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
