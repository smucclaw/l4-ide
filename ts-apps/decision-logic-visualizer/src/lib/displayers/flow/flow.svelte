<script lang="ts">
  import dagre from '@dagrejs/dagre'
  import { getLayoutedElements, type DagreConfig } from './layout.ts'
  import {
    SvelteFlow,
    Background,
    ConnectionLineType,
    type Node,
    type Edge,
  } from '@xyflow/svelte'
  import {
    type ExprFlowDisplayerProps,
    exprLirNodeToAlgaUndirectedGraph,
    algaUndirectedGraphToFlowGraph,
  } from './types.svelte.ts'

  import { onMount } from 'svelte'

  import '@xyflow/svelte/dist/style.css'

  const { context, node: exprLirNode }: ExprFlowDisplayerProps = $props()

  const flowGraph = algaUndirectedGraphToFlowGraph(
    exprLirNodeToAlgaUndirectedGraph(context, exprLirNode)
  )

  const initialNodes = flowGraph.nodes
    .toSorted((v1, v2) => v2.compare(v1))
    .map((n) => n.toSFPojo())
  const initialEdges = flowGraph.edges.map((e) => e.toSFPojo())

  /***********************************
      SvelteFlow nodes and edges
  ************************************/

  let NODES = $state.raw<Node[]>(initialNodes)
  let EDGES = $state.raw<Edge[]>(initialEdges)

  /***********************************
      Dagre Graph and Config
  ************************************/

  const dagreGraph = new dagre.graphlib.Graph()
  dagreGraph.setDefaultEdgeLabel(() => ({}))

  const dagreConfig: DagreConfig = {
    dagreGraph: dagreGraph,
    graph: {
      direction: 'LR', // horizontal
      // qn: how did they decide on these numbers?
      // Doesn't matter though, since we'll use the measured width and height if available
      defaultNodeWidth: 172,
      defaultNodeHeight: 36,
    },
  }

  onMount(() => {
    const layoutedElements = getLayoutedElements(
      dagreConfig,
      initialNodes,
      initialEdges
    )
    NODES = layoutedElements.nodes
    EDGES = layoutedElements.edges

    console.log(EDGES.length)
    console.log('edges', EDGES)
    console.log('onMounted!')
  })
</script>

<div style="height:50vh;">
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
