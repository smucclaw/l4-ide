<script lang="ts">
  import dagre from '@dagrejs/dagre'
  import { getLayoutedElements, type DagreConfig } from './layout.js'
  import {
    SvelteFlow,
    Background,
    ConnectionLineType,
    type Node,
    type Edge,
  } from '@xyflow/svelte'
  import {
    type ExprFlowDisplayerProps,
    exprLirNodeToAlgaDag,
    algaUndirectedGraphToFlowGraph,
  } from './types.svelte.js'

  import { onMount } from 'svelte'

  import '@xyflow/svelte/dist/style.css'

  const { context, node: exprLirNode }: ExprFlowDisplayerProps = $props()

  const flowGraph = algaUndirectedGraphToFlowGraph(
    exprLirNodeToAlgaDag(context, exprLirNode)
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

    console.log('nodes', NODES)
    console.log('edges', EDGES)
  })
</script>

<div style="height:80vh;">
  <SvelteFlow
    bind:nodes={NODES}
    bind:edges={EDGES}
    fitView
    connectionLineType={ConnectionLineType.Bezier}
    defaultEdgeOptions={{ type: 'bezier', animated: false }}
  >
    <Background />
  </SvelteFlow>
</div>
<section>
  <p>For debugging</p>
  <p>Nodes</p>
  <pre><code>
    {JSON.stringify(NODES, null, 2)}
  </code></pre>
  <p>edges</p>
  <pre><code>
    {JSON.stringify(EDGES, null, 2)}
  </code></pre>
</section>
