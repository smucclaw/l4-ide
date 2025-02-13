<!-- This is an 'internal' component.
 The only reason why we need to wrap this in a parent component
 is because the SvelteFlow lib requires that any use of SF hooks happen
 in a component that descends from a component that initializes SvelteFlowProvider -->
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
  import { useNodesInitialized, useSvelteFlow } from '@xyflow/svelte'
  import { type ExprFlowDisplayerProps, sfNodeTypes } from './types.svelte.js'
  import {
    exprLirNodeToAlgaDag,
    algaUndirectedGraphToFlowGraph,
  } from './lir-to-dag.js'
  import { onMount } from 'svelte'
  import { watch } from 'runed'

  import '@xyflow/svelte/dist/style.css'

  const { context, node: exprLirNode }: ExprFlowDisplayerProps = $props()

  /***********************************
    Make initial SF nodes and edges
  ************************************/

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
      SvelteFlow hooks
  ************************************/

  const sfNodes$Initialized = useNodesInitialized()
  const { fitView } = $derived(useSvelteFlow())
  onMount(() => {
    watch(
      () => sfNodes$Initialized,
      () => {
        if (sfNodes$Initialized) {
          doLayout()
          console.log('nodes initialized')
        }
      }
    )
  })

  /***********************************
      doLayout, Dagre Graph, Config
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

  function doLayout() {
    const layoutedElements = getLayoutedElements(dagreConfig, NODES, EDGES)
    NODES = layoutedElements.nodes
    EDGES = layoutedElements.edges
    console.log('nodes', NODES)
    console.log('edges', EDGES)

    window.requestAnimationFrame(() => {
      console.log('fitting view!')
      fitView({ padding: 0, duration: 25 })
    })
  }
</script>

<div style="height:100vh;">
  <SvelteFlow
    bind:nodes={NODES}
    bind:edges={EDGES}
    nodeTypes={sfNodeTypes}
    fitView
    connectionLineType={ConnectionLineType.Bezier}
    defaultEdgeOptions={{ type: 'bezier', animated: false }}
  >
    <Background />
  </SvelteFlow>
</div>
<!-- Do layout button for debugging doLayout:  -->
<!-- <button onclick={doLayout}>Do layout</button> -->
