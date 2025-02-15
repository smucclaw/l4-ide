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
  import {
    type ExprFlowDisplayerProps,
    sfNodeTypes,
    type NodeWithMeasuredDimensions,
  } from './types.svelte.js'
  import {
    exprLirNodeToAlgaDag,
    algaUndirectedGraphToFlowGraph,
  } from './lir-to-dag.js'
  import { onMount } from 'svelte'
  import { Debounced, watch } from 'runed'

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

  const layoutDebounceMs = 100

  // Set up the initial SvelteFlow hooks
  const sfNodes$Initialized = useNodesInitialized()
  const debouncedSfNodes$Initialized = new Debounced(
    () => sfNodes$Initialized.current,
    layoutDebounceMs
  )
  const { fitView } = $derived(useSvelteFlow())

  // Keep track of whether nodes have been layouted, so that won't display them before then
  let nodes$AreLayouted = $state(false)
  $inspect('nodes layouted', nodes$AreLayouted)
  const flowOpacity = $derived(nodes$AreLayouted ? 1 : 0)
  // $inspect('flowOpacity: ' + `${flowOpacity}`)

  onMount(() => {
    // Layout only after the nodes have been measured (have a width and height)
    watch(
      () => debouncedSfNodes$Initialized.current,
      () => {
        if (debouncedSfNodes$Initialized.current) {
          doLayoutAndFitView()
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
    if (
      debouncedSfNodes$Initialized.current &&
      NODES[0] &&
      NODES[0].measured?.width
    ) {
      const layoutedElements = getLayoutedElements(
        dagreConfig,
        NODES as NodeWithMeasuredDimensions[],
        EDGES
      )
      NODES = layoutedElements.nodes
      EDGES = layoutedElements.edges
      console.log('nodes', NODES)
      console.log('edges', EDGES)
    }
  }
  function doFitView() {
    window.requestAnimationFrame(() => {
      console.log('fitting view!')
      fitView({ padding: 1, duration: 15 })
    })
  }
  function doLayoutAndFitView() {
    doLayout()
    nodes$AreLayouted = true
    doFitView()
  }
</script>

<div style={`height:100svh; opacity: ${flowOpacity}`}>
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
<button onclick={doLayout}>Do layout</button>
<button onclick={doLayoutAndFitView}>Do layout and fit view</button>
