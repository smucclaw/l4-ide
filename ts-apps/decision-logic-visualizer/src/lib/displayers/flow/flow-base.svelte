<!-- This is an 'internal' component.
 The only reason why we need to wrap this in a parent component
 is because the SvelteFlow lib requires that any use of SF hooks happen
 in a component that descends from a component that initializes SvelteFlowProvider -->
<script lang="ts">
  import type { LirId } from '$lib/layout-ir/core.js'
  import {
    LirContext,
    getLirRegistryFromSvelteContext,
  } from '$lib/layout-ir/core.js'
  import dagre from '@dagrejs/dagre'
  import { getLayoutedElements, type DagreConfig } from './layout.js'
  import {
    SvelteFlow,
    Background,
    Controls,
    ConnectionLineType,
    useNodesInitialized,
    useSvelteFlow,
  } from '@xyflow/svelte'
  import * as SF from '@xyflow/svelte'
  import {
    type BaseLadderFlowDisplayerProps,
    sfNodeTypes,
    sfEdgeTypes,
    isBoolVarSFNode,
    type LadderSFNodeWithDims,
    type LadderSFGraph,
    getOriginalLirIdFromSfNode,
  } from './types.svelte.js'
  import { ladderGraphToSFGraph } from './ladder-lir-to-sf.js'
  import { cycle } from '$lib/layout-ir/value.js'
  import { onMount } from 'svelte'
  import { Debounced, watch } from 'runed'

  import '@xyflow/svelte/dist/style.css'
  import type {
    BoolVarLirNode,
    LadderLirNode,
  } from '$lib/layout-ir/ladder-lir.svelte.js'

  /************************
       Lir
  *************************/

  const { context, node: declLirNode }: BaseLadderFlowDisplayerProps = $props()
  const lir = getLirRegistryFromSvelteContext()

  /***********************************
      SvelteFlow config
  ************************************/

  /* TODO:
  - Come up with more-easily-understandable units for the minZoom
  - Prob good to add contextual zoom + hint ("Zoom in to see the details!") for wider graphs
  */
  const sfVisualOptions = {
    smallestThatCanZoomOutTo: 0.2,
  }

  /***********************************
      SvelteFlow nodes and edges
  ************************************/

  // Initial nodes and edges
  const ladderGraph = declLirNode.getBody(context)
  const initialSfGraph = ladderGraphToSFGraph(context, ladderGraph)

  // SvelteFlow nodes and edges variables
  let NODES = $state.raw<LadderSFGraph['nodes']>(initialSfGraph.nodes)
  let EDGES = $state.raw<LadderSFGraph['edges']>(initialSfGraph.edges)
  // $inspect(NODES)

  // SfId, LirId helpers
  let sfIdToLirId = initialSfGraph.sfIdToLirId
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  let lirIdToSfId = initialSfGraph.lirIdToSFId

  /***********************************
      SvelteFlow hooks
  ************************************/

  const layoutDebounceMs = 20

  // Set up the initial SvelteFlow hooks
  const sfNodes$Initialized = useNodesInitialized()
  const debouncedSfNodes$Initialized = new Debounced(
    () => sfNodes$Initialized.current,
    layoutDebounceMs
  )
  const { fitView } = $derived(useSvelteFlow())

  // Keep track of whether nodes have been layouted, so that won't display them before then
  let nodes$AreLayouted = $state(false)
  // $inspect('nodes layouted', nodes$AreLayouted)
  const flowOpacity = $derived(nodes$AreLayouted ? 1 : 0)
  // $inspect('flowOpacity: ' + `${flowOpacity}`)

  onMount(() => {
    // Layout only after the nodes have been measured (have a width and height)
    watch(
      () => debouncedSfNodes$Initialized.current,
      () => {
        // sfNodes$Initialized seems to get updated twice on the first render.
        // But it does NOT re-fire if e.g. we re-generate the SF graph
        // after the first complete initialization
        if (debouncedSfNodes$Initialized.current) {
          doLayoutAndFitView()
        }
      }
    )

    lir.subscribe(onLadderGraphNonPositionalChange)
    // TODO: Clean up subscribers --- add an onDestroy in core.ts
  })

  /*************************************
      Other SvelteFlow event listeners
  ***************************************/

  // TODO: prob better to put this in bool-var.svelte.ts
  const onBoolVarNodeClick: SF.NodeEventWithPointer<MouseEvent | TouchEvent> = (
    event
  ) => {
    const lirId = sfIdToLirId(event.node.id)
    const lirBoolVarNode = context.get(lirId) as BoolVarLirNode

    const newValue = cycle(lirBoolVarNode.getValue(context))
    ladderGraph.submitNewBinding(context, {
      unique: lirBoolVarNode.getUnique(context),
      value: newValue,
    })
  }

  const onNodeDragStop: SF.NodeTargetEventWithPointer<
    MouseEvent | TouchEvent
  > = (event) => {
    if (event.targetNode) {
      const lirNode = context.get(
        sfIdToLirId(event.targetNode.id)
      ) as LadderLirNode
      lirNode.setPosition(context, event.targetNode.position)
    }
  }

  /*********************************************
        LadderGraph event listener
  **********************************************/

  /**
   * Most naive version.
   *
   *  Assumes that the LadderGraphLirNode does NOT publish position changes (may revisit this in the future)
   */
  const onLadderGraphNonPositionalChange = (context: LirContext, id: LirId) => {
    if (id === ladderGraph.getId()) {
      const newSfGraph = ladderGraphToSFGraph(context, ladderGraph)
      NODES = newSfGraph.nodes
      EDGES = newSfGraph.edges
      sfIdToLirId = newSfGraph.sfIdToLirId
      lirIdToSfId = newSfGraph.lirIdToSFId
      // console.log('newSfGraph NODES', NODES)
    }
  }

  /*********************************************
            Layout & Fit View
  **********************************************/

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
      NODES.every((node) => node.measured?.height && node.measured?.width)
    ) {
      // Layout
      const layoutedElements = getLayoutedElements(
        dagreConfig,
        NODES as LadderSFNodeWithDims[],
        EDGES
      )
      NODES = layoutedElements.nodes
      EDGES = layoutedElements.edges

      // Update Lir with the positions and dimensions
      // (We need to do this, because we re-generate the SF graph from the LadderGraphLirNode
      // when data associated with the Lir nodes or edges changes.)
      layoutedElements.nodes.forEach((sfNode: LadderSFNodeWithDims) => {
        const lirNode = context.get(
          getOriginalLirIdFromSfNode(sfNode)
        ) as LadderLirNode
        lirNode.setPosition(context, sfNode.position)
        lirNode.setDimensions(context, {
          width: sfNode.measured.width,
          height: sfNode.measured.height,
        })
      })
    }
  }

  function doFitView() {
    window.requestAnimationFrame(() => {
      fitView({
        padding: 0.1,
        minZoom: sfVisualOptions.smallestThatCanZoomOutTo,
        duration: 15,
      })
      /***************************
       * Notes on fitView options
       ***************************
       *
       * 0.1 is the default
       *
       * The padding gets used in `getViewportForBounds` in @xyflow/system:
       *
       * https://github.com/xyflow/xyflow/blob/23669c330d2344d6ae19a237b69a74ee34fc64e8/packages/system/src/utils/general.ts#L177
       *
       * See their `src/lib/container/SvelteFlow/types.ts` for the defaults they use.
       *
       * `minZoom` is the smallest zoom level that the view *can* be zoomed to when the flow is fit to view.
       * I.e., decreasing it means that fitView can zoom out more for wider graphs.
       * Being able to zoom out more seems helpful for our usecase (understanding the broad structure of the law).
       * The default minZoom is 0.5.
       */
    })
  }

  function doLayoutAndFitView() {
    doLayout()
    nodes$AreLayouted = true
    doFitView()
  }
</script>

<!-- 
Misc SF UI TODOs:

* Make it clearer that the bool var nodes are clickable 
(should at least change the cursor to a pointer on mouseover)
-->

<!-- The consumer containing div must set the height to, e.g., 96svh if that's what's wanted -->
<div class="overall-container">
  <div class="flow-container" style={`height:100%; opacity: ${flowOpacity}`}>
    <SvelteFlow
      bind:nodes={NODES}
      bind:edges={EDGES}
      nodeTypes={sfNodeTypes}
      edgeTypes={sfEdgeTypes}
      minZoom={sfVisualOptions.smallestThatCanZoomOutTo}
      fitView
      connectionLineType={ConnectionLineType.Bezier}
      defaultEdgeOptions={{ type: 'bezier', animated: false }}
      onnodeclick={(event) => {
        if (isBoolVarSFNode(event.node)) onBoolVarNodeClick(event)
      }}
      onnodedragstop={onNodeDragStop}
    >
      <!-- disabling show lock because it didn't seem to do anything for me --- might need to adjust some other setting too -->
      <Controls position="bottom-right" showLock={false} />
      <Background />
    </SvelteFlow>
  </div>
</div>

<!-- For debugging -->
<!-- <button onclick={doLayout}>Do layout</button>
<button onclick={doLayoutAndFitView}>Do layout and fit view</button> -->

<style>
  .overall-container {
    display: flex;
    flex-direction: column;
    height: 100%;
  }

  .flow-container {
    flex: 1 1 auto;
    min-height: 0; /* Prevents overflow */
  }

  /* .paths-container {
    flex: 0 0 auto;
  } */
</style>
