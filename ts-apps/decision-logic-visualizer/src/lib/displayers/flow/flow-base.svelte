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
    Controls,
    ConnectionLineType,
    type Node,
    type Edge,
  } from '@xyflow/svelte'
  import { useNodesInitialized, useSvelteFlow } from '@xyflow/svelte'
  import {
    type LadderFlowDisplayerProps,
    sfNodeTypes,
    type SFNodeWithMeasuredDimensions,
  } from './types.svelte.js'
  import { ladderGraphToSFGraph } from './ladder-lir-to-sf.js'
  import { onMount } from 'svelte'
  import { Debounced, watch } from 'runed'

  import '@xyflow/svelte/dist/style.css'

  const { context, node: declLirNode }: LadderFlowDisplayerProps = $props()

  /* TODO:
  - Come up with more-easily-understandable units for the minZoom
  - Prob good to add contextual zoom + hint ("Zoom in to see the details!") for wider graphs
  */
  const sfVisualOptions = {
    smallestThatCanZoomOutTo: 0.2,
  }

  /***********************************
    Make initial SF nodes and edges
  ************************************/

  const ladderGraph = declLirNode.getBody(context)
  const sfGraph = ladderGraphToSFGraph(context, ladderGraph)

  /***********************************
      SvelteFlow nodes and edges
  ************************************/

  let NODES = $state.raw<Node[]>(sfGraph.nodes)
  let EDGES = $state.raw<Edge[]>(sfGraph.edges)

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

  // keep track of currently selected path
  let selectedPathId = $state<string | null>(null)

  // path info with ids for each path for selection
  const paths = ladderGraph.getPaths(context).map((path, index) => {
  // path id based on vertices
    const vertices = path.getVertices(context);
    const pathId = vertices.map(v => v.getId().toString()).join('-');

    return {
      path,
      index,
      id: pathId,
      displayId: `${index + 1}`,
      pretty: path.toPretty(context)
    };
  });

  // to handle radio button selection
  // the radio button is actually a checkbox because radio buttons can't be toggled
  function handlePathSelect(pathInfo: typeof paths[0]) {
  if (selectedPathId === pathInfo.id) {
    // deselect path if it's already selected
    selectedPathId = null;
    pathInfo.path.deselect(context);
  } else {
    // deselect previously selected path
    if (selectedPathId !== null) {
      const previousPath = paths.find(p => p.id === selectedPathId)?.path;
      if (previousPath) {
        previousPath.deselect(context);
      }
    }
    // ...and select new path
    selectedPathId = pathInfo.id;
    pathInfo.path.select(context);
  }
}

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
        NODES as SFNodeWithMeasuredDimensions[],
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

<div style={`height:96svh; opacity: ${flowOpacity}`}>
  <SvelteFlow
    bind:nodes={NODES}
    bind:edges={EDGES}
    nodeTypes={sfNodeTypes}
    minZoom={sfVisualOptions.smallestThatCanZoomOutTo}
    fitView
    connectionLineType={ConnectionLineType.Bezier}
    defaultEdgeOptions={{ type: 'bezier', animated: false }}
  >
    <!-- disabling show lock because it didn't seem to do anything for me --- might need to adjust some other setting too -->
    <Controls position="bottom-right" showLock={false} />
    <Background />
  </SvelteFlow>
</div>
<section>
  <div class="flex flex-col gap-2 w-4/5">
      <ul class="space-y-1 text-gray-500 list-none list-inside dark:text-gray-400">
        {#each paths as pathInfo}
        <li class="grid grid-cols-6">
          <div class="font-semibold col-span-1">
            <div class={`w-[30px] border-2 rounded-full text-center transition-colors ${selectedPathId === pathInfo.id ? 'bg-green-600 text-white border-green-600' : 'border-gray-300'}`}>
              {pathInfo.displayId}
            </div>
          </div>
          <label for={pathInfo.id} class="col-span-4 text-gray-700 dark:text-gray-400">
            <div>{pathInfo.pretty}</div>
          </label>
          <input
            id={pathInfo.id}
            type="checkbox"
            value={pathInfo.id}
            name="expand-list"
            class="appearance-none w-4 h-4 border-2 border-gray-300 rounded-full checked:bg-green-600 checked:border-green-600 focus:ring-green-500 focus:ring-2 transition-colors cursor-pointer"
            checked={selectedPathId === pathInfo.id}
            onchange={() => handlePathSelect(pathInfo)}
          />
        </li>
      {/each}
      </ul>
  </div>
</section>
<!-- For debugging -->
<!-- <button onclick={doLayout}>Do layout</button>
<button onclick={doLayoutAndFitView}>Do layout and fit view</button> -->
