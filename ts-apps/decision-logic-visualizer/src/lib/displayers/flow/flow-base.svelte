<!-- This is an 'internal' component.
 The only reason why we need to wrap this in a parent component
 is because the SvelteFlow lib requires that any use of SF hooks happen
 in a component that descends from a component that initializes SvelteFlowProvider -->
<script lang="ts">
  import type { LirId } from '$lib/layout-ir/core.js'
  import { LirContext } from '$lib/layout-ir/core.js'
  import {
    type LadderLirNode,
    isNNFLadderGraphLirNode,
  } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import type { BaseLadderFlowDisplayerProps } from './flow-props.js'
  import {
    type LadderSFNodeWithDims,
    type LadderSFGraph,
    type LadderSFNode,
    sfNodeTypes,
    sfEdgeTypes,
    getSFNodeId,
  } from './svelteflow-types.js'
  import { ladderGraphToSFGraph } from './ladder-lir-to-sf.js'
  import PathsList from '../paths-list.svelte'

  import { Collapsible } from 'bits-ui'
  import List from 'lucide-svelte/icons/list'
  import dagre from '@dagrejs/dagre'
  import { getLayoutedElements, type DagreConfig } from './layout.js'
  import {
    SvelteFlow,
    Background,
    Controls,
    ControlButton,
    ConnectionLineType,
    useNodesInitialized,
    useSvelteFlow,
  } from '@xyflow/svelte'
  import * as SF from '@xyflow/svelte'
  import { onMount } from 'svelte'
  import { Debounced, watch } from 'runed'

  import '@xyflow/svelte/dist/style.css' // TODO: Prob remove this

  /************************
       Lir
  *************************/

  const { context, node }: BaseLadderFlowDisplayerProps = $props()

  /** `node` is reactive (because props are implicitly reactive),
   * but `declLirNode` is not.
   * So, if you want to render a new declLirNode,
   * you'll need to destroy and re-mount the LadderFlow displayer.
   *
   * We could also work with the reactive `node` and update the sf graph
   * whenever `node` changes --- I'm not sure offhand which is better.
   * This was just the simpler route given what I already have.
   */
  const funDeclLirNode = node
  const ladderEnv = useLadderEnv()
  const lir = ladderEnv.getLirRegistry()

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
  const ladderGraph = funDeclLirNode.getBody(context)
  const initialSfGraph = ladderGraphToSFGraph(context, ladderGraph)

  // SvelteFlow nodes and edges variables
  let NODES = $state.raw<LadderSFGraph['nodes']>(initialSfGraph.nodes)
  let EDGES = $state.raw<LadderSFGraph['edges']>(initialSfGraph.edges)
  // $inspect(NODES)

  // SfId, LirId helpers
  let sfIdToLirId = initialSfGraph.sfIdToLirId
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  let lirIdToSfId = initialSfGraph.lirIdToSFId
  const sfNodeToLirId = (sfNode: LadderSFNode) => {
    return sfIdToLirId(getSFNodeId(sfNode))
  }

  // PathsList
  let pathsList = $state(
    isNNFLadderGraphLirNode(ladderGraph)
      ? ladderGraph.getPathsList(context)
      : null
  )

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

  // Keep track of whether nodes have been layouted and fit to view, so that won't display them before then
  let nodes$AreLayoutedAndFitToView = $state(false)
  // $inspect('nodes layouted', nodes$AreLayouted)
  const flowOpacity = $derived(nodes$AreLayoutedAndFitToView ? 1 : 0)
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
        updateResultDisplay()
      }
    )

    const unsubs = [
      lir.subscribe(onLadderGraphNonPositionalChange),
      lir.subscribe(onPathsListChange),
    ]

    /** Clean up when component is destroyed.
     *
     * Why is this necessary? One simple reason has to do with the LirNodes and what happens when the visualization command is run.
     * - For something to be eligible for garbage collection, it must not be reachable from a GC root.
     * - The visualizer is structured so that there's a LirContext, with a mapping from LirIds to LirNodes,
     *   that persists through, e.g., changes in the visualization calls from the language server.
     *   In particular, this mapping from LirIds to LirNodes persists even when the LadderFlow component
     *   is destroyed and re-created (which is what happens every time the 'visualize L4' LSP command is run).
     * - So, when destroying a LaddderFlow component,
     *   if we don't remove references to LirNodes that were used in the component from the LirContext,
     *   those LirNodes will not be eligible for garbage collection.
     *   I.e., every time you (e.g.) run the visualize L4 command, you'd be accumulating LirNodes in memory that will never be GC'd.
     * - (Similar considerations might also apply, mutatis mutandis, to other actions that create LirNodes.)
     *
     * For future work: I've checked, via the Chrome memory profiler, that this seems to be making a difference
     * when it comes to whether certain LirNodes stick around, but I haven't checked this for *every* potential LirNode.
     * In particular, I might need to do more when it comes to the PathsList and PathLirNodes.
     */
    return () => {
      funDeclLirNode.dispose(context)
      unsubs.forEach((unsub) => unsub.unsubscribe())
    }
  })

  /*************************************
      Other SvelteFlow event listeners
  ***************************************/

  const onNodeDragStop: SF.NodeTargetEventWithPointer<
    MouseEvent | TouchEvent
  > = (event) => {
    if (event.targetNode) {
      const lirNode = context.get(
        sfNodeToLirId(event.targetNode as LadderSFNode)
      ) as LadderLirNode
      lirNode.setPosition(context, event.targetNode.position)
    }
  }

  /***********************************
            Result UI
  ************************************/

  let resultMessage: string = $state('')
  function updateResultDisplay() {
    resultMessage = `evaluates to ${ladderGraph.getResult(context).toPretty()} (what-if mode)`
  }

  /*********************************************
        LadderGraph event listener
  **********************************************/

  /**
   * Most naive version: When a non-positional change occurs in the LadderGraphLirNode,
   * we generate and re-render the SF graph.
   *
   *  Assumptions:
   *  - The id of the LadderGraphLirNode is stable / the same throughout the lifetime of this component.
   *  - The LadderGraphLirNode does NOT publish position changes (may revisit this in the future)
   *  - The LadderGraphLirNode has all the info we need to render the SF graph; in particular,
   *    it is up to date with any changes to the graph.
   */
  const onLadderGraphNonPositionalChange = (context: LirContext, id: LirId) => {
    if (id === ladderGraph.getId()) {
      const newSfGraph = ladderGraphToSFGraph(context, ladderGraph)
      NODES = newSfGraph.nodes
      EDGES = newSfGraph.edges
      sfIdToLirId = newSfGraph.sfIdToLirId
      lirIdToSfId = newSfGraph.lirIdToSFId
      // console.log('newSfGraph NODES', NODES)

      updateResultDisplay()
    }
  }

  /*********************************************
        PathsList event listener
  **********************************************/
  const onPathsListChange = (context: LirContext, id: LirId) => {
    if (
      isNNFLadderGraphLirNode(ladderGraph) &&
      pathsList &&
      id === pathsList.getId()
    ) {
      pathsList = ladderGraph.getPathsList(context)
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
        const lirNode = context.get(sfNodeToLirId(sfNode)) as LadderLirNode
        lirNode.setPosition(context, sfNode.position)
        lirNode.setDimensions(context, {
          width: sfNode.measured.width,
          height: sfNode.measured.height,
        })
      })
    }
  }

  function doFitView() {
    fitView({
      padding: 0.1,
      minZoom: sfVisualOptions.smallestThatCanZoomOutTo,
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
  }

  function doLayoutAndFitView() {
    doLayout()
    // requestAnimationFrame in order to schedule the fit view for *after* the layouting is done
    // There may be better ways to do this
    window.requestAnimationFrame(() => {
      doFitView()
      nodes$AreLayoutedAndFitToView = true
    })
  }
</script>

<!--
Misc SF UI TODOs:

* Make it clearer that the bool var nodes are clickable
(should at least change the cursor to a pointer on mouseover)
-->

<!-- The consumer containing div must set the height to, e.g., 96svh if that's what's wanted -->
<div class="overall-container">
  <h1>{funDeclLirNode.getFunName(context)}</h1>
  <h2>{resultMessage}</h2>
  <div
    class="flow-container transition-opacity"
    style={`opacity: ${flowOpacity}`}
  >
    <SvelteFlow
      bind:nodes={NODES}
      bind:edges={EDGES}
      nodeTypes={sfNodeTypes}
      edgeTypes={sfEdgeTypes}
      minZoom={sfVisualOptions.smallestThatCanZoomOutTo}
      fitView
      connectionLineType={ConnectionLineType.Bezier}
      defaultEdgeOptions={{ type: 'bezier', animated: false }}
      onnodedragstop={onNodeDragStop}
    >
      <!-- disabling show lock because it didn't seem to do anything for me --- might need to adjust some other setting too -->
      <Controls position="bottom-right" showLock={false}>
        <ControlButton onclick={() => ladderGraph.toggleZenModeStatus(context)}>
          <!-- TODO: Make our own menu to get more real estate and use a Switch component -->
          <div class="text-[0.7rem] p-1">Zen</div>
        </ControlButton>
      </Controls>
      <Background />
    </SvelteFlow>
  </div>
  <!-- Paths Section -->
  <!-- TODO: Move the following into a lin paths container component -->
  {#if pathsList && isNNFLadderGraphLirNode(ladderGraph)}
    <div class="paths-container">
      <!-- TODO: Make a standalone wrapper over the collapsible component, as suggested by https://bits-ui.com/docs/components/collapsible  -->
      <!-- Using setTimeout instead of window requestAnimationFrame because it can take time to generate the paths list the first time round -->
      <Collapsible.Root onOpenChange={() => setTimeout(doFitView, 10)}>
        <Collapsible.Trigger class="flex items-center justify-end w-full gap-2">
          <!-- TODO: Improve the button styles -->
          <button
            class="rounded-md border-1 border-sky-700 px-2 py-1 text-xs hover:bg-accent flex items-center gap-1"
          >
            <List /><span>List paths</span>
          </button>
        </Collapsible.Trigger>
        <Collapsible.Content class="pt-2">
          <PathsList {context} node={pathsList} {ladderGraph} />
        </Collapsible.Content>
      </Collapsible.Root>
    </div>
  {/if}
</div>

<!-- For debugging -->
<!-- <button onclick={doLayout}>Do layout</button>
<button onclick={doLayoutAndFitView}>Do layout and fit view</button> -->

<style lang="postcss">
  @reference 'tailwindcss';
  /* Would be better to reference our stylesheet so we can use our theme vars if necessary,
  but there are complications that have to do with how the stylesheet is exported for consumers.
  Maybe the thing to do in the future is to have a common theme stylesheet that is shared among the lib and consumers? */

  h1 {
    @apply text-2xl font-semibold text-center mt-1;
  }

  h2 {
    @apply text-lg text-center -mt-2;
  }

  .overall-container {
    display: flex;
    flex-direction: column;
    height: 100%;
    /* Gap between the flow and the paths list container */
    row-gap: 6px;
  }

  .flow-container {
    flex: 1 1 auto;
    min-height: 0; /* Prevents overflow */
  }

  .paths-container {
    flex: 0 0 auto;
    max-height: 45%;
    overflow-y: auto;
    padding-bottom: 6px;
  }
</style>
