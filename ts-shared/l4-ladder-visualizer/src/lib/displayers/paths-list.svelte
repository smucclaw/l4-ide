<script lang="ts">
  import { onDestroy } from 'svelte'
  import type { PathListDisplayerProps } from './non-flow-props.js'
  import {
    ToggleGroup,
    ToggleGroupItem,
  } from '$lib/ui-primitives/toggle-group/index.js'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import type { LinPathLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import type { LirContext, LirId } from '$lib/layout-ir/core.js'

  /************************
       Lir
  *************************/

  const {
    context,
    node: pathsListLirNode,
    ladderGraph,
  }: PathListDisplayerProps = $props()

  const paths = pathsListLirNode.getPaths(context)

  /** Key state: What lin paths in the paths list are selected */
  let selectedPaths: LinPathLirNode[] = $state(
    pathsListLirNode.getSelectedPaths(context)
  )
  const onSelectedPathsChange = (context: LirContext, id: LirId) => {
    if (id === pathsListLirNode.getId()) {
      selectedPaths = pathsListLirNode.getSelectedPaths(context)
    }
  }
  const unsub = useLadderEnv().getLirRegistry().subscribe(onSelectedPathsChange)

  onDestroy(() => {
    pathsListLirNode.dispose(context)
    unsub.unsubscribe()
  })

  // Synchronize state in the PathsListLirNode with state in the concrete UI (the ToggleGroup)
  const pathLirIdToPathIndex = new Map(
    paths.map((p, idx) => [p.getId(), idx.toString()])
  )
  function getSelectedPaths() {
    // console.log(
    //   'getSelectedPaths',
    //   selectedPaths.map((p) => pathLirIdToPathIndex.get(p.getId()) as string)
    // )
    return selectedPaths.map(
      (p) => pathLirIdToPathIndex.get(p.getId()) as string
    )
  }
  function setSelectedPaths(toggleGroupPathIndices: string[]) {
    const selectedLinPaths = toggleGroupPathIndices
      .map((v) => parseInt(v)) // the `value` for this component must be a string
      .map((pathIndex) => paths[pathIndex])

    // Update state in the LadderNodeSelectionTracker with the new selected lin paths
    // (This in turn triggers updates to the derived state / projections)
    ladderGraph.selectNodes(
      context,
      selectedLinPaths.flatMap((p) => p.getSelectableVertices(context))
    )
  }
</script>

<section class="paths-list-content-wrapper">
  <ToggleGroup type="multiple" bind:value={getSelectedPaths, setSelectedPaths}>
    <ul class="space-y-1">
      {#each paths as path, pathIndex}
        <li class="grid grid-cols-[max-content_1fr] gap-x-3 items-center">
          <!-- Row number / path index -->
          <div class="px-2 max-w-[25px] text-right">
            {pathIndex + 1}
          </div>
          <!-- Why h-full: so that height of rows can increase to fit content when, e.g., browser window is made narrower -->
          <ToggleGroupItem
            value={`${pathIndex}`}
            class="rounded-lg max-w-fit border-1 data-[state=on]:border-2 p-2 h-full data-[state=on]:border-sky-600 hover:border-sky-800 text-xs text-left break-words"
          >
            {path.toPretty(context)}
          </ToggleGroupItem>
        </li>
      {/each}
    </ul>
  </ToggleGroup>
</section>

<style>
  .paths-list-content-wrapper {
    margin-left: auto;
    margin-right: auto;
    max-width: 65ch;
  }
</style>
