<script lang="ts">
  import { onDestroy } from 'svelte'
  import type { PathListDisplayerProps } from './non-flow-props.js'
  import {
    ToggleGroup,
    ToggleGroupItem,
  } from '$lib/ui-primitives/toggle-group/index.js'

  /************************
       Lir
  *************************/

  const { context, node: pathsListLirNode }: PathListDisplayerProps = $props()

  onDestroy(() => {
    pathsListLirNode.dispose(context)
  })

  const paths = pathsListLirNode.getPaths(context)
</script>

<!-- TODO: Make this a 'controlled' component / make the selected vs non-selected state a view of that in a more centralized location -->

<section class="paths-list-content-wrapper">
  <ToggleGroup
    type="multiple"
    onValueChange={(value: string[]) => {
      const selectedPaths = value
        .map((v) => parseInt(v)) // the `value` for this component must be a string
        .map((pathIndex) => paths[pathIndex])
      console.log('onvalueChange', value)
      console.log('onValueChange', selectedPaths)
      pathsListLirNode.highlightPaths(context, selectedPaths)
    }}
  >
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
