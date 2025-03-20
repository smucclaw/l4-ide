<script lang="ts">
  import { onDestroy } from 'svelte'
  import type { PathListDisplayerProps } from './svelteflow-types.js'
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

<section class="paths-list-content-wrapper">
  <ToggleGroup
    type="multiple"
    onValueChange={(value: string[]) => {
      const selectedPathLirIds = value
        .map((v) => parseInt(v)) // the `value` for this component must be a string
        .map((pathIndex) => paths[pathIndex].getId())
      console.log('onvalueChange', value)
      console.log('onValueChange', selectedPathLirIds)
      pathsListLirNode.highlightPaths(context, selectedPathLirIds)
    }}
  >
    <ul class="space-y-1">
      {#each paths as path, pathIndex}
        <li class="grid grid-cols-[max-content_1fr] gap-x-2 items-center">
          <!-- Row number / path index -->
          <div class="px-3 max-w-[25px] text-right">
            {pathIndex + 1}
          </div>
          <ToggleGroupItem
            value={`${pathIndex}`}
            class="rounded-lg border-1 p-2 max-w-fit hover:border-sky-700 hover:bg-stone-50 text-xs text-left"
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
