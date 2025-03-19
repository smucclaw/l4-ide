<script lang="ts">
  import { onDestroy } from 'svelte'
  import type { PathListDisplayerProps } from './svelteflow-types.js'
  import { Toggle } from '$lib/ui-primitives/toggle/index.js'

  /************************
       Lir
  *************************/

  const { context, node }: PathListDisplayerProps = $props()

  onDestroy(() => {
    node.dispose(context)
  })
</script>

<section class="paths-list-content-wrapper">
  <ul class="space-y-1">
    {#each node.getPaths(context) as path, pathIndex}
      <li class="grid grid-cols-[max-content_1fr] gap-x-2 items-center">
        <!-- Row number / path index -->
        <div class="px-3 max-w-[25px] text-right">
          {pathIndex + 1}
        </div>
        <Toggle
          pressed={path.is$Selected()}
          class="rounded-lg border-1 p-2 max-w-fit hover:bg-accent text-xs text-left"
          onPressedChange={(pressed: boolean) => {
            if (pressed) {
              path.highlightCorrespondingPathInLadderGraph(context)
            } else {
              path.unhighlightCorrespondingPathInLadderGraph(context)
            }
          }}
        >
          {path.toPretty(context)}
        </Toggle>
      </li>
    {/each}
  </ul>
</section>

<style>
  .paths-list-content-wrapper {
    margin-left: auto;
    margin-right: auto;
    max-width: 65ch;
  }
</style>
