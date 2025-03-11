<script lang="ts">
  import { type PathListDisplayerProps } from './types.svelte.js'

  /************************
       Lir
  *************************/

  const { context, node }: PathListDisplayerProps = $props()
</script>

<section class="paths-list-content-wrapper">
  <ul class="space-y-1">
    {#each node.getPaths(context) as path, pathIndex}
      <li class="grid grid-cols-[max-content_1fr] gap-x-2 items-center">
        <!-- Row number / path index -->
        <div class="font-semibold px-3 max-w-[25px] text-right">
          {pathIndex + 1}
        </div>
        <!-- TODO: Refactor the hover CSS to use our css vars -->
        <button
          class="rounded-md border-1 p-2 max-w-fit hover:bg-sky-100 text-xs text-left"
          onmouseenter={() =>
            path.highlightCorrespondingPathInLadderGraph(context)}
          onmouseleave={() =>
            path.unhighlightCorrespondingPathInLadderGraph(context)}
        >
          {path.toPretty(context)}
        </button>
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
