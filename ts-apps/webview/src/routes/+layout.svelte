<!-- New to Svelte? See https://svelte.dev/tutorial/kit/layouts for how +layout.svelte is supposed to work -->

<script lang="ts">
  // The DLV lib exports a style.css for the Ladder diagram
  // This stylesheet is required for the Ladder diagram to render correctly
  import '@repo/decision-logic-visualizer/dist/style.css'
  let { children } = $props()
</script>

{@render children()}
