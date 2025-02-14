<!-- New to Svelte? See https://svelte.dev/tutorial/kit/layouts for how +layout.svelte is supposed to work -->

<script lang="ts">
  // it was using the app.css from the webview repo and not the decisionlogicvisualizer
  // for now we're referencing it from decision-logic-visualizer
  import '@repo/decision-logic-visualizer/src/app.css'
  let { children } = $props()
</script>

{@render children()}
