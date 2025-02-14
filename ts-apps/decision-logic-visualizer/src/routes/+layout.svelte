<!-- New to Svelte? See https://svelte.dev/tutorial/kit/layouts for how +layout.svelte is supposed to work -->

<script lang="ts">
  import '../style.css'
  let { children } = $props()
</script>

{@render children()}
