<!-- Wrapper that provides the base styles for contentful nodes -->
<script lang="ts" module>
  import type { Snippet } from 'svelte'

  interface WithContentfulNodeStylesProps {
    children: Snippet
  }
</script>

<script lang="ts">
  let { children }: WithContentfulNodeStylesProps = $props()
</script>

<div class="base-sf-node-styles transition-opacity duration-300">
  {@render children()}
</div>
