<!-- Wrapper that provides the base styles for non-bundling nodes -->
<script lang="ts" module>
  import type { Snippet } from 'svelte'

  interface WithNonBundlingNodeBaseStylesProps {
    children: Snippet
  }
</script>

<script lang="ts">
  let { children }: WithNonBundlingNodeBaseStylesProps = $props()
</script>

<div class="base-sf-node-styles transition-opacity duration-300">
  {@render children()}
</div>
