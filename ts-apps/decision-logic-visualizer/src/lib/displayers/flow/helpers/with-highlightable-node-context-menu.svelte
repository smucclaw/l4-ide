<!-- For adding a context menu to a node that can be selected for highlighting -->
<script lang="ts" module>
  import type { Snippet } from 'svelte'

  interface HighlightableNodeContextMenuProps {
    children: Snippet
  }
</script>

<script lang="ts">
  import * as ContextMenu from '$lib/ui-primitives/context-menu/index.js'

  let { children }: HighlightableNodeContextMenuProps = $props()
</script>

<ContextMenu.Root>
  <ContextMenu.Trigger>
    {@render children()}
  </ContextMenu.Trigger>
  <ContextMenu.Content>
    <ContextMenu.Item inset>Highlight</ContextMenu.Item>
  </ContextMenu.Content>
</ContextMenu.Root>
