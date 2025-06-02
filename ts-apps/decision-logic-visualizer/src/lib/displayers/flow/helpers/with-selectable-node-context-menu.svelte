<!-- For adding a context menu to a node that can be selected for highlighting -->
<script lang="ts" module>
  import type { Snippet } from 'svelte'
  import type { LirContext } from '$lib/layout-ir/core'
  import { PathsTracker } from '$lib/layout-ir/paths-list.js'
  import type { SelectableNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'

  interface SelectableNodeContextMenuProps {
    context: LirContext
    node: SelectableNode
    pathsTracker: PathsTracker
    children: Snippet
  }
</script>

<script lang="ts">
  import * as ContextMenu from '$lib/ui-primitives/context-menu/index.js'
  import { useLadderEnv } from '$lib/ladder-env'

  let {
    context,
    node,
    pathsTracker,
    children,
  }: SelectableNodeContextMenuProps = $props()

  const onSelect = () => {
    pathsTracker.toggleNodeSelection(
      context,
      node,
      useLadderEnv().getTopFunDeclLirNode(context).getBody(context)
    )
  }
</script>

<ContextMenu.Root>
  <ContextMenu.Trigger>
    {@render children()}
  </ContextMenu.Trigger>
  <ContextMenu.Content>
    <!-- We can consider using a CheckboxItem in the future -->
    <ContextMenu.Item inset {onSelect}>Toggle Highlight</ContextMenu.Item>
  </ContextMenu.Content>
</ContextMenu.Root>
