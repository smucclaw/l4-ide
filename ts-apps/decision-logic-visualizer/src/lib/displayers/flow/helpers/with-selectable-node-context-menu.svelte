<!-- For adding a context menu to a node that can be selected for highlighting -->
<script lang="ts" module>
  import type { Snippet } from 'svelte'
  import type { LirContext } from '$lib/layout-ir/core'
  import type { LadderNodeSelectionTracker } from '$lib/layout-ir/paths-list.js'
  import type { SelectableLadderLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'

  interface SelectableNodeContextMenuProps {
    context: LirContext
    node: SelectableLadderLirNode
    nodeSelectionTracker: LadderNodeSelectionTracker
    children: Snippet
  }
</script>

<script lang="ts">
  import * as ContextMenu from '$lib/ui-primitives/context-menu/index.js'
  import { useLadderEnv } from '$lib/ladder-env'

  let {
    context,
    node,
    nodeSelectionTracker,
    children,
  }: SelectableNodeContextMenuProps = $props()

  const ladderEnv = useLadderEnv()
  const ladderGraph = ladderEnv.getTopFunDeclLirNode(context).getBody(context)
  const onSelect = () => {
    nodeSelectionTracker.toggleNodeSelection(context, node, ladderGraph)
  }
</script>

<ContextMenu.Root>
  <ContextMenu.Trigger>
    {@render children()}
  </ContextMenu.Trigger>
  <div class="text-[0.625rem]">
    <ContextMenu.Content>
      <!-- We can consider using a CheckboxItem in the future -->
      <ContextMenu.Item {onSelect}>Toggle Highlight</ContextMenu.Item>
    </ContextMenu.Content>
  </div>
</ContextMenu.Root>
