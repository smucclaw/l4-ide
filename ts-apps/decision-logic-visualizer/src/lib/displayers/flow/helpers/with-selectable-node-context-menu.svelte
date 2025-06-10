<!-- For adding a context menu to a node that can be selected for highlighting -->
<script lang="ts" module>
  import { type Snippet } from 'svelte'
  import type { LirContext } from '$lib/layout-ir/core'
  import type {
    SelectableLadderLirNode,
    NNFLadderGraphLirNode,
  } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'

  interface SelectableNodeContextMenuProps {
    context: LirContext
    node: SelectableLadderLirNode
    ladderGraph: NNFLadderGraphLirNode
    onSelect?: () => void
    children: Snippet
  }
</script>

<script lang="ts">
  import * as ContextMenu from '$lib/ui-primitives/context-menu/index.js'

  const defaultOnSelect = () => {
    ladderGraph.toggleNodeSelection(context, node)
  }

  let {
    context,
    node,
    ladderGraph,
    onSelect = defaultOnSelect,
    children,
  }: SelectableNodeContextMenuProps = $props()
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
