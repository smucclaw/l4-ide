<!-- For adding a context menu to a node that can be selected for highlighting -->
<script lang="ts" module>
  import { onDestroy, type Snippet } from 'svelte'
  import type { LirContext, LirId } from '$lib/layout-ir/core'
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
    node.toggleSelection(context, nodeSelectionTracker, ladderGraph)
  }

  // Selected state and updates thereof
  let selected = $state(node.isSelected(context, nodeSelectionTracker))
  const onSelectionChange = (context: LirContext, id: LirId) => {
    if (id === node.getId()) {
      selected = node.isSelected(context, nodeSelectionTracker)
      console.log('onSelectionChange', selected)
    }
  }

  const unsub = ladderEnv.getLirRegistry().subscribe(onSelectionChange)
  onDestroy(() => unsub.unsubscribe())
</script>

<ContextMenu.Root>
  <ContextMenu.Trigger>
    <div class={selected ? 'highlighted-ladder-node' : ''}>
      {@render children()}
    </div>
  </ContextMenu.Trigger>
  <div class="text-[0.625rem]">
    <ContextMenu.Content>
      <!-- We can consider using a CheckboxItem in the future -->
      <ContextMenu.Item {onSelect}>Toggle Highlight</ContextMenu.Item>
    </ContextMenu.Content>
  </div>
</ContextMenu.Root>
