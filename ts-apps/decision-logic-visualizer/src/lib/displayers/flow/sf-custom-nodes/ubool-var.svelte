<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte
-->
<script lang="ts">
  import type { UBoolVarDisplayerProps } from '../svelteflow-types.js'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import { UBoolVarLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import * as Tooltip from '$lib/ui-primitives/tooltip/index.js'
  import { cycle } from '$lib/eval/type.js'
  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'
  import WithContentfulNodeStyles from '$lib/displayers/flow/helpers/with-contentful-node-styles.svelte'
  import WithHighlightableNodeContextMenu from '$lib/displayers/flow/helpers/with-highlightable-node-context-menu.svelte'
  import ValueIndicator from '$lib/displayers/flow/helpers/value-indicator.svelte'

  let { data }: UBoolVarDisplayerProps = $props()

  // Get LadderEnv, L4 Connection
  const ladderEnv = useLadderEnv()
  const ladderGraph = ladderEnv
    .getTopFunDeclLirNode(data.context)
    .getBody(data.context)
  const l4Conn = ladderEnv.getL4Connection()

  const node = data.context.get(data.originalLirId) as UBoolVarLirNode
</script>

{#snippet inlineUI()}
  <div class="absolute bottom-1 right-1">
    <Tooltip.Root>
      <Tooltip.Trigger>
        <button
          aria-label="Unfold to definition"
          class="px-0.5 text-[0.625rem] rounded border border-border bg-background hover:bg-accent hover:text-accent-foreground transition-colors duration-150"
          onclick={() => {
            console.log('inline lir id', data.originalLirId.toString())

            l4Conn.inlineExprs(
              [node.getUnique(data.context)],
              ladderEnv.getVersionedTextDocIdentifier()
            )
          }}
        >
          +
        </button>
      </Tooltip.Trigger>
      <Tooltip.Content>Unfold to definition</Tooltip.Content>
    </Tooltip.Root>
  </div>
{/snippet}

<!-- Need to use data.bleh to maintain reactivity -- can't, e.g., do `const bleh = data.bleh` 
TODO: Look into why this is the case --- are they not re-mounting the ubool-var component? 
-->

<WithContentfulNodeStyles>
  <ValueIndicator
    value={node.getValue(data.context, ladderGraph)}
    additionalClasses={['ubool-var-node-border', ...data.classes]}
  >
    <WithNormalHandles>
      <WithHighlightableNodeContextMenu>
        <!-- Yes, we need cursor-pointer here. -->
        <button
          class="label-wrapper-for-content-bearing-sf-node cursor-pointer"
          onclick={() => {
            const newValue = cycle(node.getValue(data.context, ladderGraph))
            ladderGraph.submitNewBinding(data.context, {
              unique: node.getUnique(data.context),
              value: newValue,
            })
          }}
        >
          {data.name.label}
        </button>
      </WithHighlightableNodeContextMenu>
      {#if data.canInline}
        {@render inlineUI()}
      {/if}
    </WithNormalHandles>
  </ValueIndicator>
</WithContentfulNodeStyles>
