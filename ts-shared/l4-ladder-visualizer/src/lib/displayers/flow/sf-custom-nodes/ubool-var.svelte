<!-- Adopted from the SF DefaultNode implementation
https://github.com/xyflow/xyflow/blob/migrate/svelte5/packages/svelte/src/lib/components/nodes/DefaultNode.svelte
-->
<script lang="ts">
  import type { LadderNodeDisplayerProps } from '../svelteflow-types.js'
  import { useLadderEnv } from '$lib/ladder-env.js'
  import {
    isNNFLadderGraphLirNode,
    UBoolVarLirNode,
  } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import * as Tooltip from '$lib/ui-primitives/tooltip/index.js'
  import { cycle } from '$lib/eval/type.js'
  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'
  import WithNonBundlingNodeBaseStyles from '$lib/displayers/flow/helpers/with-non-bundling-node-base-styles.svelte'
  import WithSelectableNodeContextMenu from '$lib/displayers/flow/helpers/with-selectable-node-context-menu.svelte'
  import IsViableIndicator from '$lib/displayers/flow/helpers/is-viable-indicator.svelte'
  import ValueIndicator from '$lib/displayers/flow/helpers/value-indicator.svelte'

  let { data }: LadderNodeDisplayerProps = $props()

  // Get LadderEnv, L4 Connection, nodeSelectionTracker
  // (Note: useLadderEnv must be invoked on component initalization)
  const ladderEnv = useLadderEnv()
  const ladderGraph = ladderEnv
    .getTopFunDeclLirNode(data.context)
    .getBody(data.context)
  const l4Conn = ladderEnv.getL4Connection()

  const node = data.node as UBoolVarLirNode

  const maybeHighlightedStyle = $derived(
    isNNFLadderGraphLirNode(ladderGraph) &&
      ladderGraph.nodeIsSelected(data.context, data.node as UBoolVarLirNode)
      ? 'highlighted-ladder-node'
      : ''
  )

  const maybeElicitationStyle = $derived.by((): string => {
    const analysis = ladderGraph.getPartialEvalAnalysis(data.context)
    if (!analysis) return ''
    const u = node.getUnique(data.context)
    const rankedIndex = analysis.ranked.indexOf(u)
    if (rankedIndex === 0) return 'elicitation-next'
    if (rankedIndex > 0) return 'elicitation-needed'
    if (analysis.stillNeeded.includes(u)) return 'elicitation-needed'
    return ''
  })
</script>

{#snippet inlineUI()}
  <div class="absolute bottom-1 right-1">
    <Tooltip.Root>
      <Tooltip.Trigger>
        <button
          aria-label="Unfold to definition"
          class="px-0.5 text-[0.625rem] rounded border border-border bg-background hover:bg-accent hover:text-accent-foreground transition-colors duration-150"
          onclick={() => {
            console.log('inline lir id', node.getId())

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

{#snippet coreUBoolVarUI()}
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
    {node.getLabel(data.context)}
  </button>
{/snippet}

<!-- Need to use data.bleh to maintain reactivity -- can't, e.g., do `const bleh = data.bleh` 
TODO: Look into why this is the case --- are they not re-mounting the ubool-var component? 
-->

<WithNonBundlingNodeBaseStyles>
  <IsViableIndicator context={data.context} node={data.node}>
    <ValueIndicator
      value={node.getValue(data.context, ladderGraph)}
      additionalClasses={[
        // It's easier if the highlighted border styles are on the same element as the normal border styles.
        'ubool-var-node-border',
        maybeElicitationStyle,
        // TODO: This could prob be cleaner.
        maybeHighlightedStyle,
      ]}
    >
      <WithNormalHandles>
        {#if isNNFLadderGraphLirNode(ladderGraph)}
          <WithSelectableNodeContextMenu
            context={data.context}
            node={data.node as UBoolVarLirNode}
            {ladderGraph}
          >
            {@render coreUBoolVarUI()}
          </WithSelectableNodeContextMenu>
        {:else}
          {@render coreUBoolVarUI()}
        {/if}
        {#if node.canInline(data.context)}
          {@render inlineUI()}
        {/if}
      </WithNormalHandles>
    </ValueIndicator>
  </IsViableIndicator>
</WithNonBundlingNodeBaseStyles>
