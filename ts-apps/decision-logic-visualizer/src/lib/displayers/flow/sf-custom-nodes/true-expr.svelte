<script lang="ts">
  import { TrueExprLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import type { LadderNodeDisplayerProps } from '../svelteflow-types.js'
  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'
  import WithNonBundlingNodeBaseStyles from '$lib/displayers/flow/helpers/with-non-bundling-node-base-styles.svelte'
  import ValueIndicator from '$lib/displayers/flow/helpers/value-indicator.svelte'
  import IsViableIndicator from '$lib/displayers/flow/helpers/is-viable-indicator.svelte'

  let { data }: LadderNodeDisplayerProps = $props()
</script>

<!-- select-none to prevent text selection, to hint that this is a constant.
 Also no cursor-pointer -->
<WithNonBundlingNodeBaseStyles>
  <IsViableIndicator context={data.context} node={data.node}>
    <ValueIndicator
      value={(data.node as TrueExprLirNode).getValue(data.context)}
      additionalClasses={['bool-lit-node-border', 'select-none']}
    >
      <WithNormalHandles>
        <div class="label-wrapper-for-content-bearing-sf-node">
          {(data.node as TrueExprLirNode).toPretty(data.context)}
        </div>
      </WithNormalHandles>
    </ValueIndicator>
  </IsViableIndicator>
</WithNonBundlingNodeBaseStyles>
