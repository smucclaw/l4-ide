<script lang="ts">
  import { TrueExprLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import type { BoolLitDisplayerProps } from '../svelteflow-types.js'
  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'
  import WithContentfulNodeStyles from '$lib/displayers/flow/helpers/with-contentful-node-styles.svelte'
  import ValueIndicator from '$lib/displayers/flow/helpers/value-indicator.svelte'

  let { data }: BoolLitDisplayerProps = $props()
</script>

<!-- select-none to prevent text selection, to hint that this is a constant.
 Also no cursor-pointer -->
<WithContentfulNodeStyles>
  <ValueIndicator
    value={(data.context.get(data.originalLirId) as TrueExprLirNode).getValue(
      data.context
    )}
    additionalClasses={['bool-lit-node-border', 'select-none', ...data.classes]}
  >
    <WithNormalHandles>
      <div class="label-wrapper-for-content-bearing-sf-node">
        {(data.context.get(data.originalLirId) as TrueExprLirNode).toPretty(
          data.context
        )}
      </div></WithNormalHandles
    >
  </ValueIndicator>
</WithContentfulNodeStyles>
