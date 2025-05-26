<script lang="ts">
  import { FalseExprLirNode } from '$lib/layout-ir/ladder-graph/ladder.svelte.js'
  import type { UBoolVarDisplayerProps } from '../svelteflow-types.js'
  import WithNormalHandles from '$lib/displayers/flow/helpers/with-normal-handles.svelte'
  import WithContentfulNodeStyles from '$lib/displayers/flow/helpers/with-contentful-node-styles.svelte'

  let { data }: UBoolVarDisplayerProps = $props()
</script>

<!-- select-none to prevent text selection, to hint that this is a constant.
 Also no cursor-pointer -->
<WithContentfulNodeStyles>
  <div class={['bool-lit-node-border', 'select-none', ...data.classes]}>
    <WithNormalHandles>
      <div class="label-wrapper-for-content-bearing-sf-node">
        {(data.context.get(data.originalLirId) as FalseExprLirNode).toPretty(
          data.context
        )}
      </div>
    </WithNormalHandles>
  </div>
</WithContentfulNodeStyles>
