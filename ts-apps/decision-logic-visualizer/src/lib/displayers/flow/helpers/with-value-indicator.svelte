<!-- A non-color-based indicator of the value,
 along with the color-specific bg color
 and the border styles for the caller node -->
<script lang="ts" module>
  import type { UBoolVal } from '$lib/eval/type.js'
  import type { Snippet } from 'svelte'
  import { match } from 'ts-pattern'

  interface WithValueIndicatorProps {
    value: UBoolVal
    borderClasses: string[]
    children: Snippet
  }
</script>

<script lang="ts">
  let { value, borderClasses, children }: WithValueIndicatorProps = $props()
</script>

<!-- Need the parent to be relatively positioned,
 for the absolute positioning to work -->
<div class={['relative', ...borderClasses, ...value.getClasses()]}>
  {@render children()}
  <div
    class="absolute top-0 left-1/2 -translate-x-1/2 -translate-y-1/2
    w-4 text-[0.7rem] px-1
    text-white bg-cyan-800
    rounded-full"
  >
    {match(value)
      .with({ $type: 'TrueV' }, () => '✓')
      .with({ $type: 'FalseV' }, () => '✗')
      .with({ $type: 'UnknownV' }, () => null)
      .exhaustive()}
  </div>
</div>
