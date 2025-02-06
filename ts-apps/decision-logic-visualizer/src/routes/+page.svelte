<script lang="ts">
  import { SvelteFlowProvider } from '@xyflow/svelte'
  import { Either, Schema } from 'effect'
  import { IRExpr, ExprLirSource } from '$lib/data/program-ir'
  import { LirContext, LirRegistry, type LirRootType } from '$lib/layout-ir'

  import ExprFlow from '$lib/displayers/flow/flow.svelte'

  // Parse JSON object into IRExpr
  const example1 = {
    $type: 'BinExpr' as const,
    op: 'And' as const,
    left: {
      $type: 'BinExpr' as const,
      op: 'Or' as const,
      left: {
        $type: 'BoolVar' as const,
        value: 'True' as const,
        id: { id: 1 },
        name: 'eats',
      },
      right: {
        $type: 'BoolVar' as const,
        value: 'Unknown' as const,
        id: { id: 2 },
        name: 'walks',
      },
      id: { id: 3 },
    },
    right: {
      $type: 'BoolVar' as const,
      value: 'True' as const,
      id: { id: 4 },
      name: 'swims',
    },
    id: { id: 5 },
  }

  const decode = Schema.decodeUnknownEither(IRExpr)
  const eitherExpr = decode(example1)
  let expr: IRExpr
  if (Either.isRight(eitherExpr)) {
    expr = eitherExpr.right
  } else {
    // not sure how to just unsafely coerce, so whatever
    console.error('Decoding failed:', eitherExpr.left)
    expr = {
      $type: 'BoolVar',
      value: 'True',
      id: { id: 1 },
      name: 'decoding somehow failed??!?!?!?!',
    }
  }

  // Set up Lir
  const registry = new LirRegistry()
  const context = new LirContext()
  const nodeInfo = { registry, context }

  const exprLirNode = ExprLirSource.toLir(nodeInfo, expr)
  // console.log(exprLirNode)
  registry.setRoot(context, 'EXAMPLE_1' as LirRootType, exprLirNode)

  // style
  let isHovered = false
</script>

<h1 class="text-4xl font-bold text-center">Decision Logic Visualizer Draft</h1>
<h2 class="text-2xl italic text-center text-gray-700">
  Examples of decision logic visualizations, starting from a 'json' of the
  IRExpr that eventually gets transformed into a SvelteFlow graph
</h2>
<section id="example 1" class="example w-3/4 mx-auto space-y-4">
  <h3 class="text-2xl font-semibold">Example 1</h3>
  <SvelteFlowProvider>
    <ExprFlow {context} node={exprLirNode} />
  </SvelteFlowProvider>
  <section class="json-visualisation space-y-2">
    <input type="checkbox" id="json-toggle" class="peer hidden" />

    <label 
      for="json-toggle"
      class="inline-flex w-fit cursor-pointer p-2 rounded-lg transition-colors"
      on:mouseover={() => isHovered = true}
      on:mouseout={() => isHovered = false}
      on:focus={() => isHovered = true}
      on:blur={() => isHovered = false}
      style="background-color: {isHovered ? 'var(--color-button-hover)' : 'var(--color-button)'}"
      >
      <h4>Expand to view source JSON of the IRExpr</h4>
    </label>

    <pre class="max-h-0 overflow-hidden peer-checked:max-h-[500px] transition-[max-height] duration-300 ease-in-out bg-gray-100 p-2 rounded-md"><code>
      {JSON.stringify(example1, null, 2)}
    </code></pre>
  </section>
</section>
