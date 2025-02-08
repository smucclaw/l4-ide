<script lang="ts">
  import { SvelteFlowProvider } from '@xyflow/svelte'
  import { Either, Schema } from 'effect'
  import { IRExpr } from '@repo/viz-expr'
  import { ExprLirSource } from '$lib/data/viz-expr-to-lir.js'
  import {
    LirContext,
    LirRegistry,
    type LirRootType,
  } from '$lib/layout-ir/core.js'

  import ExprFlow from '$lib/displayers/flow/flow.svelte'

  // Parse JSON object into IRExpr
  const example1 = {
    $type: 'And' as const,
    args: [
      {
        $type: 'Or' as const,
        args: [
          {
            $type: 'BoolVar' as const,
            value: 'True' as const,
            id: { id: 1 },
            name: 'eats',
          },
          {
            $type: 'BoolVar' as const,
            value: 'Unknown' as const,
            id: { id: 2 },
            name: 'walks',
          },
        ],
        id: { id: 3 },
      },
      {
        $type: 'BoolVar' as const,
        value: 'True' as const,
        id: { id: 4 },
        name: 'swims',
      },
    ],
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
</script>

<h1>Decision Logic Visualizer draft</h1>
<h2>
  Examples of decision logic visualizations, starting from a 'json' of the
  IRExpr that eventually gets transformed into a SvelteFlow graph
</h2>
<h3>Example 1</h3>
<SvelteFlowProvider>
  <ExprFlow {context} node={exprLirNode} />
</SvelteFlowProvider>
<section>
  <p>The above is a visualization of</p>
  <pre><code>
  {JSON.stringify(example1, null, 2)}
</code></pre>
</section>
