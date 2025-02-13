<script lang="ts">
  import { SvelteFlowProvider } from '@xyflow/svelte'
  import { Either, Schema } from 'effect'
  import { match } from 'ts-pattern'
  import { IRExpr } from '@repo/viz-expr'
  import { ExprLirSource } from '$lib/data/viz-expr-to-lir.js'
  import {
    LirContext,
    LirRegistry,
    type LirRootType,
  } from '$lib/layout-ir/core.js'

  import ExprFlow from '$lib/displayers/flow/flow.svelte'

  // TODO: This stuff should just be replaced with the tailwind on hovered classes
  let isHovered = $state(false)

  /***************************
      Example 1
  ****************************/

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
            name: 'walkswalks',
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

  /***************************
      Example 2
  ****************************/

  const example2 = {
    $type: 'And' as const,
    args: [
      {
        $type: 'Or' as const,
        args: [
          {
            $type: 'BoolVar' as const,
            value: 'False' as const,
            id: { id: 1 },
            name: 'flies',
          },
          {
            $type: 'BoolVar' as const,
            value: 'True' as const,
            id: { id: 2 },
            name: 'runs',
          },
          {
            $type: 'And' as const,
            args: [
              {
                $type: 'BoolVar' as const,
                value: 'Unknown' as const,
                id: { id: 3 },
                name: 'swims',
              },
              {
                $type: 'BoolVar' as const,
                value: 'True' as const,
                id: { id: 4 },
                name: 'dives',
              },
            ],
            id: { id: 5 },
          },
        ],
        id: { id: 6 },
      },
      {
        $type: 'BoolVar' as const,
        value: 'True' as const,
        id: { id: 7 },
        name: 'jumps',
      },
      {
        $type: 'BoolVar' as const,
        value: 'False' as const,
        id: { id: 8 },
        name: 'jogs',
      },
      {
        $type: 'BoolVar' as const,
        value: 'False' as const,
        id: { id: 9 },
        name: 'reads',
      },
      {
        $type: 'BoolVar' as const,
        value: 'True' as const,
        id: { id: 10 },
        name: 'writes',
      },
      {
        $type: 'Or' as const,
        args: [
          {
            $type: 'BoolVar' as const,
            value: 'Unknown' as const,
            id: { id: 11 },
            name: 'sketches',
          },
          {
            $type: 'BoolVar' as const,
            value: 'False' as const,
            id: { id: 12 },
            name: 'paints',
          },
        ],
        id: { id: 13 },
      },
      {
        $type: 'BoolVar' as const,
        value: 'True' as const,
        id: { id: 14 },
        name: 'codes',
      },
    ],
    id: { id: 15 },
  }

  const eitherExpr2 = decode(example2)
  let expr2: IRExpr = match(eitherExpr2)
    .with({ _tag: 'Right' }, ({ right }) => right)
    .otherwise(({ left }) => {
      console.error('Decoding failed for Example 2:', left)
      return {
        $type: 'BoolVar',
        value: 'True',
        id: { id: 15 },
        name: 'decoding somehow failed!!',
      }
    })

  const exprLirNode2 = ExprLirSource.toLir(nodeInfo, expr2)
  registry.setRoot(context, 'EXAMPLE_2' as LirRootType, exprLirNode2)
</script>

<h1 class="text-4xl font-bold text-center">Decision Logic Visualizer Draft</h1>
<section class="flex items-center justify-center my-8">
  <h2 class="text-2xl italic text-center text-gray-700 w-3/4">
    Examples of decision logic visualizations, starting from a 'json' of the
    IRExpr that eventually gets transformed into a SvelteFlow graph
  </h2>
</section>
<section id="example 1" class="example w-3/4 mx-auto space-y-4">
  <h3 class="text-2xl font-semibold">Example 1</h3>
  <SvelteFlowProvider>
    <ExprFlow {context} node={exprLirNode} />
  </SvelteFlowProvider>
  <section class="json-visualisation space-y-2">
    <input type="checkbox" id="example-1-json" class="peer hidden" />
    <label
      for="example-1-json"
      class="inline-flex w-fit cursor-pointer p-2 rounded-lg transition-colors"
      onmouseover={() => (isHovered = true)}
      onmouseout={() => (isHovered = false)}
      onfocus={() => (isHovered = true)}
      onblur={() => (isHovered = false)}
      style="background-color: {isHovered
        ? 'var(--color-button-hover)'
        : 'var(--color-button)'}"
    >
      <h4>Expand to view source JSON of the IRExpr</h4>
    </label>
    <pre
      class="max-h-0 overflow-hidden peer-checked:max-h-[500px] transition-[max-height] duration-300 ease-in-out bg-gray-100 p-2 rounded-md">
      <code>
        {JSON.stringify(example1, null, 2)}
      </code>
    </pre>
  </section>
</section>
<!-- TODO: Use a svelte snippet to reduce code duplication -->
<section id="example 2" class="example w-3/4 mx-auto space-y-4">
  <h3 class="text-2xl font-semibold">Example 2</h3>
  <SvelteFlowProvider>
    <ExprFlow {context} node={exprLirNode2} />
  </SvelteFlowProvider>
  <section class="json-visualisation space-y-2">
    <input type="checkbox" id="example-2-json" class="peer hidden" />
    <label
      for="example-2-json"
      class="inline-flex w-fit cursor-pointer p-2 rounded-lg transition-colors"
      onmouseover={() => (isHovered = true)}
      onmouseout={() => (isHovered = false)}
      onfocus={() => (isHovered = true)}
      onblur={() => (isHovered = false)}
      style="background-color: {isHovered
        ? 'var(--color-button-hover)'
        : 'var(--color-button)'}"
    >
      <h4>Expand to view source JSON of the IRExpr</h4>
    </label>
    <pre
      class="max-h-0 overflow-hidden peer-checked:max-h-[500px] transition-[max-height] duration-300 ease-in-out bg-gray-100 p-2 rounded-md">
      <code>
        {JSON.stringify(example2, null, 2)}
      </code>
    </pre>
  </section>
</section>
