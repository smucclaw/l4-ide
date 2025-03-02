<script lang="ts">
  import { Schema } from 'effect'
  import { IRDecl } from '@repo/viz-expr'
  import { VizDeclLirSource } from '$lib/data/viz-expr-to-lir.js'
  import {
    LirContext,
    LirRegistry,
    type LirRootType,
  } from '$lib/layout-ir/core.js'

  import Flow from '$lib/displayers/flow/flow.svelte'

  // TODO: This stuff should just be replaced with the tailwind on hovered classes
  let isHovered = $state(false)

  /***************************
      Example 1
  ****************************/

  // Parse JSON object into IRExpr
  const example1 = {
    $type: 'FunDecl' as const,
    id: { id: 100 },
    name: { label: 'Example1', unique: 1 },
    params: [],
    body: {
      $type: 'And' as const,
      args: [
        {
          $type: 'Or' as const,
          args: [
            {
              $type: 'BoolVar' as const,
              value: 'True' as const,
              id: { id: 1 },
              name: { label: 'eats', unique: 2 },
            },
            {
              $type: 'BoolVar' as const,
              value: 'Unknown' as const,
              id: { id: 2 },
              name: {
                label: 'walkswalkswalkswalkswalkswalkswalkswalkswalkswalks',
                unique: 3,
              },
            },
          ],
          id: { id: 3 },
        },
        {
          $type: 'BoolVar' as const,
          value: 'True' as const,
          id: { id: 4 },
          name: { label: 'swims', unique: 4 },
        },
      ],
      id: { id: 5 },
    },
  }

  const decode = Schema.decodeSync(IRDecl)
  const decl = decode(example1)
  // Set up Lir
  const registry = new LirRegistry()
  const context = new LirContext()
  const nodeInfo = { registry, context }

  const funDeclLirNode = VizDeclLirSource.toLir(nodeInfo, decl)
  registry.setRoot(context, 'EXAMPLE_1' as LirRootType, funDeclLirNode)

  /***************************
      Example 2
  ****************************/

  const example2 = {
    $type: 'FunDecl' as const,
    id: { id: 200 },
    name: { label: 'Example2', unique: 5 },
    params: [],
    body: {
      $type: 'And' as const,
      args: [
        {
          $type: 'Or' as const,
          args: [
            {
              $type: 'BoolVar' as const,
              value: 'False' as const,
              id: { id: 1 },
              name: { label: 'flies', unique: 6 },
            },
            {
              $type: 'BoolVar' as const,
              value: 'True' as const,
              id: { id: 2 },
              name: { label: 'runs', unique: 7 },
            },
            {
              $type: 'And' as const,
              args: [
                {
                  $type: 'BoolVar' as const,
                  value: 'Unknown' as const,
                  id: { id: 3 },
                  name: { label: 'swims', unique: 8 },
                },
                {
                  $type: 'BoolVar' as const,
                  value: 'True' as const,
                  id: { id: 4 },
                  name: { label: 'dives', unique: 9 },
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
          name: { label: 'jumps', unique: 10 },
        },
        {
          $type: 'BoolVar' as const,
          value: 'False' as const,
          id: { id: 8 },
          name: { label: 'jogs', unique: 11 },
        },
        {
          $type: 'BoolVar' as const,
          value: 'False' as const,
          id: { id: 9 },
          name: { label: 'reads', unique: 12 },
        },
        {
          $type: 'BoolVar' as const,
          value: 'True' as const,
          id: { id: 10 },
          name: { label: 'writes', unique: 13 },
        },
        {
          $type: 'Or' as const,
          args: [
            {
              $type: 'BoolVar' as const,
              value: 'Unknown' as const,
              id: { id: 11 },
              name: { label: 'sketches', unique: 14 },
            },
            {
              $type: 'BoolVar' as const,
              value: 'False' as const,
              id: { id: 12 },
              name: { label: 'paints', unique: 15 },
            },
          ],
          id: { id: 13 },
        },
        {
          $type: 'BoolVar' as const,
          value: 'True' as const,
          id: { id: 14 },
          name: { label: 'codes', unique: 16 },
        },
      ],
      id: { id: 15 },
    },
  }

  const decl2 = decode(example2)
  const declLirNode2 = VizDeclLirSource.toLir(nodeInfo, decl2)
  registry.setRoot(context, 'EXAMPLE_2' as LirRootType, declLirNode2)
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
  <div class="viz-container-with-height">
    <Flow {context} node={funDeclLirNode} lir={lirRegistry} />
  </div>
</section>
<!-- TODO: Use a svelte snippet to reduce code duplication -->
<section id="example 2" class="example w-3/4 mx-auto my-2 space-y-4">
  <h3 class="text-2xl font-semibold">Example 2</h3>
  <div class="viz-container-with-height">
    <Flow {context} node={declLirNode2} lir={lirRegistry} />
  </div>
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

<style>
  .viz-container-with-height {
    /* Seems best to set some kind of viewport-based height in the 'outermost' containing div */
    height: 70svh;
    margin-top: 1rem;
    margin-bottom: 2rem;
  }
</style>
