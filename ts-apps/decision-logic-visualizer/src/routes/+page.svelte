<!-- DEMO page for Ladder Visualizer,
 mostly for local development / manual testing -->

<script lang="ts">
  import { Schema } from 'effect'
  import { FunDecl } from '@repo/viz-expr'
  import { VizDeclLirSource } from '$lib/data/viz-expr-to-lir.js'
  import {
    LirContext,
    LirRegistry,
    type LirRootType,
  } from '$lib/layout-ir/core.js'
  import { LadderEnv } from '$lib/ladder-env.js'
  import Flow from '$lib/displayers/flow/flow.svelte'
  import { mockLadderBackendApi } from 'jl4-client-rpc'

  // TODO: This stuff should just be replaced with the tailwind on hovered classes
  let isHovered = $state(false)

  const mockVersionedDocId = {
    uri: 'file://local.ladder',
    version: 1,
  }

  /***************************
      Example 1
  ****************************/

  // Parse JSON object into IRExpr
  const example1 = {
    $type: 'FunDecl' as const,
    id: { id: 100 },
    name: { label: 'Example 1', unique: 1 },
    params: [],
    body: {
      $type: 'And' as const,
      args: [
        {
          $type: 'Or' as const,
          args: [
            {
              $type: 'UBoolVar' as const,
              value: 'True' as const,
              id: { id: 1 },
              name: { label: 'eats', unique: 2 },
              canInline: false,
            },
            {
              $type: 'UBoolVar' as const,
              value: 'Unknown' as const,
              id: { id: 2 },
              name: {
                label: 'walkswalkswalkswalkswalkswalkswalkswalkswalkswalks',
                unique: 3,
              },
              canInline: false,
            },
          ],
          id: { id: 3 },
        },
        {
          $type: 'UBoolVar' as const,
          value: 'True' as const,
          id: { id: 4 },
          name: { label: 'swims', unique: 4 },
          canInline: false,
        },
      ],
      id: { id: 5 },
    },
  }

  const decode = Schema.decodeSync(FunDecl)
  const decl = decode(example1)
  // Set up Lir
  const lirRegistry = new LirRegistry()
  const context = new LirContext()
  const nodeInfo = { registry: lirRegistry, context }

  const mockEnv = LadderEnv.make(
    lirRegistry,
    mockVersionedDocId,
    mockLadderBackendApi
  )
  const funDeclLirNodePromise = VizDeclLirSource.toLir(nodeInfo, mockEnv, decl)
  funDeclLirNodePromise.then((funDeclLirNode) => {
    lirRegistry.setRoot(context, 'EXAMPLE_1' as LirRootType, funDeclLirNode)
  })

  /***************************
      Example 2
  ****************************/

  const example2 = {
    $type: 'FunDecl' as const,
    id: { id: 200 },
    name: { label: 'Example 2', unique: 5 },
    params: [],
    body: {
      $type: 'And' as const,
      args: [
        {
          $type: 'Or' as const,
          args: [
            {
              $type: 'UBoolVar' as const,
              value: 'False' as const,
              id: { id: 1 },
              name: { label: 'flies', unique: 6 },
              canInline: false,
            },
            {
              $type: 'UBoolVar' as const,
              value: 'True' as const,
              id: { id: 2 },
              name: { label: 'runs', unique: 7 },
              canInline: false,
            },
            {
              $type: 'And' as const,
              args: [
                {
                  $type: 'UBoolVar' as const,
                  value: 'Unknown' as const,
                  id: { id: 3 },
                  name: { label: 'swims', unique: 8 },
                  canInline: false,
                },
                {
                  $type: 'UBoolVar' as const,
                  value: 'True' as const,
                  id: { id: 4 },
                  name: { label: 'dives', unique: 9 },
                  canInline: false,
                },
              ],
              id: { id: 5 },
            },
          ],
          id: { id: 6 },
        },
        {
          $type: 'UBoolVar' as const,
          value: 'True' as const,
          id: { id: 7 },
          name: { label: 'jumps', unique: 10 },
          canInline: false,
        },
        {
          $type: 'UBoolVar' as const,
          value: 'False' as const,
          id: { id: 8 },
          name: { label: 'jogs', unique: 11 },
          canInline: false,
        },
        {
          $type: 'UBoolVar' as const,
          value: 'False' as const,
          id: { id: 9 },
          name: { label: 'reads', unique: 12 },
          canInline: false,
        },
        {
          $type: 'UBoolVar' as const,
          value: 'True' as const,
          id: { id: 10 },
          name: { label: 'writes', unique: 13 },
          canInline: false,
        },
        {
          $type: 'Or' as const,
          args: [
            {
              $type: 'UBoolVar' as const,
              value: 'Unknown' as const,
              id: { id: 11 },
              name: { label: 'sketches', unique: 14 },
              canInline: false,
            },
            {
              $type: 'UBoolVar' as const,
              value: 'False' as const,
              id: { id: 12 },
              name: { label: 'paints', unique: 15 },
              canInline: false,
            },
          ],
          id: { id: 13 },
        },
        {
          $type: 'UBoolVar' as const,
          value: 'True' as const,
          id: { id: 14 },
          name: { label: 'codes', unique: 16 },
          canInline: false,
        },
      ],
      id: { id: 15 },
    },
  }

  const funDecl2 = decode(example2)
  const funDeclLirNode2Promise = VizDeclLirSource.toLir(
    nodeInfo,
    mockEnv,
    funDecl2
  )
  funDeclLirNode2Promise.then((funDeclLirNode2) => {
    lirRegistry.setRoot(context, 'EXAMPLE_2' as LirRootType, funDeclLirNode2)
  })
</script>

<h1 class="text-4xl font-bold text-center">Ladder Visualizer demo page</h1>
<section class="flex items-center justify-center my-8">
  <h2 class="text-2xl italic text-center text-gray-700 w-3/4">
    Examples of visualizing Boolean formulas as ladder diagrams, starting from a
    'json' of the IRExpr that eventually gets transformed into a SvelteFlow
    graph
  </h2>
</section>
<section id="example 1" class="example w-3/4 mx-auto space-y-4">
  <div class="viz-container-with-height">
    {#await funDeclLirNodePromise}
      <p>Loading Example 1...</p>
    {:then funDeclLirNode}
      <Flow {context} node={funDeclLirNode} env={mockEnv} />
    {:catch error}
      <p>Error loading Example 1: {error.message}</p>
    {/await}
  </div>
</section>
<!-- TODO: Use a svelte snippet to reduce code duplication -->
<section id="example 2" class="example w-3/4 mx-auto my-2 space-y-4">
  <div class="viz-container-with-height">
    {#await funDeclLirNode2Promise}
      <p>Loading Example 2...</p>
    {:then funDeclLirNode2}
      <Flow {context} node={funDeclLirNode2} env={mockEnv} />
    {:catch error}
      <p>Error loading Example 2: {error.message}</p>
    {/await}
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
        ? 'var(--accent)'
        : 'var(--ladder-color-button)'}"
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
