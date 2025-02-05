<script lang="ts">
  // import { SvelteFlowProvider } from '@xyflow/svelte'
  import { onMount } from 'svelte'
  import { Either, Schema } from 'effect'
  import { IRExpr } from '@repo/viz-expr'
  import {
    ExprLirSource,
    LirContext,
    LirRegistry,
    type LirRootType,
    // ExprLirNode,
    ExprFlow,
  } from '@repo/decision-logic-visualizer'
  import type { WebviewApi } from 'vscode-webview'
  import { Messenger } from 'vscode-messenger-webview'

  /**************************
        VSCode
  ****************************/
  let vsCodeApi: WebviewApi<null>
  let messenger: Messenger
  onMount(() => {
    // eslint-disable-next-line no-undef
    vsCodeApi = acquireVsCodeApi()
    messenger = new Messenger(vsCodeApi, { debugLog: true })
  })

  // Parse JSON object into IRExpr (the example logic remains the same)
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
      value: 'True',
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
  registry.setRoot(context, 'EXAMPLE_1' as LirRootType, exprLirNode)
</script>

<h1>Decision Logic Visualizer</h1>
<h2>
  Examples of decision logic visualizations, starting from a 'json' of the
  IRExpr that eventually gets transformed into a SvelteFlow graph
</h2>
<h3>Example 1</h3>
<!-- TODO: Probably ecapsulate SvelteFlowProvider within DLV instead of requiring that consumers like the webview also invoke it.
Will think more about this after getting more experience with the library -->
<ExprFlow {context} node={exprLirNode} />
<section>
  <p>The above is a visualization of</p>
  <pre><code>
  {JSON.stringify(example1, null, 2)}
</code></pre>
</section>
