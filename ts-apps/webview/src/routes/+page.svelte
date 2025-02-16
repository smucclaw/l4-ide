<script lang="ts">
  import { onMount } from 'svelte'
  import {
    IRExpr,
    VisualizeDecisionLogicIRInfo,
    VisualizeDecisionLogicRequest,
    makeSuccessVisualizeResponse,
    WebviewFrontendIsReadyNotification,
    type WebviewFrontendIsReadyMessage,
  } from '@repo/viz-expr'
  import {
    ExprLirSource,
    LirContext,
    LirRegistry,
    type LirRootType,
    type ExprLirNode,
    ExprFlow,
  } from '@repo/decision-logic-visualizer'
  import type { WebviewApi } from 'vscode-webview'
  import { Messenger } from 'vscode-messenger-webview'
  import { HOST_EXTENSION } from 'vscode-messenger-common'

  /**************************
      Set up Lir
  ****************************/

  const registry = new LirRegistry()
  const context = new LirContext()
  const nodeInfo = { registry, context }

  let vizExpr: IRExpr | undefined = $state(undefined)
  let exprLirNode: ExprLirNode | undefined = $derived(
    vizExpr && ExprLirSource.toLir(nodeInfo, vizExpr)
  )
  $effect(() => {
    if (exprLirNode) {
      registry.setRoot(context, 'VizExpr' as LirRootType, exprLirNode)
    }
  })

  /**************************
        VSCode
  ****************************/

  let vsCodeApi: WebviewApi<null>
  let messenger: Messenger

  // This needs to be inside onMount so that acquireVsCodeApi does not get looked up during SSR or pre-rendering
  onMount(() => {
    // eslint-disable-next-line no-undef
    vsCodeApi = acquireVsCodeApi()
    messenger = new Messenger(vsCodeApi, { debugLog: true })

    messenger.sendNotification(
      WebviewFrontendIsReadyNotification,
      HOST_EXTENSION,
      { $type: 'webviewReady' } as WebviewFrontendIsReadyMessage
    )

    console.log('Webview: vsCodeApi:', vsCodeApi)
    console.log('Webview: onMount!')

    messenger.onRequest(
      VisualizeDecisionLogicRequest,
      (payload: VisualizeDecisionLogicIRInfo) => {
        vizExpr = payload.program
        console.log('vizExpr:', vizExpr)

        return makeSuccessVisualizeResponse()
      }
    )

    messenger.start()
  })
</script>

<h1>Visualize L4</h1>
<!-- TODO: Probably ecapsulate SvelteFlowProvider within DLV instead of requiring that consumers like the webview also invoke it.
Will think more about this after getting more experience with the library -->

{#if vizExpr && exprLirNode}
  {#key exprLirNode}
    <div class="flash-on-update visualization-container">
      <ExprFlow {context} node={exprLirNode} />
    </div>
  {/key}
{/if}

<style>
  @keyframes flash {
    0%,
    90% {
      background-color: hsl(var(--neutral));
    }
    50% {
      background-color: hsl(var(--muted));
    }
  }

  .flash-on-update {
    animation: flash 0.6s;
  }

  h1 {
    margin-top: 10px;
    padding-bottom: 2px;
    font-size: 1.5rem;
    line-height: 1.1rem;
    font-weight: 700;
    text-align: center;
  }
</style>
