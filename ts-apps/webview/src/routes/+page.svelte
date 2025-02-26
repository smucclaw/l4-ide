<script lang="ts">
  import { onMount } from 'svelte'
  import {
    IRDecl,
    VisualizeDecisionLogicIRInfo,
    VisualizeDecisionLogicRequest,
    makeSuccessVisualizeResponse,
    WebviewFrontendIsReadyNotification,
    type WebviewFrontendIsReadyMessage,
  } from '@repo/viz-expr'
  import {
    LirContext,
    LirRegistry,
    type LirRootType,
    LadderFlow,
    type DeclLirNode,
    VizDeclLirSource,
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

  let vizDecl: IRDecl | undefined = $state(undefined)
  let declLirNode: DeclLirNode | undefined = $derived(
    vizDecl && VizDeclLirSource.toLir(nodeInfo, vizDecl)
  )
  let funName = $derived(
    declLirNode && (declLirNode as DeclLirNode).getFunName(context)
  )
  $effect(() => {
    if (declLirNode) {
      registry.setRoot(context, 'VizDecl' as LirRootType, declLirNode)
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

    messenger.onRequest(
      VisualizeDecisionLogicRequest,
      (payload: VisualizeDecisionLogicIRInfo) => {
        vizDecl = payload.program

        return makeSuccessVisualizeResponse()
      }
    )

    messenger.start()
  })
</script>

<h1>{funName}</h1>

{#if vizDecl && declLirNode}
  {#key declLirNode}
    <div class="flash-on-update visualization-container">
      <LadderFlow {context} node={declLirNode} />
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
