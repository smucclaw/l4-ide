<script lang="ts">
  import { onMount } from 'svelte'
  import {
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
    type FunDeclLirNode,
    VizDeclLirSource,
  } from '@repo/decision-logic-visualizer'
  import type { WebviewApi } from 'vscode-webview'
  import { Messenger } from 'vscode-messenger-webview'
  import { HOST_EXTENSION } from 'vscode-messenger-common'

  /**************************
      Set up Lir
  ****************************/

  const lirRegistry = new LirRegistry()
  const context = new LirContext()
  const nodeInfo = { registry: lirRegistry, context }

  let declLirNode: FunDeclLirNode | undefined = $state(undefined)

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
        declLirNode = VizDeclLirSource.toLir(nodeInfo, payload.funDecl)
        lirRegistry.setRoot(context, 'VizDecl' as LirRootType, declLirNode)
        return makeSuccessVisualizeResponse()
      }
    )

    messenger.start()
  })
</script>

{#if declLirNode}
  <!-- TODO: Think more about whether to use #key -- which destroys and rebuilds the component --- or have flow-base work with the reactive node prop -->
  {#key declLirNode}
    <div class="slightly-shorter-than-full-viewport-height">
      <LadderFlow {context} node={declLirNode} lir={lirRegistry} />
    </div>
  {/key}
{/if}

<style>
  /** So there's space for the fn name
  TODO: Use calc or smtg like that to make the intent clearer
  */
  .slightly-shorter-than-full-viewport-height {
    height: 98svh;
  }
</style>
