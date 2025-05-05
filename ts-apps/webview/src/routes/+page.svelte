<script lang="ts">
  import { onMount } from 'svelte'
  import { RenderAsLadderInfo, VersionedDocId } from '@repo/viz-expr'
  import { type LadderBackendApi } from 'jl4-client-rpc'
  import { LadderApiForWebview } from '$lib/ladder-api-for-webview'
  import {
    RenderAsLadder,
    makeRenderAsLadderSuccessResponse,
    WebviewFrontendIsReadyNotification,
    type WebviewFrontendIsReadyMessage,
  } from 'jl4-client-rpc'
  import { Messenger } from 'vscode-messenger-webview'
  import { HOST_EXTENSION } from 'vscode-messenger-common'
  import type { WebviewApi } from 'vscode-webview'

  import {
    LirContext,
    LirRegistry,
    LadderFlow,
    type FunDeclLirNode,
    VizDeclLirSource,
  } from '@repo/decision-logic-visualizer'

  /**************************
      Set up Lir
  ****************************/

  const lirRegistry = new LirRegistry()
  const context = new LirContext()
  const nodeInfo = { registry: lirRegistry, context }

  let funDeclLirNode: FunDeclLirNode | undefined = $state(undefined)

  /**************************
        VSCode
  ****************************/

  let vsCodeApi: WebviewApi<null>
  let messenger: Messenger
  let backendApi: LadderBackendApi
  let versionedDocId: VersionedDocId

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

    messenger.onRequest(RenderAsLadder, (payload: RenderAsLadderInfo) => {
      versionedDocId = payload.verTextDocId
      funDeclLirNode = VizDeclLirSource.toLir(
        nodeInfo,
        payload.funDecl,
        versionedDocId
      )
      return makeRenderAsLadderSuccessResponse()
    })

    messenger.start()

    // Initialize LadderBackendApi
    backendApi = new LadderApiForWebview(messenger)
  })
</script>

{#if funDeclLirNode}
  <!-- TODO: Think more about whether to use #key -- which destroys and rebuilds the component --- or have flow-base work with the reactive node prop -->
  {#key funDeclLirNode}
    <div class="slightly-shorter-than-full-viewport-height">
      <LadderFlow
        {context}
        node={funDeclLirNode}
        lir={lirRegistry}
        {backendApi}
        {versionedDocId}
      />
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
