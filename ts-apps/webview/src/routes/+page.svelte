<script lang="ts">
  import { onMount } from 'svelte'
  import { RenderAsLadderInfo } from '@repo/viz-expr'
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
    LadderEnv,
    LadderFlow,
    type FunDeclLirNode,
    VizDeclLirSource,
    LADDER_VIZ_ROOT_TYPE,
  } from '@repo/decision-logic-visualizer'

  /**************************
      Set up Lir
  ****************************/

  const lirRegistry = new LirRegistry()
  const context = new LirContext()
  const nodeInfo = { registry: lirRegistry, context }

  let ladderEnv: LadderEnv | undefined = $state(undefined)
  let funDeclLirNode: FunDeclLirNode | undefined = $state(undefined)

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

    // Set up handlers for webview x vscode extension messenger
    messenger.sendNotification(
      WebviewFrontendIsReadyNotification,
      HOST_EXTENSION,
      { $type: 'webviewReady' } as WebviewFrontendIsReadyMessage
    )

    messenger.onRequest(
      RenderAsLadder,
      (renderLadderInfo: RenderAsLadderInfo) => {
        const backendApi = new LadderApiForWebview(messenger)
        ladderEnv = LadderEnv.make(
          lirRegistry,
          renderLadderInfo.verTextDocId,
          backendApi
        )

        funDeclLirNode = VizDeclLirSource.toLir(
          nodeInfo,
          ladderEnv,
          renderLadderInfo.funDecl
        )
        // Set the top fun decl lir node in Lir Registry
        lirRegistry.setRoot(context, LADDER_VIZ_ROOT_TYPE, funDeclLirNode)

        return makeRenderAsLadderSuccessResponse()
      }
    )

    messenger.start()
  })
</script>

{#if funDeclLirNode && ladderEnv}
  <!-- TODO: Think more about whether to use #key -- which destroys and rebuilds the component --- or have flow-base work with the reactive node prop -->
  {#key funDeclLirNode}
    <div class="slightly-shorter-than-full-viewport-height">
      <LadderFlow {context} node={funDeclLirNode} env={ladderEnv} />
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
