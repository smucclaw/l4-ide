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
  } from '@repo/decision-logic-visualizer'

  /**************************
      Set up Lir
  ****************************/

  const lirRegistry = new LirRegistry()
  const context = new LirContext()
  const nodeInfo = { registry: lirRegistry, context }

  /**************************
      Make FunDeclLirNode
      and set Lir Root
  ****************************/

  const LADDER_VIZ_ROOT_TYPE = 'VizFunDecl'

  // To get around how calling onMount with an async function is not ideal
  // (There are also other benefits to using an #await block)
  const placeholderAlwaysPendingPromise = new Promise<{
    funDeclLirNode: FunDeclLirNode
    env: LadderEnv
  }>(() => {})
  let renderLadderPromise: Promise<{
    funDeclLirNode: FunDeclLirNode
    env: LadderEnv
  }> = $state(placeholderAlwaysPendingPromise)

  async function makeFunDeclLirNodeAndSetLirRoot(
    renderLadderInfo: RenderAsLadderInfo,
    ladderEnv: LadderEnv
  ) {
    const funDeclLirNode = await VizDeclLirSource.toLir(
      nodeInfo,
      ladderEnv,
      renderLadderInfo.funDecl
    )
    lirRegistry.setRoot(context, LADDER_VIZ_ROOT_TYPE, funDeclLirNode)
    return { funDeclLirNode, env: ladderEnv }
  }

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
        const ladderEnv = LadderEnv.make(
          lirRegistry,
          renderLadderInfo.verDocId,
          backendApi,
          LADDER_VIZ_ROOT_TYPE
        )
        renderLadderPromise = makeFunDeclLirNodeAndSetLirRoot(
          renderLadderInfo,
          ladderEnv
        )
        return makeRenderAsLadderSuccessResponse()
      }
    )

    messenger.start()
  })
</script>

{#await renderLadderPromise}
  <p>Loading Ladder Diagram...</p>
{:then ladder}
  <!-- TODO: Think more about whether to use #key -- which destroys and rebuilds the component --- or have 
  flow-base work with the reactive node prop -->
  {#key ladder.funDeclLirNode}
    <div class="slightly-shorter-than-full-viewport-height">
      <LadderFlow {context} node={ladder.funDeclLirNode} env={ladder.env} />
    </div>
  {/key}
{:catch error}
  <p>Error loading Ladder Diagram: {error.message}</p>
{/await}

<style>
  .slightly-shorter-than-full-viewport-height {
    height: 98svh;
  }
</style>
