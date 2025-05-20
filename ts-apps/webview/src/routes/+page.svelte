<script lang="ts">
  import { onMount } from 'svelte'
  import { RenderAsLadderInfo } from '@repo/viz-expr'
  import { LadderApiForWebview } from '$lib/ladder-api-for-webview'
  import {
    RenderAsLadder,
    makeRenderAsLadderSuccessResponse,
    WebviewFrontendIsReadyNotification,
    type WebviewFrontendIsReadyMessage,
    type LadderBackendApi,
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
      FunDeclLirNode maker
  ****************************/

  const LADDER_VIZ_ROOT_TYPE = 'VizFunDecl'

  let backendApi: LadderBackendApi
  let ladderEnv: LadderEnv

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
    ladderEnv: LadderEnv,
    renderLadderInfo: RenderAsLadderInfo
  ) {
    const funDeclLirNode = await VizDeclLirSource.toLir(
      nodeInfo,
      ladderEnv,
      renderLadderInfo.funDecl
    )
    lirRegistry.setRoot(context, LADDER_VIZ_ROOT_TYPE, funDeclLirNode)
    return { env: ladderEnv, funDeclLirNode }
  }

  /**************************
        VSCode
  ****************************/

  let vsCodeApi: WebviewApi<null>
  let messenger: Messenger

  // This needs to be inside onMount so that acquireVsCodeApi does not get looked up during SSR or pre-rendering
  onMount(() => {
    /*************************************************************
              Init VSCode API and Messenger
    *************************************************************/

    // eslint-disable-next-line no-undef
    vsCodeApi = acquireVsCodeApi()
    messenger = new Messenger(vsCodeApi, { debugLog: true })

    /*************************************************************
                 Init LadderBackendApi
    *************************************************************/

    /** Helper: Make / update the LadderFlow component (also updates ladderEnv) */
    const makeLadderFlow = async (
      ladderInfo: RenderAsLadderInfo
    ): Promise<void> => {
      // Re-make / update ladderEnv using the verDocId from the new ladderInfo,
      // just in case
      ladderEnv = LadderEnv.make(
        lirRegistry,
        ladderInfo.verDocId,
        backendApi,
        LADDER_VIZ_ROOT_TYPE
      )
      renderLadderPromise = makeFunDeclLirNodeAndSetLirRoot(
        ladderEnv,
        ladderInfo
      )
      await renderLadderPromise
    }
    backendApi = new LadderApiForWebview(messenger, makeLadderFlow)

    /*************************************************************
           webview x vscode extension messenger
        communication initialization and handler registration
    *************************************************************/

    messenger.sendNotification(
      WebviewFrontendIsReadyNotification,
      HOST_EXTENSION,
      { $type: 'webviewReady' } as WebviewFrontendIsReadyMessage
    )

    messenger.onRequest(
      RenderAsLadder,
      async (renderLadderInfo: RenderAsLadderInfo) => {
        await makeLadderFlow(renderLadderInfo)
        return makeRenderAsLadderSuccessResponse()
      }
    )

    messenger.start()
  })
</script>

<!-- TODO: Make this a Svelte snippet or smtg like that so it can be shared between the vscode webview and jl4 webview pane -->
{#await renderLadderPromise}
  <p>Loading Ladder Diagram...</p>
{:then ladder}
  <!-- TODO: Think more about whether to use #key -- which destroys and rebuilds the component --- or have 
  flow-base work with the reactive node prop (i.e., have LadderFlow 
  update the relevant internals without destroying and rebuilding the component).
  Going with #key for now because it feels conceptually simpler -->
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
