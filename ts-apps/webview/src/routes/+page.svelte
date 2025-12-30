<script lang="ts">
  import { onDestroy, onMount } from 'svelte'
  import { RenderAsLadderInfo } from '@repo/viz-expr'
  import { LadderApiForWebview } from '$lib/ladder-api-for-webview'
  import { DecisionServiceQueryPlanRequest } from '@repo/vscode-webview-rpc'
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
  } from 'l4-ladder-visualizer'

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

  let currentLadderGraphId: import('l4-ladder-visualizer').LirId | null = null
  let lastQueryPlanBindingsKey: string | null = null
  let queryPlanInFlight = false
  let queryPlanNeedsRerun = false

  function bindingsKey(bindings: Record<string, boolean>) {
    const entries = Object.entries(bindings).sort(([a], [b]) =>
      a.localeCompare(b)
    )
    return JSON.stringify(entries)
  }

  function getCurrentAtomLabelBindings(): {
    docUri: string
    fnName: string
    bindings: Record<string, boolean>
  } | null {
    if (!ladderEnv) return null
    const docId = ladderEnv.getVersionedTextDocIdentifier()
    const top = ladderEnv.getTopFunDeclLirNode(context)
    const ladderGraph = top.getBody(context)

    const out: Record<string, boolean> = {}
    for (const [unique, val] of ladderGraph.getBindings(context).getEntries()) {
      if (!val) continue
      if (val.$type === 'TrueV') {
        out[ladderGraph.getLabelForUnique(context, unique)] = true
      } else if (val.$type === 'FalseV') {
        out[ladderGraph.getLabelForUnique(context, unique)] = false
      }
    }

    return { docUri: docId.uri, fnName: top.getFunName(context), bindings: out }
  }

  async function refreshQueryPlanFromExtension(): Promise<void> {
    const curr = getCurrentAtomLabelBindings()
    if (!curr) return

    const nextKey = `${curr.docUri}|${curr.fnName}|${bindingsKey(curr.bindings)}`
    if (lastQueryPlanBindingsKey === nextKey) return

    const resp = await messenger.sendRequest(
      DecisionServiceQueryPlanRequest,
      HOST_EXTENSION,
      curr
    )
    lastQueryPlanBindingsKey = nextKey

    const ladderGraph = ladderEnv.getTopFunDeclLirNode(context).getBody(context)

    const schemaSummary = (
      schema: import('@repo/decision-service-types').Parameter | null
    ): string | null => {
      if (!schema) return null
      if (schema.alias) return schema.alias
      if (schema.enum && schema.enum.length > 0)
        return `enum(${schema.enum.length})`
      if (schema.items) return `${schema.type}[]`
      return schema.type || null
    }

    const uniquesForAtoms = (atoms: Array<{ label: string }>) => {
      const out: number[] = []
      const seen = new Set<number>()
      for (const atom of atoms) {
        for (const u of ladderGraph.getUniquesForLabel(context, atom.label)) {
          if (seen.has(u)) continue
          seen.add(u)
          out.push(u)
        }
      }
      return out
    }

    const askByUnique = new Map<
      number,
      Array<{
        container: string
        path: string[]
        label: string
        schemaSummary: string | null
      }>
    >()
    const seenAskByUnique = new Map<number, Set<string>>()

    for (const ask of resp.asks) {
      for (const atom of ask.atoms) {
        for (const u of ladderGraph.getUniquesForLabel(context, atom.label)) {
          const seen = seenAskByUnique.get(u) ?? new Set<string>()
          seenAskByUnique.set(u, seen)
          if (seen.has(ask.label)) continue
          seen.add(ask.label)

          const entry = askByUnique.get(u) ?? []
          askByUnique.set(u, entry)
          entry.push({
            container: ask.container,
            path: ask.path,
            label: ask.label,
            schemaSummary: schemaSummary(ask.schema),
          })
        }
      }
    }

    const rankedAtoms =
      resp.asks.length > 0 ? resp.asks.flatMap((ask) => ask.atoms) : resp.ranked
    const rankedUniques = uniquesForAtoms(rankedAtoms)
    const stillNeededUniques = uniquesForAtoms(resp.stillNeeded)
    const nextUniques =
      resp.asks.length > 0 ? uniquesForAtoms(resp.asks[0]!.atoms) : []

    ladderGraph.setElicitationOverride(context, {
      ranked: rankedUniques,
      stillNeeded: stillNeededUniques,
      next: nextUniques,
      askByUnique,
    })
  }

  function scheduleQueryPlanRefresh() {
    if (queryPlanInFlight) {
      queryPlanNeedsRerun = true
      return
    }
    queryPlanInFlight = true
    void refreshQueryPlanFromExtension()
      .catch((e) => {
        console.warn('decision-service query-plan failed', e)
      })
      .finally(() => {
        queryPlanInFlight = false
        if (queryPlanNeedsRerun) {
          queryPlanNeedsRerun = false
          scheduleQueryPlanRefresh()
        }
      })
  }

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

      currentLadderGraphId = ladderEnv
        .getTopFunDeclLirNode(context)
        .getBody(context)
        .getId()
      lastQueryPlanBindingsKey = null
      scheduleQueryPlanRefresh()
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

    const sub = lirRegistry.subscribe((_ctx, id) => {
      if (!currentLadderGraphId) return
      if (!id.isEqualTo(currentLadderGraphId)) return
      scheduleQueryPlanRefresh()
    })
    onDestroy(() => sub.unsubscribe())
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
