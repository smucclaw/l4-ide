<script lang="ts">
  import { onMount } from 'svelte'
  import { Messenger } from 'vscode-messenger-webview'
  import { HOST_EXTENSION } from 'vscode-messenger-common'
  import {
    WebviewFrontendIsReadyNotification,
    GetSidebarExportedFunctions,
    GetSidebarConnectionStatus,
    RequestSidebarLogin,
    RequestSidebarLogout,
    RequestOpenUrl,
    RequestOpenServiceUrl,
    RequestOpenConsole,
    RequestOpenExtensionSettings,
    RequestCopySignInLink,
    RequestDisconnect,
    SidebarConnectionStatusChanged,
    ListSidebarDeployments,
    RequestSidebarDeploy,
    RequestSidebarUndeploy,
    GetSidebarDeploymentOpenApi,
    GetSidebarDeploymentStatus,
    ShowNotification,
    type WebviewFrontendIsReadyMessage,
    type ExportedFunctionInfo,
    type GetSidebarConnectionStatusResponse,
    type SidebarDeploymentInfo,
  } from 'jl4-client-rpc'
  import type { WebviewApi } from 'vscode-webview'
  import ToolCard from '$lib/components/tool-card.svelte'
  import InspectorPanel from '$lib/components/inspector-panel.svelte'
  import DocsPanel from '$lib/components/docs-panel.svelte'

  let functions: ExportedFunctionInfo[] = $state([])
  let activeFileUri: string = $state('')
  let activeFileName: string = $state('')
  let connectionStatus: GetSidebarConnectionStatusResponse = $state({
    serviceUrl: '',
    connected: false,
    status: 'connecting',
    isLegaleseCloud: false,
  })
  let initialized: boolean = $state(false)
  let previewDebounceTimer: ReturnType<typeof setTimeout> | null = null
  let activeTab: 'docs' | 'inspector' | 'preview' | 'deployments' =
    $state('docs')
  let menuOpen: boolean = $state(false)

  // Deploy flow state
  type DeployView = 'preview' | 'deploy-form' | 'breaking-warning'
  let deployView: DeployView = $state('preview')
  let deploymentIdInput: string = $state('')
  let deploymentIdError: string = $state('')
  let breakingChanges: BreakingChange[] = $state([])
  let verifying: boolean = $state(false)
  let deploying: boolean = $state(false)

  function notify(type: 'info' | 'warning' | 'error', message: string) {
    messenger?.sendNotification(ShowNotification, HOST_EXTENSION, {
      type,
      message,
    })
  }

  // Deployments tab state
  let deployments: SidebarDeploymentInfo[] = $state([])
  let deploymentsLoading: boolean = $state(false)
  let undeployingId: string | null = $state(null)
  let collapsedDeployments: Set<string> = $state(new Set())
  let undeployConfirm: SidebarDeploymentInfo | null = $state(null)

  // Track expanded tool cards by key (survives re-renders from file edits)
  // Key format: "deploymentId/functionName" for deploy tab, "preview/functionName" for preview tab
  let expandedCards: Set<string> = $state(new Set())

  function isCardExpanded(key: string): boolean {
    return expandedCards.has(key)
  }

  function toggleCard(key: string) {
    const next = new Set(expandedCards)
    if (next.has(key)) next.delete(key)
    else next.add(key)
    expandedCards = next
  }

  // Reset undeploy confirmation when leaving the deployments tab
  $effect(() => {
    if (activeTab !== 'deployments') {
      undeployConfirm = null
    }
  })

  let compilingDeployments: Set<string> = $state(new Set())

  function toggleDeploymentCollapse(deploymentId: string) {
    const next = new Set(collapsedDeployments)
    if (next.has(deploymentId)) next.delete(deploymentId)
    else next.add(deploymentId)
    collapsedDeployments = next

    // If expanding an uncompiled deployment (no functions, no error), trigger compilation
    if (!next.has(deploymentId)) {
      const dep = deployments.find((d) => d.deploymentId === deploymentId)
      if (dep && dep.functions.length === 0 && !dep.error) {
        triggerCompilation(deploymentId)
      }
    }
  }

  async function triggerCompilation(deploymentId: string) {
    if (!messenger || compilingDeployments.has(deploymentId)) return
    compilingDeployments = new Set([...compilingDeployments, deploymentId])
    try {
      // GET /deployments/{id} triggers compilation for pending deployments
      const result = await messenger.sendRequest(
        GetSidebarDeploymentStatus,
        HOST_EXTENSION,
        { deploymentId }
      )
      // Update the deployment in-place with the result
      const dep = deployments.find((d) => d.deploymentId === deploymentId)
      if (dep) {
        dep.status = result.status
        dep.error = result.error
      }
      // Refresh the full list to get function details
      await fetchDeployments()
    } catch {
      // Compilation failed — refresh will pick up the error
      await fetchDeployments()
    } finally {
      const next = new Set(compilingDeployments)
      next.delete(deploymentId)
      compilingDeployments = next
    }
  }

  let messenger: InstanceType<typeof Messenger> | null = $state(null)

  function displayFileName(uri: string): string {
    const decoded = decodeURIComponent(uri.replace(/^file:\/\//, ''))
    return decoded.split('/').pop() ?? uri
  }

  function statusDotClass(
    status: GetSidebarConnectionStatusResponse['status']
  ): string {
    switch (status) {
      case 'connected':
        return 'dot-green'
      case 'connecting':
        return 'dot-yellow'
      case 'error':
        return 'dot-red'
      case 'not-configured':
        return 'dot-gray'
    }
  }

  function stripProtocol(url: string): string {
    return url.replace(/^https?:\/\//, '')
  }

  function statusLabel(conn: GetSidebarConnectionStatusResponse): string {
    if (!initialized) return 'Initializing...'
    if (conn.status === 'connected')
      return `Connected to ${stripProtocol(conn.serviceUrl)}`
    if (conn.status === 'connecting')
      return `Connecting to ${stripProtocol(conn.serviceUrl)}...`
    if (conn.status === 'error') return conn.error ?? 'Connection error'
    return conn.serviceUrl
      ? `Not connected to ${stripProtocol(conn.serviceUrl)}`
      : 'Not connected'
  }

  function actionLabel(conn: GetSidebarConnectionStatusResponse): string {
    if (conn.status === 'connected') {
      if (undeployingId) return 'Removing...'
      if (undeployConfirm) return 'Undeploy Now'
      if (deploying) return 'Deploying...'
      if (verifying) return 'Verifying...'
      if (deployView === 'breaking-warning') return 'Deploy Anyway'
      if (deployView === 'deploy-form') return 'Deploy Now'
      return 'Deploy'
    }
    if (conn.status === 'connecting') return 'Connecting...'
    if (conn.status === 'error') return 'Connect'
    return conn.serviceUrl ? 'Connect' : 'Sign in with Legalese Cloud'
  }

  function isActionDanger(): boolean {
    return deployView === 'breaking-warning' || undeployConfirm !== null
  }

  function isActionDisabled(): boolean {
    if (deploying || verifying || undeployingId) return true
    // Disable while connecting
    if (connectionStatus.status === 'connecting') return true
    // Connect / Sign in are never disabled
    if (connectionStatus.status !== 'connected') return false
    // Undeploy confirm is always enabled
    if (undeployConfirm) return false
    // Disable on non-deploy tabs
    if (
      activeTab === 'deployments' ||
      activeTab === 'inspector' ||
      activeTab === 'docs'
    )
      return true
    if (deployView === 'preview' && functions.length === 0) return true
    return false
  }

  function deriveDeploymentId(fileName: string): string {
    return fileName.replace(/\.l4$/i, '').replace(/[^a-zA-Z0-9_-]/g, '-')
  }

  const deploymentIdPattern = /^[a-zA-Z0-9_\s-]{1,36}$/

  function sanitizeDeploymentId(raw: string): string {
    return raw.trim().replace(/\s+/g, '-')
  }

  function showDeployForm() {
    deploymentIdInput = deriveDeploymentId(activeFileName)
    deploymentIdError = ''
    breakingChanges = []
    deployView = 'deploy-form'
  }

  function cancelDeploy() {
    deployView = 'preview'
  }

  /** Normalize a per-deployment OpenAPI response (fs-prefixed fields) into a common shape. */
  function normalizeRemoteFunctions(
    remoteOpenApi: Record<string, unknown>
  ): Array<{
    name: string
    parameters?: {
      properties?: Record<string, { type?: string }>
      required?: string[]
    }
    returnType?: string
  }> {
    // Per-deployment: { functions: [{ name, parameters, returnType, ... }] }
    const meta = remoteOpenApi.functions as
      | Array<Record<string, unknown>>
      | undefined
    if (meta) {
      return meta.map((f) => ({
        name: (f.name as string) ?? '',
        parameters: f.parameters as
          | {
              properties?: Record<string, { type?: string }>
              required?: string[]
            }
          | undefined,
        returnType: (f.returnType as string) ?? '',
      }))
    }
    return []
  }

  /** A segment of a breaking change message: either plain text or an identifier. */
  type ChangeSegment = { text: string; ident?: boolean }
  type BreakingChange = ChangeSegment[]

  function ident(name: string): ChangeSegment {
    const needsBackticks = /\s/.test(name)
    return { text: needsBackticks ? `\`${name}\`` : name, ident: true }
  }
  function txt(text: string): ChangeSegment {
    return { text }
  }

  /** Detect breaking changes between local functions and a remote deployment's OpenAPI. */
  function detectBreakingChanges(
    localFns: ExportedFunctionInfo[],
    remoteOpenApi: Record<string, unknown>
  ): BreakingChange[] {
    const changes: BreakingChange[] = []
    const remoteFns = normalizeRemoteFunctions(remoteOpenApi)
    const remoteByName = new Map(remoteFns.map((f) => [f.name, f]))

    for (const local of localFns) {
      const remote = remoteByName.get(local.name)
      if (!remote) continue // new function, not breaking

      // Check return type changes
      if (
        remote.returnType &&
        local.returnType &&
        remote.returnType !== local.returnType
      ) {
        changes.push([
          ident(local.name),
          txt(' return type changed from '),
          ident(remote.returnType),
          txt(' to '),
          ident(local.returnType),
        ])
      }

      const remoteProps = remote.parameters?.properties ?? {}
      const localProps = local.parameters?.properties ?? {}
      const remoteRequired = new Set(remote.parameters?.required ?? [])
      const localRequired = new Set(local.parameters?.required ?? [])

      // Removed params
      for (const name of Object.keys(remoteProps)) {
        if (!(name in localProps)) {
          changes.push([
            ident(local.name),
            txt(' parameter '),
            ident(name),
            txt(' removed'),
          ])
        }
      }

      // New required params
      for (const name of localRequired) {
        if (!remoteRequired.has(name) && !(name in remoteProps)) {
          changes.push([
            ident(local.name),
            txt(' new required parameter '),
            ident(name),
          ])
        }
      }

      // Type changes on existing params
      for (const [name, localParam] of Object.entries(localProps)) {
        const remoteParam = remoteProps[name]
        if (
          remoteParam &&
          remoteParam.type &&
          localParam.type &&
          remoteParam.type !== localParam.type
        ) {
          changes.push([
            ident(local.name),
            txt(' parameter '),
            ident(name),
            txt(' type changed from '),
            ident(remoteParam.type),
            txt(' to '),
            ident(localParam.type),
          ])
        }
      }
    }

    // Removed functions
    for (const remote of remoteFns) {
      if (!localFns.find((f) => f.name === remote.name)) {
        changes.push([txt('rule '), ident(remote.name), txt(' removed')])
      }
    }

    return changes
  }

  async function continueDeploy() {
    if (!messenger) return
    const raw = deploymentIdInput.trim()
    if (!deploymentIdPattern.test(raw)) {
      deploymentIdError =
        'Must be 1-36 chars: letters, numbers, hyphens, underscores, spaces'
      return
    }
    if (raw.startsWith('.')) {
      deploymentIdError = 'Must not start with a dot'
      return
    }
    const id = sanitizeDeploymentId(raw)
    deploymentIdError = ''
    verifying = true

    // Check for breaking changes if deployment exists
    try {
      const { openapi } = await messenger.sendRequest(
        GetSidebarDeploymentOpenApi,
        HOST_EXTENSION,
        { deploymentId: id }
      )
      if (openapi) {
        const changes = detectBreakingChanges(
          functions,
          openapi as Record<string, unknown>
        )
        if (changes.length > 0) {
          breakingChanges = changes
          verifying = false
          deployView = 'breaking-warning'
          return
        }
      }
    } catch {
      // Deployment doesn't exist yet — no breaking changes
    }

    verifying = false
    await executeDeploy(id)
  }

  async function deployAnyway() {
    await executeDeploy(sanitizeDeploymentId(deploymentIdInput))
  }

  async function executeDeploy(deploymentId: string) {
    if (!messenger || !activeFileUri) return
    deploying = true
    try {
      const result = await messenger.sendRequest(
        RequestSidebarDeploy,
        HOST_EXTENSION,
        { deploymentId, fileUri: activeFileUri }
      )
      if (result.success) {
        // Poll lightweight status endpoint until ready/failed
        const did = result.deploymentId ?? deploymentId
        let status = 'compiling'
        let error: string | undefined
        for (let i = 0; i < 60; i++) {
          await new Promise((r) => setTimeout(r, 1000))
          try {
            const resp = await messenger.sendRequest(
              GetSidebarDeploymentStatus,
              HOST_EXTENSION,
              { deploymentId: did }
            )
            status = resp.status
            error = resp.error
            if (status === 'ready' || status === 'failed') break
          } catch {
            // ignore transient errors
          }
        }
        deploying = false
        deployView = 'preview'
        if (status === 'ready') {
          await fetchDeployments()
          activeTab = 'deployments'
          notify('info', `Deployed "${did}" successfully.`)
        } else if (status === 'failed') {
          notify(
            'error',
            `Deploy "${did}" failed: ${error ?? 'compilation error'}`
          )
        } else {
          notify(
            'warning',
            `Deployed "${did}" — still compiling. Refresh later.`
          )
          activeTab = 'deployments'
        }
      } else {
        notify('error', result.error ?? 'Deploy failed')
        deploying = false
      }
    } catch (err) {
      notify('error', err instanceof Error ? err.message : 'Deploy failed')
      deploying = false
    }
  }

  async function fetchDeployments() {
    if (!messenger || !connectionStatus.connected) return
    deploymentsLoading = true
    try {
      const result = await messenger.sendRequest(
        ListSidebarDeployments,
        HOST_EXTENSION,
        undefined as never
      )
      deployments = result.deployments
    } catch {
      deployments = []
    } finally {
      deploymentsLoading = false
    }
  }

  function requestUndeploy(dep: SidebarDeploymentInfo) {
    undeployConfirm = dep
  }

  function cancelUndeploy() {
    undeployConfirm = null
  }

  async function handleUndeploy() {
    if (!messenger || !undeployConfirm) return
    const deploymentId = undeployConfirm.deploymentId
    undeployConfirm = null
    undeployingId = deploymentId
    try {
      const result = await messenger.sendRequest(
        RequestSidebarUndeploy,
        HOST_EXTENSION,
        { deploymentId }
      )
      if (result.success) {
        deployments = deployments.filter((d) => d.deploymentId !== deploymentId)
        notify('info', `Removed deployment "${deploymentId}".`)
      } else {
        notify('error', result.error ?? `Failed to remove "${deploymentId}"`)
      }
    } catch (err) {
      notify(
        'error',
        err instanceof Error
          ? err.message
          : `Failed to remove "${deploymentId}"`
      )
    } finally {
      undeployingId = null
    }
  }

  function handleAction() {
    if (connectionStatus.status === 'connected') {
      if (undeployConfirm) {
        handleUndeploy()
      } else if (deployView === 'breaking-warning') {
        deployAnyway()
      } else if (deployView === 'deploy-form') {
        continueDeploy()
      } else {
        showDeployForm()
      }
    } else if (connectionStatus.status === 'connecting') {
      // Do nothing while connecting
    } else {
      messenger?.sendNotification(
        RequestSidebarLogin,
        HOST_EXTENSION,
        undefined as never
      )
    }
  }

  function toggleMenu() {
    menuOpen = !menuOpen
  }

  function closeMenu() {
    menuOpen = false
  }

  function menuAction(fn: () => void) {
    return () => {
      closeMenu()
      fn()
    }
  }

  function openDocumentation() {
    messenger?.sendNotification(RequestOpenUrl, HOST_EXTENSION, {
      url: 'https://legalese.com/l4',
    })
  }

  function openServiceUrl() {
    messenger?.sendNotification(
      RequestOpenServiceUrl,
      HOST_EXTENSION,
      undefined as never
    )
  }

  function openConsole() {
    messenger?.sendNotification(
      RequestOpenConsole,
      HOST_EXTENSION,
      undefined as never
    )
  }

  function disconnect() {
    messenger?.sendNotification(
      RequestDisconnect,
      HOST_EXTENSION,
      undefined as never
    )
  }

  function signOut() {
    messenger?.sendNotification(
      RequestSidebarLogout,
      HOST_EXTENSION,
      undefined as never
    )
  }

  function openExtensionSettings() {
    messenger?.sendNotification(
      RequestOpenExtensionSettings,
      HOST_EXTENSION,
      undefined as never
    )
  }

  function copySignInLink() {
    messenger?.sendNotification(
      RequestCopySignInLink,
      HOST_EXTENSION,
      undefined as never
    )
  }

  async function refreshConnectionStatus() {
    if (!messenger) return
    try {
      connectionStatus = await messenger.sendRequest(
        GetSidebarConnectionStatus,
        HOST_EXTENSION,
        undefined as never
      )
    } catch {
      // ignore
    }
  }

  onMount(() => {
    // eslint-disable-next-line no-undef
    const vsCodeApi: WebviewApi<null> = acquireVsCodeApi()
    messenger = new Messenger(vsCodeApi, { debugLog: true })

    messenger.onNotification(SidebarConnectionStatusChanged, (status) => {
      connectionStatus = status
      initialized = true
      if (status.connected) fetchDeployments()
    })

    messenger.start()

    messenger.sendNotification(
      WebviewFrontendIsReadyNotification,
      HOST_EXTENSION,
      { $type: 'webviewReady' } as WebviewFrontendIsReadyMessage
    )

    refreshConnectionStatus().then(() => {
      initialized = true
      if (connectionStatus.connected) fetchDeployments()
    })

    window.addEventListener('message', (event) => {
      const msg = event.data
      if (msg?.type === 'l4-token-colors' && msg.colors) {
        const root = document.documentElement
        for (const [key, value] of Object.entries(
          msg.colors as Record<string, string>
        )) {
          root.style.setProperty(`--l4-tok-${key}`, value)
        }
      }

      if (msg?.type === 'l4-sidebar-switch-tab') {
        activeTab = msg.tab as typeof activeTab
      }

      if (msg?.type === 'l4-sidebar-clear-file') {
        activeFileUri = ''
        activeFileName = ''
        functions = []
        if (previewDebounceTimer) clearTimeout(previewDebounceTimer)
        if (deployView !== 'preview') deployView = 'preview'
        undeployConfirm = null
      }

      if (msg?.type === 'l4-sidebar-active-file') {
        const uri = msg.uri as string
        const version = msg.version as number
        activeFileUri = uri
        activeFileName = displayFileName(uri)
        if (deployView !== 'preview') {
          deployView = 'preview'
        }
        undeployConfirm = null
        // Debounce 200ms to avoid flickering on rapid edits
        if (previewDebounceTimer) clearTimeout(previewDebounceTimer)
        previewDebounceTimer = setTimeout(() => {
          messenger
            ?.sendRequest(GetSidebarExportedFunctions, HOST_EXTENSION, {
              verDocId: { uri, version },
            })
            .then((response) => {
              functions = response.functions
            })
            .catch(() => {
              functions = []
            })
        }, 200)
      }
    })

    document.addEventListener('click', (e) => {
      if (menuOpen) {
        const target = e.target as HTMLElement
        if (!target.closest('.menu-wrapper')) {
          menuOpen = false
        }
      }
    })
  })
</script>

<div class="sidebar-panel">
  <div class="tab-bar">
    <button
      class="tab"
      class:active={activeTab === 'docs'}
      onclick={() => (activeTab = 'docs')}
    >
      Docs
    </button>
    <button
      class="tab"
      class:active={activeTab === 'inspector'}
      onclick={() => (activeTab = 'inspector')}
    >
      Inspector
    </button>
    <button
      class="tab"
      class:active={activeTab === 'preview'}
      onclick={() => (activeTab = 'preview')}
    >
      Deploy
    </button>
    <button
      class="tab"
      class:active={activeTab === 'deployments'}
      onclick={() => (activeTab = 'deployments')}
    >
      Deployments
    </button>
  </div>

  <div class="tab-content">
    <div class="tab-pane" hidden={activeTab !== 'docs'}>
      <DocsPanel {messenger} />
    </div>
    <div class="tab-pane" hidden={activeTab !== 'inspector'}>
      <InspectorPanel {messenger} />
    </div>
    {#if activeTab === 'preview'}
      {#if deployView === 'deploy-form'}
        <!-- Deploy popover -->
        <div class="deploy-form">
          <button class="back-btn" onclick={cancelDeploy}
            >&larr; Back to preview</button
          >
          <div class="form-group">
            <label class="form-label" for="deployment-id"
              >Create new deployment</label
            >
            <input
              id="deployment-id"
              class="form-input"
              type="text"
              bind:value={deploymentIdInput}
              placeholder="Deployment name"
              maxlength="36"
            />
            {#if deploymentIdError}
              <div class="form-error">{deploymentIdError}</div>
            {/if}
          </div>
          {#if deployments.length > 0}
            <div class="form-group">
              <!-- svelte-ignore a11y_label_has_associated_control -->
              <label class="form-label">Or replace existing</label>
              <div class="existing-deployments">
                {#each deployments as dep (dep.deploymentId)}
                  <button
                    class="existing-dep-btn"
                    class:selected={deploymentIdInput === dep.deploymentId}
                    onclick={() => (deploymentIdInput = dep.deploymentId)}
                  >
                    {dep.deploymentId}
                    <span class="existing-dep-count"
                      >{dep.functions.length} rule{dep.functions.length !== 1
                        ? 's'
                        : ''}</span
                    >
                  </button>
                {/each}
              </div>
            </div>
          {/if}
        </div>
      {:else if deployView === 'breaking-warning'}
        <!-- Breaking change warning -->
        <div class="breaking-warning">
          <button class="back-btn" onclick={() => (deployView = 'deploy-form')}
            >&larr; Cancel deployment</button
          >
          <div class="warning-header">&#9888; Breaking changes detected</div>
          <div class="warning-body">
            <p class="warning-desc">
              Deploying the following rules in <strong
                >{deploymentIdInput}</strong
              > may break existing integrations:
            </p>
            <ul class="breaking-list">
              {#each breakingChanges as change}
                <li>
                  {#if change[0]?.ident}<span class="breaking-ident"
                      >{change[0].text}</span
                    ><br />{/if}
                  {#each change.slice(change[0]?.ident ? 1 : 0) as seg}{#if seg.ident}<span
                        class="breaking-ident">{seg.text}</span
                      >{:else}<span>{seg.text}</span>{/if}{/each}
                </li>
              {/each}
            </ul>
          </div>
        </div>
      {:else}
        <!-- Normal preview -->
        {#if functions.length === 0}
          <div class="empty-state">
            <p class="hint">
              Open an L4 file containing valid rules marked with @export
            </p>
          </div>
        {:else}
          <div class="functions-list">
            {#each functions as func (func.name)}
              <ToolCard
                {func}
                expanded={isCardExpanded('.local/' + func.name)}
                onToggle={() => toggleCard('.local/' + func.name)}
              />
            {/each}
          </div>
        {/if}
      {/if}
    {/if}
    {#if activeTab === 'deployments'}
      {#if undeployConfirm}
        <div class="breaking-warning">
          <button class="back-btn" onclick={cancelUndeploy}
            >&larr; Back to deployments</button
          >
          <div class="warning-header">
            &#9888; This will break existing integrations
          </div>
          <div class="warning-body">
            <p class="warning-desc">
              Removing <strong>{undeployConfirm.deploymentId}</strong> will permanently
              delete the following rules:
            </p>
            <ul class="breaking-list">
              {#each undeployConfirm.functions as func}
                <li><span class="breaking-ident">{func.name}</span></li>
              {/each}
            </ul>
          </div>
        </div>
      {:else if !connectionStatus.connected}
        <div class="empty-state">
          <p class="hint">
            {#if !initialized}
              &nbsp;
            {:else if connectionStatus.serviceUrl}
              Connect to {stripProtocol(connectionStatus.serviceUrl)} to view deployments.
            {:else}
              Sign in with Legalese Cloud to view your deployments.
            {/if}
          </p>
        </div>
      {:else if deploymentsLoading}
        <div class="empty-state">
          <p class="hint">Loading deployments...</p>
        </div>
      {:else if deployments.length === 0}
        <div class="empty-state">
          <p class="hint">
            No deployments yet. Deploy an L4 file to get started.
          </p>
        </div>
      {:else}
        <div class="deployments-list">
          {#each deployments as dep (dep.deploymentId)}
            <!-- svelte-ignore a11y_click_events_have_key_events -->
            <!-- svelte-ignore a11y_no_static_element_interactions -->
            <div
              class="deployment-header"
              onclick={() => toggleDeploymentCollapse(dep.deploymentId)}
            >
              <span
                class="chevron"
                class:rotated={!collapsedDeployments.has(dep.deploymentId)}
                >&#9002;</span
              >
              <span class="deployment-id">{dep.deploymentId}</span>
              <span class="deployment-fn-count">
                {#if dep.error}
                  Error
                {:else if dep.functions.length === 0 && compilingDeployments.has(dep.deploymentId)}
                  Compiling...
                {:else if dep.functions.length === 0}
                  Uncompiled
                {:else}
                  {dep.functions.length} rule{dep.functions.length !== 1
                    ? 's'
                    : ''}
                {/if}
              </span>
              <button
                class="undeploy-btn"
                disabled={undeployingId === dep.deploymentId}
                onclick={(e: MouseEvent) => {
                  e.stopPropagation()
                  requestUndeploy(dep)
                }}
                title="Undeploy"
              >
                {undeployingId === dep.deploymentId
                  ? 'Removing...'
                  : 'Undeploy'}
              </button>
            </div>
            {#if !collapsedDeployments.has(dep.deploymentId)}
              {#if dep.error}
                <div class="deployment-error"><pre>{dep.error}</pre></div>
              {:else if compilingDeployments.has(dep.deploymentId)}
                <div class="deployment-empty">Compiling...</div>
              {:else if dep.functions.length > 0}
                {#each dep.functions as func (func.name)}
                  <ToolCard
                    {func}
                    expanded={isCardExpanded(
                      dep.deploymentId + '/' + func.name
                    )}
                    onToggle={() =>
                      toggleCard(dep.deploymentId + '/' + func.name)}
                  />
                {/each}
              {:else}
                <div class="deployment-empty">No rules</div>
              {/if}
            {/if}
          {/each}
        </div>
      {/if}
    {/if}
  </div>

  {#if verifying || deploying}
    <div class="progress-bar"><div class="progress-bar-fill"></div></div>
  {/if}
  <div class="status-footer">
    <div class="footer-info">
      <div class="menu-wrapper">
        <button class="status-row-btn" onclick={toggleMenu}>
          <span class="status-dot {statusDotClass(connectionStatus.status)}"
          ></span>
          <span class="status-label-inline"
            >{statusLabel(connectionStatus)}</span
          >
          <svg class="dropdown-caret" width="10" height="10" viewBox="0 0 10 10"
            ><path
              d="M2 3.5 L5 7 L8 3.5"
              fill="none"
              stroke="currentColor"
              stroke-width="1.5"
              stroke-linecap="round"
              stroke-linejoin="round"
            /></svg
          >
        </button>
        {#if menuOpen}
          <div class="dropdown-menu">
            <button class="menu-item" onclick={menuAction(openDocumentation)}>
              Open L4 Docs website
            </button>
            <div class="menu-separator"></div>
            {#if connectionStatus.connected}
              {#if connectionStatus.serviceUrl}
                <button class="menu-item" onclick={menuAction(openServiceUrl)}>
                  Visit {connectionStatus.serviceUrl}
                </button>
              {/if}
              <button
                class="menu-item"
                onclick={menuAction(() => {
                  fetchDeployments()
                  activeTab = 'deployments'
                })}
              >
                Refresh Deployments
              </button>
              {#if !connectionStatus.isLegaleseCloud}
                <button
                  class="menu-item"
                  onclick={menuAction(openExtensionSettings)}
                >
                  Extension Settings
                </button>
                <button class="menu-item" onclick={menuAction(disconnect)}>
                  Disconnect
                </button>
              {/if}
              {#if connectionStatus.isLegaleseCloud}
                <div class="menu-separator"></div>
                <button class="menu-item" onclick={menuAction(openConsole)}>
                  Legalese Cloud Console
                </button>
                <button
                  class="menu-item"
                  onclick={menuAction(openExtensionSettings)}
                >
                  Extension Settings
                </button>
                <button class="menu-item" onclick={menuAction(signOut)}>
                  Sign out
                </button>
              {/if}
            {:else}
              <button
                class="menu-item"
                onclick={menuAction(openExtensionSettings)}
              >
                Extension Settings
              </button>
              <button class="menu-item" onclick={menuAction(copySignInLink)}>
                Copy Legalese Cloud Sign-In Link
              </button>
            {/if}
          </div>
        {/if}
      </div>
      <span class="file-info">
        {#if functions.length > 0}
          {functions.length} rule{functions.length !== 1 ? 's' : ''} exported in
          {activeFileName}
        {:else if activeFileName}
          No exported rules in {activeFileName}
        {:else}
          No file open
        {/if}
      </span>
    </div>
    {#if initialized}
      <button
        class="action-btn"
        class:action-btn-danger={isActionDanger()}
        onclick={handleAction}
        disabled={isActionDisabled()}
      >
        {actionLabel(connectionStatus)}
      </button>
    {/if}
  </div>
</div>

<style>
  :global(html),
  :global(body) {
    height: 100%;
    margin: 0;
    padding: 0;
    overflow: hidden;
  }

  .sidebar-panel {
    font-family: var(--vscode-font-family, sans-serif);
    font-size: var(--vscode-font-size, 13px);
    color: var(--vscode-foreground);
    background: var(--vscode-sideBar-background);
    display: flex;
    flex-direction: column;
    height: 100%;
    min-height: 100vh;
    overflow: hidden;
  }

  .tab-bar {
    display: flex;
    border-bottom: 1px solid var(--vscode-panel-border, #444);
    flex-shrink: 0;
  }

  .tab {
    flex: 1;
    padding: 6px 8px;
    background: none;
    border: none;
    border-bottom: 1px solid transparent;
    color: var(--vscode-foreground);
    cursor: pointer;
    font-size: 0.92em;
    opacity: 0.6;
    transition: opacity 0.15s;
  }

  .tab:hover {
    opacity: 0.9;
  }

  .tab.active {
    opacity: 1;
    border-bottom-color: #c8376a;
  }

  .tab-content {
    flex: 1;
    overflow-y: auto;
    padding: 16px;
  }

  .tab-pane {
    height: 100%;
  }

  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 40vh;
    text-align: center;
    color: var(--vscode-descriptionForeground);
  }

  .empty-state .hint {
    font-size: 0.95em;
    line-height: 1.2;
    max-width: 200px;
  }

  .functions-list {
    display: flex;
    flex-direction: column;
  }

  .progress-bar {
    flex-shrink: 0;
    height: 2px;
    overflow: hidden;
    background: var(--vscode-panel-border, #444);
  }

  .progress-bar-fill {
    height: 100%;
    width: 40%;
    background: var(--vscode-progressBar-background, #0e70c0);
    animation: progress-sweep 1.2s ease-in-out infinite;
  }

  @keyframes progress-sweep {
    0% {
      transform: translateX(-100%);
    }
    100% {
      transform: translateX(350%);
    }
  }

  .status-footer {
    flex-shrink: 0;
    padding: 16px;
    border-top: 1px solid var(--vscode-panel-border, #444);
    background: var(--vscode-sideBar-background);
    display: flex;
    align-items: stretch;
    gap: 8px;
  }

  .footer-info {
    display: flex;
    flex-direction: column;
    gap: 4px;
    flex: 1;
    min-width: 0;
  }

  .status-row {
    display: flex;
    align-items: center;
    gap: 6px;
    line-height: 1.3;
    min-height: 18px;
  }

  .status-dot {
    width: 7px;
    height: 7px;
    border-radius: 50%;
    flex-shrink: 0;
  }

  .dot-green {
    background: #89d185;
  }

  .dot-yellow {
    background: #cca700;
  }

  .dot-red {
    background: #f14c4c;
  }

  .dot-gray {
    background: #6e6e6e;
  }

  .status-label {
    font-size: 0.88em;
    color: var(--vscode-descriptionForeground);
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    flex: 1;
    min-width: 0;
  }

  .file-info {
    font-size: 0.85em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.7;
    flex: 1;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .action-btn {
    flex-shrink: 0;
    align-self: stretch;
    padding: 5px 10px;
    font-size: 0.88em;
    border-radius: 3px;
    border: none;
    background: #c8376a;
    color: #fff;
    cursor: pointer;
    white-space: nowrap;
  }

  .action-btn:hover:not(:disabled) {
    background: #d94d7e;
  }

  .action-btn:disabled {
    opacity: 0.4;
    cursor: default;
  }

  /* Deploy form */
  .deploy-form,
  .breaking-warning {
    padding: 4px 0;
    font-size: 1em;
    line-height: 1.2;
  }

  .back-btn {
    background: none;
    border: none;
    color: var(--vscode-descriptionForeground);
    cursor: pointer;
    font-size: 0.88em;
    padding: 2px 0 6px;
    margin-bottom: 8px;
  }

  .back-btn:hover {
    color: var(--vscode-foreground);
  }

  .form-group {
    margin-bottom: 10px;
  }

  .form-label {
    display: block;
    font-size: 0.85em;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.7;
    margin-bottom: 4px;
  }

  .form-input {
    width: 100%;
    box-sizing: border-box;
    padding: 4px 6px;
    background: var(--vscode-input-background, #3c3c3c);
    color: var(--vscode-input-foreground, #ccc);
    border: 1px solid var(--vscode-input-border, #555);
    border-radius: 3px;
    outline: none;
  }

  .form-input:focus {
    border-color: #c8376a;
  }

  .form-error {
    color: #f14c4c;
    margin-top: 2px;
  }

  .existing-deployments {
    display: flex;
    flex-direction: column;
    gap: 5px;
  }

  .existing-dep-btn {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 4px 8px;
    background: none;
    border: 1px solid var(--vscode-panel-border, #444);
    border-radius: 3px;
    color: var(--vscode-foreground);
    cursor: pointer;
    text-align: left;
  }

  .existing-dep-btn:hover {
    background: var(--vscode-list-hoverBackground, #2a2d2e);
  }

  .existing-dep-btn.selected {
    border-color: #c8376a;
    background: rgba(200, 55, 106, 0.1);
  }

  .existing-dep-count {
    font-size: 0.85em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.6;
  }

  .deploy-continue-btn,
  .action-btn-danger {
    background: #a33 !important;
  }

  .action-btn-danger:hover:not(:disabled) {
    background: #c44 !important;
  }

  /* Breaking change warning */
  .warning-header {
    font-weight: 600;
    color: #cca700;
    margin-bottom: 10px;
  }

  .warning-desc {
    margin: 0 0 10px 0;
    color: var(--vscode-foreground);
  }

  .breaking-list {
    padding-left: 16px;
    margin: 0;
    color: var(--vscode-foreground);
  }

  .breaking-list li {
    margin-bottom: 10px;
  }

  .breaking-ident {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.92em;
    color: var(--l4-tok-identifier, #4ec9b0);
  }

  .deployments-list {
    display: flex;
    flex-direction: column;
  }

  .deployment-header {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 4px 8px;
    margin-top: 4px;
    margin-bottom: 4px;
    background: var(--vscode-sideBarSectionHeader-background, #252526);
    border: 1px solid var(--vscode-panel-border, #444);
    border-radius: 4px;
    cursor: pointer;
    user-select: none;
    font-size: 0.92em;
    font-weight: 500;
  }

  .collapse-toggle {
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    padding: 0;
    font-size: 11px;
    display: flex;
    align-items: center;
    flex-shrink: 0;
  }

  .chevron {
    display: inline-block;
    color: var(--vscode-descriptionForeground);
    transition: transform 0.15s;
    transform-origin: 25% 50%;
  }

  .chevron.rotated {
    transform: rotate(90deg);
  }

  .deployment-id {
    font-size: 0.95em;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    flex: 1;
    min-width: 0;
  }

  .deployment-fn-count {
    font-size: 0.82em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.6;
    flex-shrink: 0;
  }

  .deployment-empty {
    font-size: 0.88em;
    color: var(--vscode-descriptionForeground);
    opacity: 0.6;
    padding: 4px 0;
  }

  .deployment-error {
    margin: 4px 0;
    padding: 6px 8px;
    background: var(
      --vscode-inputValidation-errorBackground,
      rgba(255, 0, 0, 0.1)
    );
    border: 1px solid
      var(--vscode-inputValidation-errorBorder, rgba(255, 0, 0, 0.3));
    border-radius: 4px;
  }

  .deployment-error pre {
    margin: 0;
    font-size: 0.82em;
    font-family: var(--vscode-editor-font-family, monospace);
    white-space: pre-wrap;
    word-break: break-word;
    max-height: 260px;
    overflow-y: auto;
    color: var(--vscode-errorForeground, #f48771);
  }

  .undeploy-btn {
    margin-left: auto;
    font-size: 0.85em;
    padding: 2px 1px 2px 4px;
    border-radius: 3px;
    border: none;
    background: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    opacity: 0.5;
    flex-shrink: 0;
  }

  .undeploy-btn:hover:not(:disabled) {
    opacity: 1;
    color: #f14c4c;
  }

  .undeploy-btn:disabled {
    opacity: 0.4;
    cursor: default;
  }

  .menu-wrapper {
    position: relative;
    display: flex;
    align-items: center;
    min-height: 18px;
  }

  .status-row-btn {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    max-width: 100%;
    padding: 0;
    margin: 0;
    background: none;
    border: none;
    color: inherit;
    cursor: pointer;
    font: inherit;
    text-align: left;
    line-height: 1.3;
    min-height: 0;
    vertical-align: top;
  }

  .status-label-inline {
    font-size: 0.88em;
    color: var(--vscode-descriptionForeground);
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    min-width: 0;
  }

  .dropdown-caret {
    flex-shrink: 0;
    color: var(--vscode-descriptionForeground);
  }

  .dropdown-menu {
    position: absolute;
    bottom: calc(100% + 4px);
    left: 0;
    min-width: 200px;
    background: var(--vscode-menu-background, #252526);
    border: 1px solid var(--vscode-menu-border, #454545);
    border-radius: 4px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
    z-index: 100;
    padding: 4px 0;
  }

  .menu-item {
    display: block;
    width: 100%;
    padding: 5px 12px;
    font-size: 0.92em;
    text-align: left;
    background: none;
    border: none;
    color: var(--vscode-menu-foreground, #ccc);
    cursor: pointer;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .menu-item:hover {
    background: var(--vscode-menu-selectionBackground, #04395e);
    color: var(--vscode-menu-selectionForeground, #fff);
  }

  .menu-separator {
    height: 1px;
    margin: 4px 0;
    background: var(--vscode-menu-separatorBackground, #454545);
  }
</style>
