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
    RequestAddL4ToolsToClaudeCode,
    RequestInstallL4Cli,
    RequestCopySignInLink,
    RequestDisconnect,
    SidebarConnectionStatusChanged,
    ListSidebarDeployments,
    RequestSidebarDeploy,
    RequestSidebarUndeploy,
    RequestSidebarDownloadDeployment,
    GetSidebarDeploymentSchemas,
    GetSidebarDeploymentStatus,
    GetSidebarUpdateStatus,
    ShowNotification,
    RequestRevealLocation,
    type WebviewFrontendIsReadyMessage,
    type ExportedFunctionInfo,
    type GetSidebarConnectionStatusResponse,
    type SidebarDeploymentInfo,
    type RemoteFunctionSchema,
  } from 'jl4-client-rpc'
  import type { WebviewApi } from 'vscode-webview'
  import ToolCard from '$lib/components/tool-card.svelte'
  import InspectorPanel from '$lib/components/inspector-panel.svelte'
  import DeploymentMetadata from '$lib/components/deployment-metadata.svelte'
  import DocsPanel from '$lib/components/docs-panel.svelte'
  import AiChatPanel from '$lib/components/ai/ai-chat-panel.svelte'

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
  let activeTab: 'ai-chat' | 'docs' | 'inspector' | 'preview' | 'deployments' =
    $state('docs')
  let menuOpen: boolean = $state(false)

  // Deploy flow state
  // Deploy is a two-step wizard: `deploy-form` picks the deployment id,
  // `mission` collects deployment-level metadata (description/mission),
  // then the actual deploy fires. `breaking-warning` interrupts before
  // the deploy if the new shape would break existing integrations.
  type DeployView = 'preview' | 'deploy-form' | 'mission' | 'breaking-warning'
  let deployView: DeployView = $state('preview')
  let deploymentIdInput: string = $state('')
  let deploymentIdError: string = $state('')
  // Operator-supplied "Intended use" for the deployment.
  // First of a planned set of per-deployment configuration fields.
  // Blank for a fresh deployment; pre-populated from the deployed
  // metadata.description when redeploying an existing id, so the user
  // sees/edits the current value rather than silently blanking it.
  let deploymentMission: string = $state('')
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
  let downloadingId: string | null = $state(null)
  let openDeploymentMenuId: string | null = $state(null)
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
      openDeploymentMenuId = null
    }
  })

  // Dismiss the per-deployment dropdown when the user clicks anywhere
  // outside any deployment-menu wrapper. Listener is only attached
  // while a menu is open so we don't pay for it on every click.
  $effect(() => {
    if (openDeploymentMenuId === null) return
    const onPointerDown = (e: MouseEvent) => {
      const target = e.target as HTMLElement | null
      if (!target?.closest('.deployment-menu-wrapper')) {
        openDeploymentMenuId = null
      }
    }
    window.addEventListener('mousedown', onPointerDown)
    return () => window.removeEventListener('mousedown', onPointerDown)
  })

  // Reset the Deploy-tab flow when switching away so the footer action
  // button returns to its idle "Deploy" label instead of remaining on
  // "Deploy Now" / "Verifying..." while the user is on another tab.
  $effect(() => {
    if (activeTab !== 'preview' && deployView !== 'preview') {
      deployView = 'preview'
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
      if (deployView === 'mission') return 'Deploy Now'
      if (deployView === 'deploy-form') return 'Continue'
      if (activeTab === 'deployments' && deployments.length > 0)
        return 'Open in web browser'
      // Tabs that aren't the Deploy tab surface the button as
      // "Deploy preview" — one click jumps to Deploy and shows the
      // tool cards (which is already the default Deploy-tab view).
      // Keeps the footer useful without forcing users to hunt for
      // the Deploy tab manually when they've authored an @export,
      // while the label makes the target action (a deploy, via the
      // preview step) clear.
      if (
        activeTab === 'ai-chat' ||
        activeTab === 'docs' ||
        activeTab === 'inspector'
      ) {
        return 'Deploy preview'
      }
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
    // On the deployments tab, the button becomes "Open in web browser"
    // when at least one deployment exists; otherwise stays disabled.
    if (activeTab === 'deployments') {
      return deployments.length === 0
    }
    // Non-deploy tabs surface a "Preview" button that jumps to the
    // Deploy tab. Enabled iff the active file has at least one rule
    // ready for export — otherwise the Deploy tab would just show
    // the empty "Open an L4 file containing valid rules" hint.
    if (
      activeTab === 'inspector' ||
      activeTab === 'docs' ||
      activeTab === 'ai-chat'
    )
      return functions.length === 0
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
    deploymentMission = ''
    deployView = 'deploy-form'
  }

  function selectExistingDeployment(id: string) {
    deploymentIdInput = id
    // Redeploy: pre-populate "Intended use" from the deployed metadata
    // so it shows/edits the current value instead of starting blank.
    const dep = deployments.find((d) => d.deploymentId === id)
    deploymentMission = dep?.description ?? ''
  }

  /**
   * Step 1 → step 2: validate the deployment id, then advance to the
   * metadata (mission) screen. The actual deploy fires from there.
   */
  function goToMission() {
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
    deploymentIdError = ''
    // If the (typed or selected) id matches an existing deployment and
    // the user hasn't entered anything, pre-fill "Intended use" from the
    // deployed metadata so a redeploy preserves/shows it.
    if (!deploymentMission.trim()) {
      const match = deployments.find(
        (d) => d.deploymentId === sanitizeDeploymentId(raw)
      )
      if (match?.description) deploymentMission = match.description
    }
    deployView = 'mission'
  }

  function cancelDeploy() {
    deployView = 'preview'
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

  /** A node in a parameter / return-value JSON schema (structurally a
   *  superset of both `FunctionParameters` and `FunctionParameter`). */
  type SchemaNode = {
    type?: string
    format?: string
    enum?: string[]
    properties?: Record<string, SchemaNode>
    required?: string[]
    items?: SchemaNode
  }

  /** Direction of compatibility: `in` = request parameter (caller sends),
   *  `out` = return value (caller consumes). The two are mirror images —
   *  e.g. a *new optional input* is safe, but a *removed output field* is
   *  not (and vice-versa). */
  type Dir = 'in' | 'out'

  function subject(
    fn: string,
    kind: 'parameter' | 'return value',
    loc: string
  ): ChangeSegment[] {
    return loc
      ? [ident(fn), txt(` ${kind} `), ident(loc)]
      : [ident(fn), txt(` ${kind}`)]
  }

  /**
   * Recursively diff one schema node (old `remote` vs new `local`),
   * appending a message for every backwards-incompatible change.
   * Recurses through object `properties` and array `items` so nested
   * record/list parameters are compared at every depth.
   */
  function diffNode(
    fn: string,
    kind: 'parameter' | 'return value',
    loc: string,
    remote: SchemaNode,
    local: SchemaNode,
    dir: Dir,
    changes: BreakingChange[]
  ) {
    const here = () => subject(fn, kind, loc)

    // Type change — once the type differs, deeper structural diffing
    // would just be noise, so report and stop descending this branch.
    if (remote.type && local.type && remote.type !== local.type) {
      changes.push([
        ...here(),
        txt(' type changed from '),
        ident(remote.type),
        txt(' to '),
        ident(local.type),
      ])
      return
    }

    // Format change (e.g. plain string → date) is a contract change.
    const rFmt = remote.format ?? ''
    const lFmt = local.format ?? ''
    if (rFmt !== lFmt && (rFmt || lFmt)) {
      changes.push([
        ...here(),
        txt(' format changed from '),
        ident(rFmt || 'none'),
        txt(' to '),
        ident(lFmt || 'none'),
      ])
    }

    // Enum (allowed value set).
    const rEnum = remote.enum ?? []
    const lEnum = local.enum ?? []
    if (rEnum.length && lEnum.length) {
      if (dir === 'in') {
        const dropped = rEnum.filter((v) => !lEnum.includes(v))
        if (dropped.length)
          changes.push([
            ...here(),
            txt(' no longer accepts '),
            ident(dropped.join(', ')),
          ])
      } else {
        const added = lEnum.filter((v) => !rEnum.includes(v))
        if (added.length)
          changes.push([
            ...here(),
            txt(' may now return new values '),
            ident(added.join(', ')),
          ])
      }
    } else if (dir === 'in' && !rEnum.length && lEnum.length) {
      // Previously any value, now restricted — rejects formerly-valid input.
      changes.push([
        ...here(),
        txt(' is now restricted to a fixed set of values'),
      ])
    } else if (dir === 'out' && rEnum.length && !lEnum.length) {
      // Previously a fixed set, now unbounded — exhaustive consumers break.
      changes.push([
        ...here(),
        txt(' is no longer limited to a fixed set of values'),
      ])
    }

    // Object properties.
    if (remote.properties || local.properties) {
      const rProps = remote.properties ?? {}
      const lProps = local.properties ?? {}
      const rReq = new Set(remote.required ?? [])
      const lReq = new Set(local.required ?? [])

      for (const key of Object.keys(rProps)) {
        const childLoc = loc ? `${loc}.${key}` : key
        if (!(key in lProps)) {
          changes.push([
            ...subject(fn, kind, childLoc),
            dir === 'in' ? txt(' removed') : txt(' no longer returned'),
          ])
          continue
        }
        // Required-ness tightening.
        if (dir === 'in' && lReq.has(key) && !rReq.has(key)) {
          changes.push([
            ...subject(fn, kind, childLoc),
            txt(' is now required'),
          ])
        }
        if (dir === 'out' && rReq.has(key) && !lReq.has(key)) {
          changes.push([
            ...subject(fn, kind, childLoc),
            txt(' may now be absent from the result'),
          ])
        }
        diffNode(fn, kind, childLoc, rProps[key], lProps[key], dir, changes)
      }

      for (const key of Object.keys(lProps)) {
        if (key in rProps) continue
        const childLoc = loc ? `${loc}.${key}` : key
        // New optional input / new output field → compatible. Only a new
        // *required* input breaks existing callers.
        if (dir === 'in' && lReq.has(key)) {
          changes.push([
            ident(fn),
            txt(' has a new required parameter '),
            ident(childLoc),
          ])
        }
      }
    }

    // Array element schema.
    if (remote.items || local.items) {
      diffNode(
        fn,
        kind,
        loc ? `${loc}[]` : '[]',
        remote.items ?? {},
        local.items ?? {},
        dir,
        changes
      )
    }
  }

  /**
   * Detect backwards-incompatible changes between the local functions
   * and the currently-deployed interface (fetched per-function from
   * jl4-service). New functions and new optional parameters are safe;
   * removals, renames, type/format changes, required-ness tightening and
   * enum narrowing are breaking — checked recursively at every depth.
   */
  function detectBreakingChanges(
    localFns: ExportedFunctionInfo[],
    remoteFns: RemoteFunctionSchema[]
  ): BreakingChange[] {
    const changes: BreakingChange[] = []
    const remoteByName = new Map(remoteFns.map((f) => [f.name, f]))

    for (const local of localFns) {
      const remote = remoteByName.get(local.name)
      if (!remote) continue // new function — not breaking

      // Return type (display name, e.g. BOOLEAN / DEONTIC). A change here
      // also covers deontic ⇄ non-deontic (different request envelope).
      const returnTypeChanged =
        !!remote.returnType &&
        !!local.returnType &&
        remote.returnType !== local.returnType
      if (returnTypeChanged) {
        changes.push([
          ident(local.name),
          txt(' return type changed from '),
          ident(remote.returnType as string),
          txt(' to '),
          ident(local.returnType),
        ])
      }

      // Recursive parameter (input) diff.
      diffNode(
        local.name,
        'parameter',
        '',
        (remote.parameters as unknown as SchemaNode) ?? {},
        local.parameters as unknown as SchemaNode,
        'in',
        changes
      )

      // Recursive return-value (output) diff — mirror image of inputs:
      // a removed/renamed output field or a narrowed output type breaks
      // existing consumers. Only meaningful when the return type itself
      // is unchanged and both sides expose a structured schema.
      if (!returnTypeChanged && remote.returnSchema && local.returnSchema) {
        diffNode(
          local.name,
          'return value',
          '',
          remote.returnSchema as unknown as SchemaNode,
          local.returnSchema as unknown as SchemaNode,
          'out',
          changes
        )
      }
    }

    // Removed functions.
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

    // Check for breaking changes against the currently-deployed interface.
    try {
      const { functions: remoteFns } = await messenger.sendRequest(
        GetSidebarDeploymentSchemas,
        HOST_EXTENSION,
        { deploymentId: id }
      )
      // `null` ⇒ deployment doesn't exist yet ⇒ first deploy, nothing to break.
      if (remoteFns) {
        const changes = detectBreakingChanges(functions, remoteFns)
        if (changes.length > 0) {
          breakingChanges = changes
          verifying = false
          deployView = 'breaking-warning'
          return
        }
      }
    } catch {
      // Schema fetch failed — don't block the deploy on a detection error.
    }

    verifying = false
    await executeDeploy(id)
  }

  async function deployAnyway() {
    // User reviewed the breaking changes and chose to proceed: overwrite
    // the existing deployment via POST (ungated), bypassing the PUT gate.
    await executeDeploy(sanitizeDeploymentId(deploymentIdInput), true)
  }

  async function executeDeploy(deploymentId: string, overwrite = false) {
    if (!messenger || !activeFileUri) return
    deploying = true
    try {
      const result = await messenger.sendRequest(
        RequestSidebarDeploy,
        HOST_EXTENSION,
        {
          deploymentId,
          fileUri: activeFileUri,
          mission: deploymentMission.trim() || undefined,
          overwrite,
        }
      )
      if (result.success) {
        const did = result.deploymentId ?? deploymentId
        // No updateId ⇒ resolved immediately (content-hash dedupe).
        // Otherwise poll the async deploy/update job to a terminal state.
        let outcome: 'applied' | 'rejected' | 'pending' = result.updateId
          ? 'pending'
          : 'applied'
        let error: string | undefined
        if (result.updateId) {
          for (let i = 0; i < 60; i++) {
            await new Promise((r) => setTimeout(r, 1000))
            try {
              const resp = await messenger.sendRequest(
                GetSidebarUpdateStatus,
                HOST_EXTENSION,
                { deploymentId: did, updateId: result.updateId }
              )
              error = resp.error
              if (resp.status === 'applied') {
                outcome = 'applied'
                break
              }
              if (resp.status === 'rejected') {
                outcome = 'rejected'
                break
              }
            } catch {
              // ignore transient errors
            }
          }
        }
        deploying = false
        if (outcome === 'applied') {
          // Switch to the deployments tab *before* awaiting the list
          // fetch, otherwise the deploy panel repaints its Preview
          // screen for the duration of the round-trip (visible flicker).
          deployView = 'preview'
          activeTab = 'deployments'
          notify('info', `Deployed "${did}" successfully.`)
          await fetchDeployments()
        } else if (outcome === 'rejected') {
          deployView = 'preview'
          notify(
            'error',
            `Deploy "${did}" rejected: ${error ?? 'compilation error'}`
          )
        } else {
          deployView = 'preview'
          activeTab = 'deployments'
          notify(
            'warning',
            `Deploying "${did}" — still in progress. Refresh later.`
          )
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
      // Deployments are collapsed by default; user can expand individually.
      collapsedDeployments = new Set(deployments.map((d) => d.deploymentId))
    } catch {
      deployments = []
    } finally {
      deploymentsLoading = false
    }
  }

  function toggleDeploymentMenu(deploymentId: string) {
    openDeploymentMenuId =
      openDeploymentMenuId === deploymentId ? null : deploymentId
  }

  function closeDeploymentMenu() {
    openDeploymentMenuId = null
  }

  function requestUndeploy(dep: SidebarDeploymentInfo) {
    undeployConfirm = dep
  }

  async function requestDownload(dep: SidebarDeploymentInfo) {
    if (!messenger) return
    closeDeploymentMenu()
    downloadingId = dep.deploymentId
    try {
      const res = await messenger.sendRequest(
        RequestSidebarDownloadDeployment,
        HOST_EXTENSION,
        { deploymentId: dep.deploymentId }
      )
      if (res.cancelled) return
      if (res.success) {
        notify(
          'info',
          `Saved ${res.fileCount} file${res.fileCount === 1 ? '' : 's'} to ${res.folderPath}`
        )
      } else {
        notify('error', res.error ?? `Failed to download "${dep.deploymentId}"`)
      }
    } catch (err) {
      notify(
        'error',
        err instanceof Error
          ? err.message
          : `Failed to download "${dep.deploymentId}"`
      )
    } finally {
      downloadingId = null
    }
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
      } else if (deployView === 'mission') {
        continueDeploy()
      } else if (deployView === 'deploy-form') {
        goToMission()
      } else if (activeTab === 'deployments' && deployments.length > 0) {
        openServiceUrl()
      } else if (
        activeTab === 'ai-chat' ||
        activeTab === 'docs' ||
        activeTab === 'inspector'
      ) {
        // "Preview" click jumps to the Deploy tab so the cards the
        // button promised become visible. The Deploy tab's own
        // footer action then reverts to the regular "Deploy" flow.
        activeTab = 'preview'
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

  function addL4ToolsToClaudeCode() {
    messenger?.sendNotification(
      RequestAddL4ToolsToClaudeCode,
      HOST_EXTENSION,
      undefined as never
    )
  }

  function installL4Cli() {
    messenger?.sendNotification(
      RequestInstallL4Cli,
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
      class:active={activeTab === 'ai-chat'}
      onclick={() => (activeTab = 'ai-chat')}
    >
      Legalese AI
    </button>
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
    <div class="tab-pane" hidden={activeTab !== 'ai-chat'}>
      <AiChatPanel {messenger} visible={activeTab === 'ai-chat'} />
    </div>
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
                    onclick={() => selectExistingDeployment(dep.deploymentId)}
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
      {:else if deployView === 'mission'}
        <!-- Deployment metadata (step 2) -->
        <DeploymentMetadata
          bind:mission={deploymentMission}
          deploymentId={sanitizeDeploymentId(deploymentIdInput)}
          heading="Deployment metadata"
          onBack={() => (deployView = 'deploy-form')}
        />
      {:else if deployView === 'breaking-warning'}
        <!-- Breaking change warning -->
        <div class="breaking-warning">
          <button class="back-btn" onclick={() => (deployView = 'mission')}
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
                onReveal={func.srcLine
                  ? () =>
                      messenger?.sendNotification(
                        RequestRevealLocation,
                        HOST_EXTENSION,
                        { uri: activeFileUri, line: func.srcLine! }
                      )
                  : undefined}
              />
            {/each}
          </div>
        {/if}
      {/if}
    {/if}
    {#if activeTab === 'deployments'}
      <div class="deployments-tab-wrapper">
        <div class="deployments-tab-body">
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
                  Connect to {stripProtocol(connectionStatus.serviceUrl)} to view
                  deployments.
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
                {@const canDownload =
                  !!dep.hasFiles &&
                  (dep.status === 'ready' || dep.status === 'pending')}
                {@const canUndeploy = true}
                {@const showMenu = canDownload || canUndeploy}
                <div
                  class="deployment-group"
                  class:collapsed={collapsedDeployments.has(dep.deploymentId)}
                >
                  <div class="deployment-header">
                    <button
                      class="deployment-header-toggle"
                      onclick={() => toggleDeploymentCollapse(dep.deploymentId)}
                      title={collapsedDeployments.has(dep.deploymentId)
                        ? 'Expand deployment'
                        : 'Collapse deployment'}
                    >
                      <span
                        class="chevron"
                        class:rotated={!collapsedDeployments.has(
                          dep.deploymentId
                        )}>&#9002;</span
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
                    </button>
                    {#if showMenu}
                      <div class="deployment-menu-wrapper">
                        <button
                          class="deployment-menu-btn"
                          aria-label="Deployment actions"
                          aria-haspopup="menu"
                          aria-expanded={openDeploymentMenuId ===
                            dep.deploymentId}
                          title="Deployment actions"
                          onclick={(e: MouseEvent) => {
                            e.stopPropagation()
                            toggleDeploymentMenu(dep.deploymentId)
                          }}
                        >
                          {#if downloadingId === dep.deploymentId || undeployingId === dep.deploymentId}
                            <span class="menu-spinner" aria-hidden="true"
                            ></span>
                          {:else}
                            &#8943;
                          {/if}
                        </button>
                        {#if openDeploymentMenuId === dep.deploymentId}
                          <div class="dropdown-menu deployment-dropdown-menu">
                            {#if canDownload}
                              <button
                                class="menu-item"
                                disabled={downloadingId === dep.deploymentId}
                                onclick={(e: MouseEvent) => {
                                  e.stopPropagation()
                                  requestDownload(dep)
                                }}
                              >
                                {downloadingId === dep.deploymentId
                                  ? 'Downloading...'
                                  : 'Download'}
                              </button>
                            {/if}
                            {#if canUndeploy}
                              <button
                                class="menu-item menu-item-danger"
                                disabled={undeployingId === dep.deploymentId}
                                onclick={(e: MouseEvent) => {
                                  e.stopPropagation()
                                  closeDeploymentMenu()
                                  requestUndeploy(dep)
                                }}
                              >
                                {undeployingId === dep.deploymentId
                                  ? 'Removing...'
                                  : 'Undeploy'}
                              </button>
                            {/if}
                          </div>
                        {/if}
                      </div>
                    {/if}
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
                </div>
              {/each}
            </div>
          {/if}
        </div>
        <aside class="deployment-info-note" role="note">
          <p>
            Deployments are automatically available to Legalese AI, VS Code
            Copilot, and any other MCP-speaking agent as local MCP tools.
          </p>
          <p>
            They're also available as REST API's, online MCP server and WebMCP
            tools {connectionStatus.isLegaleseCloud
              ? 'on the Legalese Cloud'
              : 'via the connected JL4 service'}. Open the deployments in the
            web browser to learn more.
          </p>
        </aside>
      </div>
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
            <button
              class="menu-item"
              onclick={menuAction(openExtensionSettings)}
            >
              Extension Settings
            </button>
            <button class="menu-item" onclick={menuAction(installL4Cli)}>
              Install L4 CLI
            </button>
            <button
              class="menu-item"
              onclick={menuAction(addL4ToolsToClaudeCode)}
            >
              Add L4 Tools to Claude Code
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
                <button class="menu-item" onclick={menuAction(disconnect)}>
                  Disconnect
                </button>
              {/if}
              {#if connectionStatus.isLegaleseCloud}
                <div class="menu-separator"></div>
                <button class="menu-item" onclick={menuAction(openConsole)}>
                  Legalese Cloud Console
                </button>
                <button class="menu-item" onclick={menuAction(signOut)}>
                  Sign out
                </button>
              {/if}
            {:else if connectionStatus.status === 'connecting'}
              <button class="menu-item" onclick={menuAction(disconnect)}>
                Disconnect
              </button>
            {:else}
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
          No L4 file open
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
    gap: 8px;
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
    border-color: var(--vscode-foreground, #ccc);
  }

  .form-textarea {
    font-family: inherit;
    font-size: inherit;
    line-height: 1.4;
    resize: vertical;
    min-height: 56px;
  }

  .form-textarea:disabled {
    opacity: 0.6;
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

  .deployments-tab-wrapper {
    display: flex;
    flex-direction: column;
    min-height: 100%;
  }

  .deployments-tab-body {
    flex: 1 0 auto;
    padding-bottom: 24px;
  }

  .deployment-info-note {
    flex-shrink: 0;
    margin-top: auto;
    margin-bottom: 2px;
    padding: 16px 18px;
    background: var(
      --vscode-textBlockQuote-background,
      rgba(127, 127, 127, 0.1)
    );
    border-radius: 4px;
    font-size: 0.82em;
    line-height: 1.45;
    color: var(--vscode-descriptionForeground);
  }

  .deployment-info-note > :global(p) {
    margin: 0;
  }

  .deployment-info-note > :global(p + p) {
    margin-top: 6px;
  }

  .deployments-list {
    display: flex;
    flex-direction: column;
    gap: 16px;
    padding-bottom: 16px;
  }

  .deployment-group {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .deployment-group.collapsed {
    margin-bottom: -8px;
  }

  .deployment-header {
    display: flex;
    align-items: center;
    background: var(--vscode-sideBarSectionHeader-background, #252526);
    user-select: none;
    font-size: 0.92em;
    font-weight: 500;
  }

  .deployment-header-toggle {
    display: flex;
    align-items: center;
    gap: 6px;
    flex: 1;
    min-width: 0;
    padding: 4px 5px;
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    font-family: inherit;
    font-size: inherit;
    font-weight: inherit;
    text-align: left;
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
    border: 1px solid var(--vscode-panel-border, #444);
    border-left: 3px solid var(--vscode-testing-iconFailed, #f14c4c);
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

  .deployment-menu-wrapper {
    position: relative;
    flex-shrink: 0;
  }

  .deployment-menu-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 22px;
    height: 22px;
    padding: 0;
    margin: 0 2px;
    border: none;
    border-radius: 3px;
    background: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    font-size: 1.2em;
    line-height: 1;
    opacity: 0.5;
  }

  .deployment-menu-btn:hover,
  .deployment-menu-btn[aria-expanded='true'] {
    opacity: 1;
    background: var(
      --vscode-toolbar-hoverBackground,
      rgba(255, 255, 255, 0.08)
    );
  }

  /* Combined selector to outweigh `.dropdown-menu` (defined later in
   * this stylesheet for the status-footer menu, which anchors
   * bottom-left). The deployment menu must instead drop *below* its
   * trigger and align to the trigger's right edge so it expands
   * leftward and stays inside the sidebar panel.
   *
   * Background / border / shadow are repeated here (rather than
   * relying on the base `.dropdown-menu` cascade) so this menu
   * visually matches the connection-status footer dropdown
   * unconditionally, regardless of stylesheet ordering. */
  .dropdown-menu.deployment-dropdown-menu {
    position: absolute;
    top: calc(100% + 2px);
    bottom: auto;
    right: 0;
    left: auto;
    min-width: 140px;
    background: var(--vscode-menu-background, #252526);
    border: 1px solid var(--vscode-menu-border, #454545);
    border-radius: 4px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
    z-index: 100;
    padding: 4px 0;
  }

  .menu-item:disabled {
    opacity: 0.5;
    cursor: default;
  }

  .menu-item-danger:not(:disabled):hover {
    color: #f14c4c;
    background: var(--vscode-menu-selectionBackground, #04395e);
  }

  .menu-spinner {
    display: inline-block;
    width: 10px;
    height: 10px;
    border: 1.5px solid currentColor;
    border-right-color: transparent;
    border-radius: 50%;
    animation: menu-spin 0.7s linear infinite;
    opacity: 0.7;
  }

  @keyframes menu-spin {
    to {
      transform: rotate(360deg);
    }
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
    box-sizing: border-box;
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
