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
    RequestOpenServiceUrl,
    RequestOpenConsole,
    RequestInstallMarketplace,
    RequestDownloadMarketplaceSkill,
    RequestInstallDeploymentSkill,
    RequestInstallL4Cli,
    RequestCopySignInLink,
    RequestDisconnect,
    SidebarConnectionStatusChanged,
    ListSidebarDeployments,
    RequestSidebarDeploy,
    RequestSidebarUndeploy,
    RequestSidebarDownloadDeployment,
    GetSidebarDeploymentSchemas,
    GenerateSidebarIntendedUse,
    GetSidebarDeploymentStatus,
    GetSidebarUpdateStatus,
    ShowNotification,
    RequestRenderPreview,
    RequestRenderInline,
    RequestRenderSave,
    GetSidebarImportedFiles,
    AiChatPickAttachment,
    RequestRevealLocation,
    type WebviewFrontendIsReadyMessage,
    type ExportedFunctionInfo,
    type GetSidebarConnectionStatusResponse,
    type SidebarDeploymentInfo,
    type SidebarImportedFile,
    type RemoteFunctionSchema,
    type AiChatAttachment,
    type Harness,
  } from 'jl4-client-rpc'
  import type { WebviewApi } from 'vscode-webview'
  import HarnessInstallMenu from '$lib/components/harness-install-menu.svelte'
  import ToolCard from '$lib/components/tool-card.svelte'
  import InspectorPanel from '$lib/components/inspector-panel.svelte'
  import DeploymentMetadata from '$lib/components/deployment-metadata.svelte'
  import DocsPanel from '$lib/components/docs-panel.svelte'
  import AiChatPanel from '$lib/components/ai/ai-chat-panel.svelte'
  import DeploymentIntegratePopover from '$lib/components/deployment-integrate-popover.svelte'
  import CloudUpsell from '$lib/components/cloud-upsell.svelte'

  let functions: ExportedFunctionInfo[] = $state([])
  let activeFileUri: string = $state('')
  let activeFileName: string = $state('')
  let connectionStatus: GetSidebarConnectionStatusResponse = $state({
    serviceUrl: '',
    connected: false,
    status: 'connecting',
    isLegaleseCloud: false,
    mcpPort: 19415,
  })
  let initialized: boolean = $state(false)
  let previewDebounceTimer: ReturnType<typeof setTimeout> | null = null
  let activeTab:
    | 'ai-chat'
    | 'docs'
    | 'inspector'
    | 'render'
    | 'preview'
    | 'deployments' = $state('docs')
  let menuOpen: boolean = $state(false)

  // Render tab state
  let renderIncludeUnused: boolean = $state(false)
  let renderNumberSections: boolean = $state(true)
  let renderNumberClauses: boolean = $state(true)
  let renderToc: boolean = $state(false)
  let renderBusy: boolean = $state(false)
  // "Save & refine" flow: the deterministic render is refined through the
  // legalese-compose-4 pipeline using these formatting preferences /
  // drafting policy. The format below is what the refined document is saved
  // as. Preferences and the policy reference persist in localStorage.
  const AI_PREFS_KEY = 'l4.render.ai.formatPrefs'
  const AI_POLICY_KEY = 'l4.render.ai.draftingPolicy'
  // Document format for the "Save & refine" flow (document formats only —
  // the JSON/plan exports aren't documents to redraft).
  let renderRefineFormat: 'html' | 'akn' | 'text' = $state('html')
  let renderFormatPrefs: string = $state('')
  // The drafting-policy file as a ready-to-stage chat attachment (the
  // picker returns this shape for both text and PDF).
  let draftingPolicy: AiChatAttachment | null = $state(null)
  // Hand-off to the AI chat panel: set after a render when the user
  // asked to refine, consumed by AiChatPanel's refineRequest effect.
  let renderRefineRequest: {
    prompt: string
    attachment: AiChatAttachment | null
    files: Array<{ name: string; path: string }>
    nonce: number
  } | null = $state(null)
  // A valid Legalese Cloud session — gates the AI rendering controls.
  const cloudSessionValid: boolean = $derived(
    connectionStatus.isLegaleseCloud && connectionStatus.connected
  )
  // Imports checklist: the modules the active file pulls in, and the set
  // the user has deselected (excluded) from the render. Default: all
  // imports excluded — a URI present in `excludedModules` is left out.
  let importedFiles: SidebarImportedFile[] = $state([])
  let excludedModules: Set<string> = $state(new Set())
  let importsLoading: boolean = $state(false)

  // Live inline HTML preview: the rendered HTML for the iframe `srcdoc`,
  // re-fetched (debounced) whenever the active file, its in-memory version
  // (edits), or a render option changes. Never touches disk.
  let renderHtml: string = $state('')
  let renderInlineError: string = $state('')
  let renderInlineBusy: boolean = $state(false)
  let renderInlineTimer: ReturnType<typeof setTimeout> | null = null
  // The preview iframe element + its last-known scroll position, so a live
  // re-render (which replaces `srcdoc` and reloads the frame) can restore
  // where the reader was rather than jumping to the top.
  let renderFrameEl: HTMLIFrameElement | null = $state(null)
  let renderScrollX = 0
  let renderScrollY = 0

  // On each (re)load of the preview: restore the saved scroll position,
  // start tracking further scrolls, and intercept in-document link clicks.
  // The frame is same-origin (sandbox allows it), so contentWindow /
  // contentDocument are reachable.
  function onRenderFrameLoad() {
    const win = renderFrameEl?.contentWindow
    const doc = renderFrameEl?.contentDocument
    if (!win) return
    win.scrollTo(renderScrollX, renderScrollY)
    win.addEventListener(
      'scroll',
      () => {
        renderScrollX = win.scrollX
        renderScrollY = win.scrollY
      },
      { passive: true }
    )
    // Links in the rendered document are all in-document anchors. Inside a
    // sandboxed `srcdoc` frame a fragment jump can't navigate (it reloads
    // `about:srcdoc` as a blank page), so intercept every anchor click and
    // keep it within the frame: scroll the `#…` target into view.
    doc?.addEventListener('click', (ev) => {
      const anchor = (ev.target as HTMLElement | null)?.closest?.('a')
      if (!anchor) return
      ev.preventDefault()
      const href = anchor.getAttribute('href') ?? ''
      if (!href.startsWith('#')) return
      const id = decodeURIComponent(href.slice(1))
      const targetEl =
        doc.getElementById(id) ??
        doc.querySelector(`[name="${CSS.escape(id)}"]`)
      targetEl?.scrollIntoView({ behavior: 'smooth', block: 'start' })
    })
  }
  // The active file's in-memory version — bumped on every edit (via the
  // active-file message), which drives the live re-render.
  let activeFileVersion: number = $state(0)
  // Floating-bar UI state: which popup/tray (if any) is open, and whether a
  // Save is in flight.
  let saveMenuOpen: boolean = $state(false)
  let saveBusy: boolean = $state(false)
  let refineTrayOpen: boolean = $state(false)
  let importsTrayOpen: boolean = $state(false)
  // Light/dark theme for the rendered iframe. The generated HTML is
  // hardcoded light, so dark mode injects an override stylesheet. Defaults
  // to VSCode's current theme (resolved in onMount), then user-overridable.
  let renderDark: boolean = $state(false)

  const RENDER_HELP_URL =
    'https://legalese.com/l4/tutorials/natural-language-functions/optimising-natural-language-generation.md'

  // The webview's editor background — the dark page matches it so the
  // rendered document sits on the same surface as the empty-state and the
  // surrounding editor. The srcdoc can't read the parent's CSS vars, so we
  // resolve it to a literal at inject time.
  function editorBg(): string {
    try {
      const v = getComputedStyle(document.documentElement)
        .getPropertyValue('--vscode-editor-background')
        .trim()
      return v || '#181818'
    } catch {
      return '#181818'
    }
  }

  // Dark-mode override for the rendered document. The exported HTML themes
  // itself with `--ink`/`--muted` vars plus a few literal light backgrounds
  // (jl4-core/src/L4/Export/Render.hs); these rules invert those for a dark
  // page. Injected after the document's own <style>, so equal-specificity
  // rules win by source order.
  function darkOverrideCss(bg: string): string {
    return `
    :root { --ink:#e6e6e6; --muted:#9d9d9d; }
    html { color-scheme: dark; }
    body { background:${bg}; }
    .l4-doc { background:${bg}; }
    .forced { color:#e0726f; }
    .cond { color:#c3c3c3; }
    nav.toc { background:rgba(255,255,255,0.04); border-color:rgba(255,255,255,0.12); }
    table.l4-table th, table.l4-table td { border-color:rgba(255,255,255,0.14); }
    table.l4-table th { background:rgba(255,255,255,0.07); }
    table.l4-table tbody tr:nth-child(even) { background:rgba(255,255,255,0.03); }
    @media print { body, .l4-doc { background:#fff; } }
  `
  }

  // True when VSCode is using a dark (or high-contrast dark) theme. In a
  // webview, `prefers-color-scheme` tracks the editor theme.
  function prefersDark(): boolean {
    try {
      return window.matchMedia('(prefers-color-scheme: dark)').matches
    } catch {
      return false
    }
  }

  // Splice the dark override into the rendered HTML's <head> (falling back
  // to a prefix when there's no head).
  function applyTheme(html: string): string {
    if (!renderDark || !html) return html
    const tag = `<style data-l4-dark>${darkOverrideCss(editorBg())}</style>`
    return html.includes('</head>')
      ? html.replace('</head>', `${tag}</head>`)
      : tag + html
  }

  // The HTML actually fed to the iframe, themed per the toggle.
  const displayHtml: string = $derived(applyTheme(renderHtml))

  // Export formats offered by the Save dropdown. The live preview is always
  // HTML; the format only chooses what the Save action persists.
  const SAVE_FORMATS: Array<{ value: string; label: string }> = [
    { value: 'html', label: 'HTML document' },
    { value: 'akn', label: 'Akoma Ntoso (XML)' },
    { value: 'text', label: 'Plain text' },
    { value: 'json', label: 'Document IR (JSON)' },
    { value: 'plan', label: 'Export plan (JSON)' },
  ]

  async function fetchImportedFiles() {
    if (!messenger) return
    importsLoading = true
    try {
      const res = await messenger.sendRequest(
        GetSidebarImportedFiles,
        HOST_EXTENSION,
        undefined as never
      )
      // Newly-discovered imports default to excluded; a prior include/exclude
      // decision for an already-known URI is preserved across re-fetches (tab
      // re-entry, file switches with shared imports). Stale URIs are dropped.
      const prevKnown = new Set(importedFiles.map((f) => f.uri))
      importedFiles = res.files
      excludedModules = new Set(
        res.files
          .map((f) => f.uri)
          .filter((uri) => !prevKnown.has(uri) || excludedModules.has(uri))
      )
    } catch {
      importedFiles = []
    } finally {
      importsLoading = false
    }
  }

  function toggleModule(uri: string) {
    const next = new Set(excludedModules)
    if (next.has(uri)) next.delete(uri)
    else next.add(uri)
    excludedModules = next
  }

  // Refresh the imports checklist when the Render tab is shown or the
  // active file changes underneath it.
  $effect(() => {
    if (activeTab === 'render' && activeFileUri && messenger) {
      fetchImportedFiles()
    }
  })

  // Restore persisted AI formatting preferences / drafting policy.
  function loadAiPrefs() {
    try {
      renderFormatPrefs = localStorage.getItem(AI_PREFS_KEY) ?? ''
      const raw = localStorage.getItem(AI_POLICY_KEY)
      draftingPolicy = raw ? JSON.parse(raw) : null
    } catch {
      // localStorage unavailable / corrupt — start clean.
    }
  }

  // Persist on every change (cheap; these are small strings).
  $effect(() => {
    try {
      localStorage.setItem(AI_PREFS_KEY, renderFormatPrefs)
      if (draftingPolicy) {
        localStorage.setItem(AI_POLICY_KEY, JSON.stringify(draftingPolicy))
      } else {
        localStorage.removeItem(AI_POLICY_KEY)
      }
    } catch {
      // ignore quota / availability errors
    }
  })

  // Pick a drafting-policy document (text or PDF) via the extension's
  // native file picker, reusing the AI chat attachment flow.
  async function pickDraftingPolicy() {
    if (!messenger) return
    try {
      const res = await messenger.sendRequest(
        AiChatPickAttachment,
        HOST_EXTENSION,
        { accept: 'text-or-pdf' }
      )
      if (res.note) notify('info', res.note)
      if (res.attachment) draftingPolicy = res.attachment
    } catch (e) {
      notify('error', e instanceof Error ? e.message : String(e))
    }
  }

  function clearDraftingPolicy() {
    draftingPolicy = null
  }

  // file:// URI → filesystem path the AI agent's fs tools can read.
  function fsPathFromUri(uri: string): string {
    return decodeURIComponent(uri.replace(/^file:\/\//, ''))
  }

  function baseName(p: string): string {
    return p.split('/').pop() ?? p
  }

  // Human label for the selected format, used in the refine prompt.
  function formatLabel(fmt: string): string {
    return fmt === 'akn'
      ? 'Akoma Ntoso XML'
      : fmt === 'text'
        ? 'plain-text'
        : 'HTML'
  }

  // The refinement prompt is deliberately short: the L4 source and the
  // generated document ride along as `@`-mention chips (the agent reads
  // them with its fs tools), so no paths are spelled out here.
  function buildRefinePrompt(): string {
    const fmt = formatLabel(renderRefineFormat)
    const lines = [
      `Refine the formatting and wording of the generated ${fmt} document, using the L4 source only as context. Preserve the defined terms, rules, values and scope exactly — change presentation only, never the substance.`,
    ]
    if (renderFormatPrefs.trim()) {
      lines.push(``, `Formatting preferences:`, renderFormatPrefs.trim())
    }
    if (draftingPolicy) {
      lines.push(``, `Apply the attached drafting policy.`)
    }
    lines.push(``, `Save the result as a new copy next to the original.`)
    return lines.join('\n')
  }

  // After a successful render with "Refine with Legalese AI" on, open a
  // fresh AI chat conversation seeded with the refine prompt, the two
  // documents as chips, and the policy attachment, then switch tabs so
  // the agent starts immediately.
  function startAiRefinement(generatedPath: string) {
    if (!activeFileUri) return
    const sourcePath = fsPathFromUri(activeFileUri)
    renderRefineRequest = {
      prompt: buildRefinePrompt(),
      attachment: draftingPolicy,
      files: [
        { name: baseName(sourcePath), path: sourcePath },
        { name: baseName(generatedPath), path: generatedPath },
      ],
      nonce: ++actionNonce,
    }
    activeTab = 'ai-chat'
  }

  // Fetch the live HTML render of the active file (reflecting the current
  // toggles and in-memory edits) and drop it into the iframe. Never writes
  // a file. Errors surface as an overlay rather than a toast — they're
  // transient while the user is mid-edit.
  async function refreshInlineRender() {
    if (!messenger || !activeFileUri) {
      renderHtml = ''
      return
    }
    renderInlineBusy = true
    try {
      const res = await messenger.sendRequest(
        RequestRenderInline,
        HOST_EXTENSION,
        {
          includeUnused: renderIncludeUnused,
          numberSections: renderNumberSections,
          numberClauses: renderNumberClauses,
          toc: renderToc,
          excludeModules: [...excludedModules],
        }
      )
      if (res.success) {
        renderHtml = res.html ?? ''
        renderInlineError = ''
      } else {
        renderInlineError = res.error ?? 'Render failed.'
      }
    } catch (e) {
      renderInlineError = e instanceof Error ? e.message : String(e)
    } finally {
      renderInlineBusy = false
    }
  }

  // Debounce live re-renders so rapid edits / toggle flips coalesce.
  function scheduleInlineRender() {
    if (renderInlineTimer) clearTimeout(renderInlineTimer)
    renderInlineTimer = setTimeout(refreshInlineRender, 180)
  }

  // Re-render the preview whenever the active file, its version (edits), or
  // any render option changes — while the Render tab is open.
  $effect(() => {
    // Read every dependency so Svelte re-runs this effect on their change.
    const _deps = [
      activeTab,
      activeFileUri,
      activeFileVersion,
      renderNumberSections,
      renderNumberClauses,
      renderToc,
      renderIncludeUnused,
      excludedModules,
    ]
    void _deps
    if (activeTab === 'render' && activeFileUri && messenger) {
      scheduleInlineRender()
    }
  })

  // Save the rendered document in the chosen format. The extension renders
  // and shows a native Save dialog for the location.
  async function saveAs(format: string) {
    saveMenuOpen = false
    if (!messenger || !activeFileUri || saveBusy) return
    saveBusy = true
    try {
      const res = await messenger.sendRequest(
        RequestRenderSave,
        HOST_EXTENSION,
        {
          format,
          includeUnused: renderIncludeUnused,
          numberSections: renderNumberSections,
          numberClauses: renderNumberClauses,
          toc: renderToc,
          excludeModules: [...excludedModules],
        }
      )
      if (res.canceled) return
      if (!res.success) {
        notify('error', res.error ?? 'Save failed.')
        return
      }
      const savedName = res.savedPath
        ? (res.savedPath.split('/').pop() ?? res.savedPath)
        : undefined
      notify('info', savedName ? `Saved ${savedName}` : 'Saved.')
    } catch (e) {
      notify('error', e instanceof Error ? e.message : String(e))
    } finally {
      saveBusy = false
    }
  }

  // Open/close the "Save & refine" tray (signed out shows the sign-in
  // upsell inside it).
  function toggleRefineTray() {
    refineTrayOpen = !refineTrayOpen
    if (refineTrayOpen) importsTrayOpen = false
  }

  function toggleImportsTray() {
    importsTrayOpen = !importsTrayOpen
    if (importsTrayOpen) refineTrayOpen = false
  }

  // Run "Save & refine": render the deterministic document (in the chosen
  // format) to a file beside the source, then hand off to the AI chat panel
  // seeded with the refine prompt and the two documents.
  async function runRefine() {
    if (!messenger || !activeFileUri) return
    if (!cloudSessionValid) {
      handleAction()
      return
    }
    renderBusy = true
    try {
      const res = await messenger.sendRequest(
        RequestRenderPreview,
        HOST_EXTENSION,
        {
          format: renderRefineFormat,
          includeUnused: renderIncludeUnused,
          numberSections: renderNumberSections,
          numberClauses: renderNumberClauses,
          toc: renderToc,
          excludeModules: [...excludedModules],
          openInEditor: false,
        }
      )
      if (!res.success) {
        notify('error', res.error ?? 'Render failed.')
        return
      }
      if (res.savedPath) {
        refineTrayOpen = false
        startAiRefinement(res.savedPath)
      } else {
        notify(
          'warning',
          'Save the L4 file to disk to refine the render with Legalese AI.'
        )
      }
    } catch (e) {
      notify('error', e instanceof Error ? e.message : String(e))
    } finally {
      renderBusy = false
    }
  }

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

  // Ask the extension to draft the "Intended use" text from the
  // exported function schemas via the summize model. Returns null when
  // the field should be left untouched: no messenger yet, or the user
  // isn't signed in (the extension already showed the sign-in nudge).
  async function generateIntendedUse(): Promise<string | null> {
    if (!messenger) return null
    if (functions.length === 0) {
      notify('warning', 'No exported functions to describe yet.')
      return null
    }
    try {
      const res = await messenger.sendRequest(
        GenerateSidebarIntendedUse,
        HOST_EXTENSION,
        // `functions` is a Svelte 5 $state proxy; structured-clone
        // (postMessage) can't serialize the reactive proxy, so unwrap
        // to a plain snapshot before crossing the RPC boundary.
        { functions: $state.snapshot(functions) }
      )
      if ('notSignedIn' in res) return null
      if ('error' in res) {
        notify('error', res.error)
        return null
      }
      return res.text
    } catch (err) {
      notify(
        'error',
        `Could not generate description: ${err instanceof Error ? err.message : String(err)}`
      )
      return null
    }
  }

  // Deployments tab state
  let deployments: SidebarDeploymentInfo[] = $state([])
  let deploymentsLoading: boolean = $state(false)
  // Free-text filter over deployment names (matched as you type).
  let deploymentFilter: string = $state('')
  // Sort order for the deployments list — case-insensitive by name.
  type DeploymentSortMode = 'az' | 'za'
  let deploymentSort: DeploymentSortMode = $state('az')
  let filteredDeployments: SidebarDeploymentInfo[] = $derived.by(() => {
    const query = deploymentFilter.trim().toLowerCase()
    const list = query
      ? deployments.filter((d) => d.deploymentId.toLowerCase().includes(query))
      : deployments.slice()
    list.sort((a, b) =>
      a.deploymentId.localeCompare(b.deploymentId, undefined, {
        sensitivity: 'base',
      })
    )
    return deploymentSort === 'za' ? list.reverse() : list
  })
  let undeployingId: string | null = $state(null)
  let downloadingId: string | null = $state(null)
  let openDeploymentMenuId: string | null = $state(null)
  let collapsedDeployments: Set<string> = $state(new Set())
  let undeployConfirm: SidebarDeploymentInfo | null = $state(null)
  // Which deployment's "Integrate" pop-over is open (deploymentId).
  let integrateForId: string | null = $state(null)
  // "Use in chat" hand-off to the AI panel. `nonce` makes a repeat
  // click on the same deployment re-trigger the panel-side effect.
  let deploymentChatRequest: {
    deploymentId: string
    apiBaseUrl: string
    intendedUse?: string
    nonce: number
  } | null = $state(null)
  // "Integrate → Learn more" deep-link into the in-app Docs tab.
  let docNav: { url: string; nonce: number } | null = $state(null)
  let actionNonce = 0

  // Cloud vs self-hosted shapes the integration affordances:
  //  - cloud: "Use in chat" is offered; Integrate shows the 4 cloud
  //    sections (incl. the OpenAI v1 endpoint).
  //  - self-hosted jl4-service: no "Use in chat"; Integrate shows the
  //    3 host-scoped sections (no OpenAI v1 endpoint).
  const integrateMode: 'cloud' | 'self-hosted' = $derived(
    connectionStatus.isLegaleseCloud ? 'cloud' : 'self-hosted'
  )

  function useInChat(dep: SidebarDeploymentInfo): void {
    const orgSlug = connectionStatus.orgSlug
    if (!connectionStatus.isLegaleseCloud || !orgSlug) {
      messenger?.sendNotification(ShowNotification, HOST_EXTENSION, {
        type: 'warning',
        message: 'Sign in with Legalese Cloud to use this feature',
      })
      return
    }
    integrateForId = null
    openDeploymentMenuId = null
    deploymentChatRequest = {
      deploymentId: dep.deploymentId,
      apiBaseUrl: `https://ai.legalese.cloud/${orgSlug}/${dep.deploymentId}`,
      ...(dep.description ? { intendedUse: dep.description } : {}),
      nonce: ++actionNonce,
    }
    activeTab = 'ai-chat'
  }

  function toggleIntegrate(dep: SidebarDeploymentInfo): void {
    // Self-hosted mode can render without an org slug; cloud mode
    // needs the verified org slug to build the URLs, so gate on it.
    if (integrateMode === 'cloud' && !connectionStatus.orgSlug) {
      messenger?.sendNotification(ShowNotification, HOST_EXTENSION, {
        type: 'warning',
        message: 'Sign in with Legalese Cloud to use this feature',
      })
      return
    }
    openDeploymentMenuId = null
    integrateForId =
      integrateForId === dep.deploymentId ? null : dep.deploymentId
  }

  function onLearnMore(url: string): void {
    integrateForId = null
    docNav = { url, nonce: ++actionNonce }
    activeTab = 'docs'
  }

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
      integrateForId = null
    }
  })

  // Dismiss the per-deployment dropdown when the user clicks anywhere
  // outside any deployment-menu wrapper. Listener is only attached
  // while a menu is open so we don't pay for it on every click.
  // Menu-only: the Integrate dialog is a modal that owns its own
  // dismissal (backdrop click / Esc) and lives outside
  // `.deployment-actions`, so this must not touch `integrateForId`
  // or it would close the dialog the instant the user clicks it.
  $effect(() => {
    if (openDeploymentMenuId === null) return
    const onPointerDown = (e: MouseEvent) => {
      const target = e.target as HTMLElement | null
      if (!target?.closest('.deployment-actions')) {
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
      // Tabs that aren't the Deploy tab surface the button as
      // "Deploy preview" — one click jumps to Deploy and shows the
      // tool cards (which is already the default Deploy-tab view).
      // Keeps the footer useful without forcing users to hunt for
      // the Deploy tab manually when they've authored an @export,
      // while the label makes the target action (a deploy, via the
      // preview step) clear. The Deployments tab behaves the same;
      // opening the live service in a browser lives in the deployment
      // menu ("Visit …") instead.
      if (
        activeTab === 'ai-chat' ||
        activeTab === 'docs' ||
        activeTab === 'inspector' ||
        activeTab === 'deployments'
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
    // Non-deploy tabs (including Deployments) surface a "Deploy preview"
    // button that jumps to the Deploy tab. Enabled iff the active file has
    // at least one rule ready for export — otherwise the Deploy tab would
    // just show the empty "Open an L4 file containing valid rules" hint.
    if (
      activeTab === 'inspector' ||
      activeTab === 'docs' ||
      activeTab === 'ai-chat' ||
      activeTab === 'deployments'
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
   * Remove the DEONTIC simulation-envelope keys ('startTime', 'events')
   * from a parameter schema's top-level properties/required. These keys
   * are present in jl4-service's deployed schema but not in the LSP's
   * local schema, so they would otherwise drive false-positive diffs.
   * Returns the schema unchanged when not deontic or when no properties
   * are present.
   */
  function stripDeonticEnvelope(
    schema: SchemaNode | undefined,
    isDeontic: boolean
  ): SchemaNode | undefined {
    if (!isDeontic || !schema || !schema.properties) return schema
    const props = { ...schema.properties }
    delete props.startTime
    delete props.events
    const required = (schema.required ?? []).filter(
      (k) => k !== 'startTime' && k !== 'events'
    )
    return { ...schema, properties: props, required }
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

      // Return type (display name, e.g. BOOLEAN / DEONTIC OF P, A). A
      // change here also covers deontic ⇄ non-deontic (different request
      // envelope).
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

      // For DEONTIC functions, jl4-service injects 'startTime' and 'events'
      // into the deployed parameter schema (the simulation envelope), but
      // the LSP-derived local schema does not. Strip them on both sides
      // before diffing so the envelope's presence/absence doesn't show up
      // as a spurious "parameter removed" breaking change.
      const isDeontic =
        (remote.returnType ?? '').startsWith('DEONTIC') ||
        (local.returnType ?? '').startsWith('DEONTIC')
      const remoteParams = stripDeonticEnvelope(
        remote.parameters as unknown as SchemaNode,
        isDeontic
      )
      const localParams = stripDeonticEnvelope(
        local.parameters as unknown as SchemaNode,
        isDeontic
      )

      // Recursive parameter (input) diff.
      diffNode(
        local.name,
        'parameter',
        '',
        remoteParams ?? {},
        localParams ?? {},
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
      } else if (
        activeTab === 'ai-chat' ||
        activeTab === 'docs' ||
        activeTab === 'inspector' ||
        activeTab === 'deployments'
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

  // The global gateway "skills marketplace" — org-agnostic; org scope is
  // resolved from the user's sign-in. Shown to copy + offered as a
  // one-click install into each harness. The public GitHub repo is the form
  // claude.ai accepts (it only adds marketplaces from a GitHub repo).
  const MARKETPLACE_URL = 'https://github.com/legalese/cloud-rules'
  const MARKETPLACE_DOC =
    'https://legalese.com/l4/tutorials/legalese-cloud/agent-marketplace.md'

  function installMarketplace(harness: Harness) {
    messenger?.sendNotification(RequestInstallMarketplace, HOST_EXTENSION, {
      harness,
    })
  }

  // Save the gateway "skills marketplace" plugin (discovery skill + rules MCP)
  // as a zip — the download counterpart of the Install dropdown.
  function downloadMarketplaceSkill() {
    messenger?.sendNotification(
      RequestDownloadMarketplaceSkill,
      HOST_EXTENSION,
      undefined as never
    )
  }

  let marketplaceCopied = $state(false)
  async function copyMarketplaceUrl() {
    try {
      await navigator.clipboard.writeText(MARKETPLACE_URL)
      marketplaceCopied = true
      setTimeout(() => (marketplaceCopied = false), 1200)
    } catch {
      // Webviews can restrict clipboard; the input stays selectable.
    }
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
    loadAiPrefs()
    // Default the rendered-document theme to VSCode's current theme.
    renderDark = prefersDark()
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
        renderHtml = ''
        renderInlineError = ''
        if (previewDebounceTimer) clearTimeout(previewDebounceTimer)
        if (deployView !== 'preview') deployView = 'preview'
        undeployConfirm = null
      }

      if (msg?.type === 'l4-sidebar-active-file') {
        const uri = msg.uri as string
        const version = msg.version as number
        // A genuine file switch starts the preview at the top; edits to the
        // same file keep the reader's scroll position.
        if (uri !== activeFileUri) {
          renderScrollX = 0
          renderScrollY = 0
        }
        activeFileUri = uri
        activeFileVersion = version
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
      const target = e.target as HTMLElement
      if (menuOpen && !target.closest('.menu-wrapper')) {
        menuOpen = false
      }
      // Render-tab Save dropdown closes on any click outside its wrapper.
      if (saveMenuOpen && !target.closest('.render-save-wrap')) {
        saveMenuOpen = false
      }
      // Render-tab trays close on any click outside the tray itself and the
      // button that owns it — so clicking another icon-bar button (a toggle,
      // Save, help, …) dismisses the tray too. The owning toggle buttons
      // manage their own open/close in their handlers.
      if (
        (refineTrayOpen || importsTrayOpen) &&
        !target.closest('.render-tray') &&
        !target.closest('.render-tray-toggle')
      ) {
        refineTrayOpen = false
        importsTrayOpen = false
      }
    })

    // Clicks inside the preview iframe land in a separate document, so the
    // handler above never sees them — focus instead moves to the iframe and
    // blurs this window. Treat that as an outside click and close the
    // render menus/trays. (Native <select> popups keep focus on the
    // <select>, so opening the format dropdown won't trip this.)
    window.addEventListener('blur', () => {
      if (!saveMenuOpen && !refineTrayOpen && !importsTrayOpen) return
      setTimeout(() => {
        if (document.activeElement?.tagName === 'IFRAME') {
          saveMenuOpen = false
          refineTrayOpen = false
          importsTrayOpen = false
        }
      }, 0)
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
      class:active={activeTab === 'render'}
      onclick={() => (activeTab = 'render')}
    >
      Render
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

  <!-- Frame is the non-scrolling visible-area box: `.tab-content`
       scrolls inside it, while the Integrate overlay is a sibling of
       the scroller so it pins to the visible viewport (centred there,
       not at the middle of the scroll height). -->
  <div class="tab-content-frame">
    <div class="tab-content">
      <div class="tab-pane" hidden={activeTab !== 'ai-chat'}>
        <AiChatPanel
          {messenger}
          visible={activeTab === 'ai-chat'}
          {deploymentChatRequest}
          refineRequest={renderRefineRequest}
        />
      </div>
      <div class="tab-pane" hidden={activeTab !== 'docs'}>
        <DocsPanel {messenger} navTarget={docNav} />
      </div>
      <div class="tab-pane" hidden={activeTab !== 'inspector'}>
        <InspectorPanel {messenger} {onLearnMore} />
      </div>
      <div class="tab-pane render-pane" hidden={activeTab !== 'render'}>
        <div class="render-stage">
          <!-- Live preview: the active L4 file rendered to HTML, updated as
               you type. Always HTML; the format only matters when saving. -->
          {#if !activeFileUri}
            <div class="render-empty">
              <p>Open a valid L4 file to render it as a document.</p>
            </div>
          {:else if renderHtml}
            <iframe
              bind:this={renderFrameEl}
              class="render-frame"
              class:dark={renderDark}
              title="Rendered document preview"
              sandbox="allow-same-origin"
              srcdoc={displayHtml}
              onload={onRenderFrameLoad}
            ></iframe>
          {:else if renderInlineError}
            <div class="render-empty render-empty-error">
              <p>Couldn’t render this file.</p>
              <pre class="render-error-text">{renderInlineError}</pre>
            </div>
          {:else}
            <div class="render-empty"><p>Rendering…</p></div>
          {/if}

          {#if renderInlineBusy && renderHtml}
            <div class="render-progress" aria-hidden="true">
              <div class="render-progress-fill"></div>
            </div>
          {/if}

          <!-- Refine tray — floats directly above the icon bar. -->
          {#if refineTrayOpen}
            <div class="render-tray refine-tray">
              {#if cloudSessionValid}
                <div class="render-tray-title">Refine with Legalese AI</div>
                <p class="render-tray-hint">
                  Describe your formatting preferences and/or load a drafting
                  policy, then save and refine the document with Legalese AI.
                </p>
                <textarea
                  class="ai-prefs-input"
                  rows="5"
                  placeholder="Formatting preferences — e.g. “Use formal headings, spell out numbers, UK English, single-column layout.”"
                  bind:value={renderFormatPrefs}
                ></textarea>
                <div class="ai-policy-row">
                  {#if draftingPolicy}
                    <span class="ai-policy-file" title={draftingPolicy.name}>
                      {draftingPolicy.name}
                    </span>
                    <button
                      class="ai-policy-btn"
                      onclick={clearDraftingPolicy}
                      title="Remove drafting policy">Remove</button
                    >
                  {:else}
                    <button class="ai-policy-btn" onclick={pickDraftingPolicy}>
                      <svg
                        class="ai-policy-icon"
                        viewBox="0 0 16 16"
                        aria-hidden="true"
                      >
                        <path
                          d="M12.5 7.5L7 13a3 3 0 0 1-4.2-4.2L8.8 2.8a2 2 0 0 1 2.8 2.8L5.8 11.4a1 1 0 0 1-1.4-1.4L9.2 5.2"
                          stroke="currentColor"
                          stroke-width="1.5"
                          fill="none"
                          stroke-linecap="round"
                          stroke-linejoin="round"
                        />
                      </svg>
                      Load existing drafting policy…
                    </button>
                  {/if}
                </div>
                <div class="render-tray-actions">
                  <label class="render-tray-format-label">
                    <select
                      class="render-tray-format"
                      bind:value={renderRefineFormat}
                    >
                      <option value="html">HTML document</option>
                      <option value="akn">Akoma Ntoso (XML)</option>
                      <option value="text">Plain text</option>
                    </select>
                  </label>
                  <button
                    class="render-tray-cta"
                    onclick={runRefine}
                    disabled={renderBusy || !activeFileUri}
                  >
                    {renderBusy ? 'Preparing…' : 'Save & refine →'}
                  </button>
                </div>
              {:else}
                <div class="render-tray-title">Refine with Legalese AI</div>
                <p class="render-tray-hint">
                  Sign in with Legalese Cloud to re-draft this document to your
                  house style and drafting policy.
                </p>
                <button
                  class="render-tray-cta render-tray-cta-end"
                  onclick={handleAction}>Sign in with Legalese Cloud</button
                >
              {/if}
            </div>
          {/if}

          <!-- Imports tray — choose which imported modules to include. -->
          {#if importsTrayOpen}
            <div class="render-tray imports-tray">
              <div class="render-tray-title">Imported files</div>
              {#if importedFiles.length > 0}
                <p class="render-tray-hint">
                  Select imported dependencies to include in the render.
                </p>
                <div class="render-tray-list">
                  {#each importedFiles as file (file.uri)}
                    <label class="render-check render-import-row">
                      <span class="render-check-label">{file.label}</span>
                      <span
                        class="toggle"
                        class:on={!excludedModules.has(file.uri)}
                        aria-hidden="true"
                      >
                        <span class="knob"></span>
                      </span>
                      <input
                        class="visually-hidden"
                        type="checkbox"
                        checked={!excludedModules.has(file.uri)}
                        onchange={() => toggleModule(file.uri)}
                      />
                    </label>
                  {/each}
                  <label class="render-check render-check-last">
                    <span class="render-check-label"
                      >Include unused imported definitions</span
                    >
                    <span
                      class="toggle"
                      class:on={renderIncludeUnused}
                      aria-hidden="true"
                    >
                      <span class="knob"></span>
                    </span>
                    <input
                      class="visually-hidden"
                      type="checkbox"
                      bind:checked={renderIncludeUnused}
                    />
                  </label>
                </div>
              {:else if importsLoading}
                <div class="render-imports-empty">Loading imported files…</div>
              {:else}
                <div class="render-imports-empty">
                  This file has no imported dependencies.
                </div>
              {/if}
            </div>
          {/if}

          <!-- Floating icon bar — sits above the iframe at the bottom. -->
          <div class="render-bar">
            <div class="render-bar-group">
              <!-- Save (with format dropdown above) -->
              <div class="render-save-wrap">
                <button
                  class="render-bar-btn"
                  onclick={() => (saveMenuOpen = !saveMenuOpen)}
                  disabled={!activeFileUri || saveBusy}
                  title="Save the rendered document"
                >
                  <svg
                    class="render-icon"
                    viewBox="0 0 16 16"
                    aria-hidden="true"
                  >
                    <path
                      d="M3 2.5h7l3 3V13a.5.5 0 0 1-.5.5h-9A.5.5 0 0 1 3 13V2.5z M5 2.5v3.5h5V2.5 M5.5 13v-3.5h5V13"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="1.2"
                      stroke-linejoin="round"
                    />
                  </svg>
                  <span>Save</span>
                </button>
                {#if saveMenuOpen}
                  <div class="dropdown-menu render-save-menu">
                    {#each SAVE_FORMATS as fmt (fmt.value)}
                      <button
                        class="menu-item"
                        onclick={() => saveAs(fmt.value)}>{fmt.label}</button
                      >
                    {/each}
                  </div>
                {/if}
              </div>

              <!-- Save & refine (opens the refine tray) -->
              <button
                class="render-bar-btn render-tray-toggle"
                onclick={toggleRefineTray}
                title="Save & refine with Legalese AI"
              >
                <svg class="render-icon" viewBox="0 0 16 16" aria-hidden="true">
                  <path
                    d="M3 13l6.5-6.5 M11 3l.6 1.4L13 5l-1.4.6L11 7l-.6-1.4L9 5l1.4-.6L11 3z M4.5 6l.4.9.9.4-.9.4-.4.9-.4-.9L3.2 7.3l.9-.4.4-.9z"
                    fill="none"
                    stroke="currentColor"
                    stroke-width="1.2"
                    stroke-linecap="round"
                    stroke-linejoin="round"
                  />
                </svg>
                <span>Save &amp; refine</span>
                <svg
                  class="render-chevron"
                  class:flipped={refineTrayOpen}
                  viewBox="0 0 10 10"
                  aria-hidden="true"
                >
                  <path
                    d="M2 6.5 L5 3 L8 6.5"
                    fill="none"
                    stroke="currentColor"
                    stroke-width="1.4"
                    stroke-linecap="round"
                    stroke-linejoin="round"
                  />
                </svg>
              </button>
            </div>

            <div class="render-bar-group">
              <!-- Imports (opens tray) -->
              <button
                class="render-bar-btn render-tray-toggle"
                class:on={importsTrayOpen}
                onclick={toggleImportsTray}
                title="Choose imports to include"
              >
                <svg class="render-icon" viewBox="0 0 16 16" aria-hidden="true">
                  <path
                    d="M4 1.5h5l3 3V14a.5.5 0 0 1-.5.5h-7A.5.5 0 0 1 4 14V1.5z M9 1.5v3h3"
                    fill="none"
                    stroke="currentColor"
                    stroke-width="1.2"
                    stroke-linejoin="round"
                  />
                </svg>
                <span>Imports</span>
                <svg
                  class="render-chevron"
                  class:flipped={importsTrayOpen}
                  viewBox="0 0 10 10"
                  aria-hidden="true"
                >
                  <path
                    d="M2 6.5 L5 3 L8 6.5"
                    fill="none"
                    stroke="currentColor"
                    stroke-width="1.4"
                    stroke-linecap="round"
                    stroke-linejoin="round"
                  />
                </svg>
              </button>

              <!-- Section numbers toggle -->
              <button
                class="render-icon-btn"
                class:on={renderNumberSections}
                onclick={() => (renderNumberSections = !renderNumberSections)}
                aria-pressed={renderNumberSections}
                title="Number sections (§ 1, 1.1)"
              >
                <span class="render-glyph">§</span>
              </button>

              <!-- Clause numbers toggle -->
              <button
                class="render-icon-btn"
                class:on={renderNumberClauses}
                onclick={() => (renderNumberClauses = !renderNumberClauses)}
                aria-pressed={renderNumberClauses}
                title="Number clauses (1., 2.)"
              >
                <svg class="render-icon" viewBox="0 0 16 16" aria-hidden="true">
                  <text
                    x="1.5"
                    y="6.5"
                    font-size="5"
                    fill="currentColor"
                    font-family="serif">1.</text
                  >
                  <text
                    x="1.5"
                    y="13"
                    font-size="5"
                    fill="currentColor"
                    font-family="serif">2.</text
                  >
                  <path
                    d="M6.5 4.5h8 M6.5 11h8"
                    fill="none"
                    stroke="currentColor"
                    stroke-width="1.1"
                    stroke-linecap="round"
                  />
                </svg>
              </button>

              <!-- Table of contents toggle -->
              <button
                class="render-icon-btn"
                class:on={renderToc}
                onclick={() => (renderToc = !renderToc)}
                aria-pressed={renderToc}
                title="Table of contents"
              >
                <svg class="render-icon" viewBox="0 0 16 16" aria-hidden="true">
                  <path
                    d="M2.5 4h2 M2.5 8h2 M2.5 12h2 M6.5 4h7 M6.5 8h7 M6.5 12h7"
                    fill="none"
                    stroke="currentColor"
                    stroke-width="1.2"
                    stroke-linecap="round"
                  />
                </svg>
              </button>

              <!-- Light/dark theme for the rendered document -->
              <button
                class="render-icon-btn"
                onclick={() => (renderDark = !renderDark)}
                aria-pressed={renderDark}
                title={renderDark
                  ? 'Rendered document: dark — switch to light'
                  : 'Rendered document: light — switch to dark'}
              >
                {#if renderDark}
                  <svg
                    class="render-icon"
                    viewBox="0 0 16 16"
                    aria-hidden="true"
                  >
                    <path
                      d="M13 9.5A5 5 0 0 1 6.5 3a.4.4 0 0 0-.55-.5A5.5 5.5 0 1 0 13.5 9a.4.4 0 0 0-.5-.5z"
                      fill="currentColor"
                    />
                  </svg>
                {:else}
                  <svg
                    class="render-icon"
                    viewBox="0 0 16 16"
                    aria-hidden="true"
                  >
                    <circle
                      cx="8"
                      cy="8"
                      r="3"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="1.3"
                    />
                    <path
                      d="M8 1.5v1.6 M8 12.9v1.6 M1.5 8h1.6 M12.9 8h1.6 M3.4 3.4l1.1 1.1 M11.5 11.5l1.1 1.1 M12.6 3.4l-1.1 1.1 M4.5 11.5l-1.1 1.1"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="1.3"
                      stroke-linecap="round"
                    />
                  </svg>
                {/if}
              </button>

              <!-- Help -->
              <button
                class="render-icon-btn render-help-btn"
                onclick={() => onLearnMore(RENDER_HELP_URL)}
                title="Learn more about rendering"
              >
                <span class="render-glyph">?</span>
              </button>
            </div>
          </div>
        </div>
      </div>
      {#if activeTab === 'preview'}
        <div class="preview-pane">
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
                        onclick={() =>
                          selectExistingDeployment(dep.deploymentId)}
                      >
                        {dep.deploymentId}
                        <span class="existing-dep-count"
                          >{dep.functions.length} rule{dep.functions.length !==
                          1
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
              onGenerate={generateIntendedUse}
            />
          {:else if deployView === 'breaking-warning'}
            <!-- Breaking change warning -->
            <div class="breaking-warning">
              <button class="back-btn" onclick={() => (deployView = 'mission')}
                >&larr; Cancel deployment</button
              >
              <div class="warning-header">
                &#9888; Breaking changes detected
              </div>
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
                <button
                  class="learn-more"
                  onclick={() =>
                    onLearnMore(
                      'https://legalese.com/l4/tutorials/deploying-rules/exporting-rules-for-deployment.md'
                    )}>Learn more</button
                >
              </div>
            {:else}
              <div class="functions-list">
                <!-- Not keyed by func.name: L4 allows distinct exports to
                     share a name, and a keyed each throws on duplicate keys.
                     The list is replaced wholesale on each LSP response, so
                     there is no cross-update identity to preserve. -->
                {#each functions as func}
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
        </div>
      {/if}
      {#if activeTab === 'deployments'}
        <div class="deployments-tab-wrapper">
          <div
            class="deployments-tab-body"
            class:with-info-note={connectionStatus.connected}
          >
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
                    Removing <strong>{undeployConfirm.deploymentId}</strong> will
                    permanently delete the following rules:
                  </p>
                  <ul class="breaking-list">
                    {#each undeployConfirm.functions as func}
                      <li><span class="breaking-ident">{func.name}</span></li>
                    {/each}
                  </ul>
                </div>
              </div>
            {:else if !connectionStatus.connected}
              {#if !initialized}
                <div class="empty-state">
                  <p class="hint">&nbsp;</p>
                </div>
              {:else if connectionStatus.serviceUrl}
                <div class="empty-state">
                  <p class="hint">
                    Connect to {stripProtocol(connectionStatus.serviceUrl)} to view
                    deployments.
                  </p>
                </div>
              {:else}
                <CloudUpsell context="deployments" onSignIn={handleAction} />
              {/if}
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
              <div class="deployment-filter-bar">
                <button
                  class="deployment-refresh-btn"
                  onclick={() => fetchDeployments()}
                  disabled={deploymentsLoading}
                  title="Refresh deployments"
                  aria-label="Refresh deployments"
                >
                  <svg
                    width="14"
                    height="14"
                    viewBox="0 0 16 16"
                    fill="none"
                    class:spinning={deploymentsLoading}
                    ><path
                      d="M13.5 8a5.5 5.5 0 1 1-1.6-3.9M13.5 2v3h-3"
                      stroke="currentColor"
                      stroke-width="1.3"
                      stroke-linecap="round"
                      stroke-linejoin="round"
                    /></svg
                  >
                </button>
                <div class="deployment-filter-field">
                  <input
                    class="deployment-filter-input"
                    type="text"
                    placeholder="Filter deployments..."
                    bind:value={deploymentFilter}
                  />
                  {#if deploymentFilter}
                    <button
                      class="deployment-filter-clear"
                      onclick={() => (deploymentFilter = '')}
                      title="Clear filter"
                      aria-label="Clear filter">&times;</button
                    >
                  {/if}
                </div>
                <select
                  class="deployment-sort-select"
                  bind:value={deploymentSort}
                  title="Sort deployments"
                  aria-label="Sort deployments"
                >
                  <option value="az">A → Z</option>
                  <option value="za">Z → A</option>
                </select>
              </div>
              {#if filteredDeployments.length === 0}
                <div class="empty-state">
                  <p class="hint">No deployments match "{deploymentFilter}".</p>
                </div>
              {:else}
                <div class="deployments-list">
                  {#each filteredDeployments as dep (dep.deploymentId)}
                    {@const canDownload =
                      !!dep.hasFiles &&
                      (dep.status === 'ready' || dep.status === 'pending')}
                    {@const canUndeploy = true}
                    {@const showMenu = canDownload || canUndeploy}
                    <div
                      class="deployment-group"
                      class:collapsed={collapsedDeployments.has(
                        dep.deploymentId
                      )}
                    >
                      <div class="deployment-header">
                        <button
                          class="deployment-header-toggle"
                          onclick={() =>
                            toggleDeploymentCollapse(dep.deploymentId)}
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
                              {dep.functions.length} rule{dep.functions
                                .length !== 1
                                ? 's'
                                : ''}
                            {/if}
                          </span>
                        </button>
                        <div class="deployment-actions">
                          {#if integrateMode === 'cloud'}
                            <button
                              class="deployment-text-btn"
                              title="Chat with this deployment in Legalese AI"
                              onclick={(e: MouseEvent) => {
                                e.stopPropagation()
                                useInChat(dep)
                              }}
                            >
                              Chat
                            </button>
                          {/if}
                          <button
                            class="deployment-text-btn"
                            title="Integration endpoints for this deployment"
                            aria-haspopup="dialog"
                            aria-expanded={integrateForId === dep.deploymentId}
                            onclick={(e: MouseEvent) => {
                              e.stopPropagation()
                              toggleIntegrate(dep)
                            }}
                          >
                            Integrate
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
                                <div
                                  class="dropdown-menu deployment-dropdown-menu"
                                >
                                  {#if canDownload}
                                    <button
                                      class="menu-item"
                                      disabled={downloadingId ===
                                        dep.deploymentId}
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
                                      disabled={undeployingId ===
                                        dep.deploymentId}
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
                      </div>
                      {#if !collapsedDeployments.has(dep.deploymentId)}
                        {#if dep.error}
                          <div class="deployment-error">
                            <pre>{dep.error}</pre>
                          </div>
                        {:else if compilingDeployments.has(dep.deploymentId)}
                          <div class="deployment-empty">Compiling...</div>
                        {:else if dep.functions.length > 0}
                          <!-- Unkeyed: see note on the preview functions list;
                             func.name is not a safe key. -->
                          {#each dep.functions as func}
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
            {/if}
          </div>
          {#if connectionStatus.connected}
            <aside class="deployment-info-note" role="note">
              <p>
                Deployments are automatically available to Legalese AI and as
                REST API's, online MCP and WebMCP server
                {connectionStatus.isLegaleseCloud
                  ? 'as well as OpenAI- and Anthropic-compatible AI endpoints on the Legalese Cloud'
                  : 'via the connected JL4 service'}.
              </p>
              {#if connectionStatus.isLegaleseCloud}
                <div class="marketplace-block">
                  <p class="marketplace-title">
                    Add your deployments to Claude Code and other agents
                  </p>
                  <div class="marketplace-row">
                    <input
                      class="marketplace-input"
                      type="text"
                      readonly
                      value={MARKETPLACE_URL}
                      onfocus={(e) => e.currentTarget.select()}
                    />
                    <button
                      class="marketplace-copy"
                      onclick={copyMarketplaceUrl}
                      title={marketplaceCopied ? 'Copied' : 'Copy'}
                      aria-label="Copy marketplace URL"
                      >{marketplaceCopied ? '✓' : '⧉'}</button
                    >
                  </div>
                  <div class="marketplace-actions">
                    <HarnessInstallMenu
                      label="Install Skills Marketplace"
                      title="Install the L4 Rules into an AI harness"
                      up
                      large
                      onChoose={(id) => installMarketplace(id as Harness)}
                    />
                    <button
                      class="marketplace-download"
                      onclick={downloadMarketplaceSkill}
                      title="Download the L4 Rules skill as a plugin zip"
                      aria-label="Download plugin zip"
                    >
                      <svg
                        width="14"
                        height="14"
                        viewBox="0 0 16 16"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="1.5"
                        stroke-linecap="round"
                        stroke-linejoin="round"
                        aria-hidden="true"
                      >
                        <path d="M8 2v8m0 0L5 7m3 3 3-3" />
                        <path d="M3 13h10" />
                      </svg>
                    </button>
                    <button
                      class="learn-more"
                      onclick={() => onLearnMore(MARKETPLACE_DOC)}
                      >Learn more</button
                    >
                  </div>
                </div>
              {/if}
            </aside>
          {/if}
        </div>
      {/if}
    </div>
    {#if integrateForId && activeTab === 'deployments'}
      <!-- Sibling of the scroller (not inside it) so the dim + dialog
           pin to the visible tab viewport and stay centred regardless
           of how far the deployment list is scrolled. -->
      <DeploymentIntegratePopover
        deploymentId={integrateForId}
        mode={integrateMode}
        orgSlug={connectionStatus.orgSlug ?? ''}
        host={connectionStatus.serviceUrl}
        onClose={() => (integrateForId = null)}
        {onLearnMore}
        onInstall={(target) => {
          const id = integrateForId
          if (!id) return
          integrateForId = null
          messenger?.sendNotification(
            RequestInstallDeploymentSkill,
            HOST_EXTENSION,
            { deploymentId: id, target }
          )
        }}
      />
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
            <button class="menu-item" onclick={menuAction(installL4Cli)}>
              Install L4 CLI
            </button>
            <div class="menu-separator"></div>
            {#if connectionStatus.connected}
              {#if connectionStatus.serviceUrl}
                <button class="menu-item" onclick={menuAction(openServiceUrl)}>
                  Visit {connectionStatus.serviceUrl}
                </button>
              {/if}
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
    padding: 0 18px;
    gap: 14px;
  }

  .tab {
    padding: 6px 2px;
    background: none;
    border: none;
    border-bottom: 1px solid transparent;
    color: var(--vscode-foreground);
    cursor: pointer;
    font-size: 0.92em;
    opacity: 0.6;
    transition: opacity 0.15s;
    /* Keep multi-word labels (e.g. "Legalese AI") on a single line
       when the sidebar narrows, rather than wrapping to two rows. */
    white-space: nowrap;
  }

  .tab:hover {
    opacity: 0.9;
  }

  .tab.active {
    opacity: 1;
    border-bottom-color: #c8376a;
  }

  /* Non-scrolling visible-area box. Holds the scroller and the
     Integrate overlay as siblings so the overlay can pin to the
     viewport. `min-height: 0` lets the scroller actually scroll
     instead of stretching the frame to the content height. */
  .tab-content-frame {
    position: relative;
    flex: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
  }

  /* The scroller. Padding now lives on each tab's own content (see
     `.tab-pane`, `.deployments-tab-wrapper`, `.preview-pane`) rather
     than here, so the Integrate overlay can cover the full visible
     tab — padding included — with a plain `inset: 0`. */
  .tab-content {
    flex: 1;
    overflow-y: auto;
    min-height: 0;
  }

  .tab-pane {
    height: 100%;
    box-sizing: border-box;
    padding: 16px;
  }

  /* Deploy ("preview") tab owns its 16px padding too. */
  .preview-pane {
    box-sizing: border-box;
    padding: 16px;
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

  .empty-state .learn-more {
    background: none;
    border: none;
    padding: 0;
    margin-top: 8px;
    /* Extension primary action colour (crimson), matching the
       Submit / Deploy CTAs and the active-tab accent. */
    color: #c8376a;
    cursor: pointer;
    font-size: 0.95em;
  }
  .empty-state .learn-more:hover {
    color: #d94d7e;
    text-decoration: underline;
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

  .form-label {
    display: block;
    font-size: 0.85em;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    opacity: 0.7;
    margin-bottom: 6px;
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

  /* The Render tab is a full-bleed preview surface: no padding, the
     stage fills it and the iframe fills the stage. */
  .render-pane {
    padding: 0;
    display: flex;
    flex-direction: column;
  }
  /* Positioning context for the iframe, the floating bar, and the trays. */
  .render-stage {
    position: relative;
    flex: 1;
    min-height: 0;
    display: flex;
    overflow: hidden;
    background: var(--vscode-editor-background, #1e1e1e);
    /* Size container so the floating bar can collapse its labelled buttons
       to icon-only when the sidebar is too narrow to fit the text. */
    container-type: inline-size;
  }
  .render-frame {
    flex: 1;
    width: 100%;
    height: 100%;
    border: none;
    /* Match the iframe's own background to the document theme, so no white
       sliver bleeds through below the page in dark mode. */
    background: #fff;
  }
  .render-frame.dark {
    background: var(--vscode-editor-background, #181818);
  }
  .render-empty {
    flex: 1;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 8px;
    padding: 24px;
    text-align: center;
    font-size: 12px;
    opacity: 0.75;
  }
  .render-empty-error {
    opacity: 0.9;
  }
  .render-error-text {
    max-width: 100%;
    max-height: 40%;
    overflow: auto;
    margin: 0;
    padding: 8px 10px;
    text-align: left;
    white-space: pre-wrap;
    word-break: break-word;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 11px;
    color: var(--vscode-errorForeground, #f14c4c);
    background: var(
      --vscode-textCodeBlock-background,
      rgba(128, 128, 128, 0.1)
    );
    border-radius: 4px;
  }
  /* Thin progress line at the very top while a live re-render is running. */
  .render-progress {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 2px;
    overflow: hidden;
    z-index: 5;
  }
  .render-progress-fill {
    height: 100%;
    width: 40%;
    background: #c8376a;
    animation: render-progress-slide 1s ease-in-out infinite;
  }
  @keyframes render-progress-slide {
    0% {
      transform: translateX(-100%);
    }
    100% {
      transform: translateX(350%);
    }
  }

  /* Floating icon bar — mirrors the chat input's bottom action bar but
     shorter: a single row of icon buttons inset above the iframe. */
  .render-bar {
    position: absolute;
    left: 16px;
    right: 16px;
    bottom: 16px;
    z-index: 8;
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 8px;
    padding: 4px;
    background: var(--vscode-input-background, #252526);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 8px;
  }
  .render-bar-group {
    display: flex;
    align-items: center;
    gap: 4px;
    min-width: 0;
  }
  /* Text+icon button (Save, Imports). */
  .render-bar-btn {
    display: inline-flex;
    align-items: center;
    gap: 5px;
    height: 28px;
    padding: 0 8px;
    background: transparent;
    color: var(--vscode-foreground);
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-size: 0.86em;
    white-space: nowrap;
    transition: background-color 0.1s ease-out;
  }
  .render-bar-btn:hover:not(:disabled),
  .render-bar-btn.on {
    background: rgba(128, 128, 128, 0.18);
  }
  .render-bar-btn:disabled {
    opacity: 0.45;
    cursor: default;
  }
  /* Icon-only buttons (section/clause/TOC toggles, help). */
  .render-icon-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 28px;
    height: 28px;
    padding: 0;
    background: transparent;
    color: var(--vscode-foreground);
    border: none;
    border-radius: 4px;
    cursor: pointer;
    transition: background-color 0.1s ease-out;
  }
  .render-icon-btn:hover {
    background: rgba(128, 128, 128, 0.18);
  }
  /* Pressed/active toggle — light grey fill. */
  .render-icon-btn.on {
    background: rgba(128, 128, 128, 0.32);
  }
  .render-help-btn {
    color: var(--vscode-descriptionForeground);
  }
  .render-icon {
    width: 16px;
    height: 16px;
    flex-shrink: 0;
    display: block;
  }
  .render-glyph {
    font-size: 14px;
    line-height: 1;
    font-weight: 600;
  }
  .render-chevron {
    width: 9px;
    height: 9px;
    flex-shrink: 0;
    color: var(--vscode-descriptionForeground);
    transition: transform 0.12s ease;
  }
  .render-chevron.flipped {
    transform: rotate(180deg);
  }
  /* When the bar is too narrow for the labels, drop just the text from the
     labelled buttons (Save, Save & refine, Imports). The leading icon and
     the dropdown/tray chevron stay; tooltips keep them discoverable. The
     icon-only toggles are unaffected. */
  @container (max-width: 440px) {
    .render-bar-btn span {
      display: none;
    }
    .render-bar-btn {
      padding: 0 6px;
    }
  }
  /* Save split wrapper — anchors the format dropdown above the button. */
  .render-save-wrap {
    position: relative;
    display: flex;
    align-items: center;
  }
  .render-save-menu {
    left: 0;
    bottom: calc(100% + 6px);
    min-width: 180px;
  }
  /* Trays float above the bar (refine prompt, imports checklist). */
  .render-tray {
    position: absolute;
    left: 21px;
    right: 21px;
    bottom: 53px;
    z-index: 9;
    display: flex;
    flex-direction: column;
    gap: 8px;
    max-height: 60%;
    overflow-y: auto;
    padding: 12px;
    background: var(--vscode-input-background, #252526);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 8px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3);
  }
  .render-tray-title {
    font-size: 11px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    opacity: 0.85;
  }
  .render-tray-hint {
    margin: 0;
    font-size: 11px;
    opacity: 0.7;
    line-height: 1.4;
  }
  .render-tray-list {
    display: flex;
    flex-direction: column;
    gap: 4px;
  }
  /* Bottom action row: format select on the left, CTA on the right. */
  .render-tray-actions {
    display: flex;
    align-items: flex-end;
    justify-content: space-between;
    gap: 10px;
    margin-top: 2px;
  }
  .render-tray-format-label {
    display: flex;
    flex-direction: column;
    gap: 3px;
    font-size: 11px;
    opacity: 0.85;
  }
  .render-tray-format {
    background: var(--vscode-input-background);
    color: var(--vscode-input-foreground, var(--vscode-foreground));
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    padding: 4px 6px;
    font-size: 12px;
    min-width: 150px;
  }
  .render-tray-format:focus {
    outline: none;
    border-color: #c8376a;
  }
  /* Matches the deployment Integrate popover's install buttons. */
  .render-tray-cta {
    flex-shrink: 0;
    padding: 8px 14px;
    background: var(--vscode-button-secondaryBackground, transparent);
    color: var(--vscode-button-secondaryForeground, var(--vscode-foreground));
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    cursor: pointer;
    font-size: 0.95em;
  }
  .render-tray-cta:hover:not(:disabled) {
    background: var(
      --vscode-button-secondaryHoverBackground,
      var(--vscode-list-hoverBackground)
    );
  }
  .render-tray-cta:disabled {
    opacity: 0.55;
    cursor: default;
  }
  /* Standalone CTA (e.g. sign-in) right-aligned like the actions-row CTA. */
  .render-tray-cta-end {
    align-self: flex-end;
  }
  .render-check {
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 8px;
    font-size: 12px;
    cursor: pointer;
    padding: 2px 0;
  }
  .render-check-label {
    flex: 1;
    min-width: 0;
  }
  .render-check-last {
    margin-top: 6px;
  }
  /* Toggle switch — matches the Legalese AI settings panel. */
  .toggle {
    flex-shrink: 0;
    position: relative;
    width: 30px;
    height: 18px;
    border-radius: 9999px;
    background: var(--vscode-input-background, rgba(128, 128, 128, 0.35));
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    transition:
      background 120ms ease,
      border-color 120ms ease;
    cursor: pointer;
  }
  .toggle .knob {
    position: absolute;
    top: 1px;
    left: 1px;
    width: 14px;
    height: 14px;
    border-radius: 50%;
    background: var(--vscode-foreground);
    opacity: 0.7;
    transition:
      transform 120ms ease,
      opacity 120ms ease;
  }
  .toggle.on {
    background: #c8376a;
    border-color: #c8376a;
  }
  .toggle.on .knob {
    transform: translateX(12px);
    background: #fff;
    opacity: 1;
  }
  .render-check:focus-within .toggle {
    outline: 1px solid #c8376a;
    outline-offset: 1px;
  }
  .visually-hidden {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border: 0;
  }
  .render-import-row {
    word-break: break-word;
  }
  .render-imports-empty {
    font-size: 12px;
    opacity: 0.7;
  }
  /* Legalese AI rendering controls. */
  .ai-prefs-input {
    width: 100%;
    box-sizing: border-box;
    resize: vertical;
    min-height: 56px;
    margin-top: 2px;
    background: var(--vscode-input-background);
    color: var(--vscode-input-foreground);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    padding: 6px 8px;
    font-family: var(--vscode-font-family, sans-serif);
    font-size: 12px;
    line-height: 1.45;
  }
  .ai-prefs-input::placeholder {
    color: var(--vscode-input-placeholderForeground);
  }
  .ai-prefs-input:focus {
    outline: none;
    border-color: #c8376a;
  }
  .ai-policy-row {
    display: flex;
    align-items: center;
    gap: 8px;
    margin-top: 2px;
  }
  .ai-policy-file {
    flex: 1;
    min-width: 0;
    font-size: 12px;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .ai-policy-btn {
    display: inline-flex;
    align-items: center;
    gap: 6px;
    padding: 6px 10px;
    background: var(--vscode-button-secondaryBackground, transparent);
    color: var(--vscode-button-secondaryForeground, var(--vscode-foreground));
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    cursor: pointer;
    font-size: 0.9em;
  }
  .ai-policy-icon {
    width: 14px;
    height: 14px;
    flex-shrink: 0;
  }
  .ai-policy-btn:hover {
    background: var(
      --vscode-button-secondaryHoverBackground,
      var(--vscode-list-hoverBackground)
    );
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
    box-sizing: border-box;
    /* This tab owns its 16px padding (no longer inherited from
       `.tab-content`). */
    padding: 16px;
  }

  .deployments-tab-body {
    flex: 1 0 auto;
    display: flex;
    flex-direction: column;
  }

  /* Only reserve the gap above the info-note when it's actually shown
     (connected). When disconnected the box should centre in the full
     tab height, matching the AI tab. */
  .deployments-tab-body.with-info-note {
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

  .marketplace-block {
    margin-top: 12px;
    padding-top: 12px;
    border-top: 1px solid var(--vscode-panel-border, rgba(128, 128, 128, 0.2));
  }
  .marketplace-title {
    margin: 0 0 8px;
  }
  .marketplace-row {
    display: flex;
    align-items: stretch;
    gap: 6px;
  }
  .marketplace-input {
    flex: 1;
    min-width: 0;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 1em;
    padding: 6px 8px;
    background: var(--vscode-input-background);
    color: var(--vscode-input-foreground);
    border: 1px solid var(--vscode-input-border, transparent);
    border-radius: 4px;
  }
  .marketplace-copy {
    flex-shrink: 0;
    background: var(--vscode-button-secondaryBackground, transparent);
    color: var(--vscode-button-secondaryForeground, var(--vscode-foreground));
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    cursor: pointer;
    padding: 0 10px;
  }
  .marketplace-copy:hover {
    background: var(
      --vscode-button-secondaryHoverBackground,
      var(--vscode-list-hoverBackground)
    );
  }
  .marketplace-actions {
    display: flex;
    align-items: center;
    gap: 10px;
    margin-top: 10px;
  }
  /* Icon-only secondary button next to the Install dropdown. */
  .marketplace-download {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    padding: 7px 9px;
    background: var(--vscode-button-secondaryBackground, transparent);
    color: var(--vscode-button-secondaryForeground, var(--vscode-foreground));
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    cursor: pointer;
    line-height: 0;
  }
  .marketplace-download:hover {
    background: var(
      --vscode-button-secondaryHoverBackground,
      var(--vscode-list-hoverBackground)
    );
  }
  .marketplace-actions .learn-more {
    background: none;
    border: none;
    padding: 0;
    color: #c8376a;
    cursor: pointer;
    font-size: inherit;
  }
  .marketplace-actions .learn-more:hover {
    color: #d94d7e;
    text-decoration: underline;
  }

  .deployment-filter-bar {
    display: flex;
    align-items: center;
    gap: 6px;
    margin-bottom: 12px;
  }

  .deployment-refresh-btn {
    flex: none;
    display: flex;
    align-items: center;
    justify-content: center;
    box-sizing: border-box;
    padding: 4px 6px;
    background: var(--vscode-input-background, #3c3c3c);
    color: var(--vscode-descriptionForeground, #999);
    border: 1px solid var(--vscode-input-border, #555);
    border-radius: 3px;
    cursor: pointer;
  }

  .deployment-refresh-btn:hover:not(:disabled) {
    color: var(--vscode-foreground, #ccc);
    background: var(--vscode-list-hoverBackground, #2a2d2e);
  }

  .deployment-refresh-btn:disabled {
    cursor: default;
    opacity: 0.7;
  }

  .deployment-refresh-btn svg.spinning {
    animation: deployment-refresh-spin 0.8s linear infinite;
  }

  @keyframes deployment-refresh-spin {
    to {
      transform: rotate(360deg);
    }
  }

  .deployment-filter-field {
    position: relative;
    flex: 1;
    min-width: 0;
  }

  .deployment-filter-input {
    width: 100%;
    box-sizing: border-box;
    padding: 5px 24px 5px 8px;
    font-size: 0.92em;
    background: var(--vscode-input-background, #3c3c3c);
    color: var(--vscode-input-foreground, #ccc);
    border: 1px solid var(--vscode-input-border, #555);
    border-radius: 3px;
    outline: none;
  }

  .deployment-filter-input:focus {
    border-color: var(--vscode-foreground, #ccc);
  }

  .deployment-filter-clear {
    position: absolute;
    right: 4px;
    top: 50%;
    transform: translateY(-50%);
    display: flex;
    align-items: center;
    justify-content: center;
    width: 18px;
    height: 18px;
    padding: 0;
    border: none;
    border-radius: 3px;
    background: none;
    color: var(--vscode-descriptionForeground, #999);
    font-size: 14px;
    line-height: 1;
    cursor: pointer;
  }

  .deployment-filter-clear:hover {
    color: var(--vscode-foreground, #ccc);
    background: var(--vscode-toolbar-hoverBackground, rgba(255, 255, 255, 0.1));
  }

  .deployment-sort-select {
    flex: none;
    box-sizing: border-box;
    padding: 4px 6px;
    font-size: 0.92em;
    line-height: normal;
    background: var(--vscode-dropdown-background, #3c3c3c);
    color: var(--vscode-dropdown-foreground, #ccc);
    border: 1px solid var(--vscode-dropdown-border, #555);
    border-radius: 3px;
    outline: none;
    cursor: pointer;
  }

  .deployment-sort-select:focus {
    border-color: var(--vscode-foreground, #ccc);
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

  .deployment-actions {
    position: relative;
    display: flex;
    align-items: center;
    gap: 2px;
    flex-shrink: 0;
  }

  /* Text-style action, matching the inspector's "Remove all" /
     dismiss controls: transparent, foreground-coloured, dimmed until
     hover. Keeps the deployment header visually quiet. */
  .deployment-text-btn {
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    opacity: 0.6;
    font-size: 0.85em;
    flex-shrink: 0;
    padding: 4px 6px;
    white-space: nowrap;
  }
  .deployment-text-btn:hover {
    opacity: 1;
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
