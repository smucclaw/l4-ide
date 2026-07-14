<script lang="ts">
  import { onMount } from 'svelte'
  import type {
    AiMcpServerInfo,
    AiPermissionCategory,
    AiPermissionValue,
  } from 'jl4-client-rpc'
  import type { AiChatStore } from '$lib/stores/ai-chat.svelte'
  import { aiPrefs } from '$lib/stores/ai-prefs.svelte'

  let {
    store,
    onClose,
  }: {
    store: AiChatStore
    onClose: () => void
  } = $props()

  type Values = Record<AiPermissionCategory, AiPermissionValue>

  let values = $state<Values | null>(null)

  // ── MCP servers ───────────────────────────────────────────────────
  let mcpServers = $state<AiMcpServerInfo[] | null>(null)
  /** Master switch mirrored from `legaleseAi.mcp.enabled`. */
  let mcpAllEnabled = $state(true)
  let showAddForm = $state(false)
  let addName = $state('')
  let addTransport = $state<'http' | 'sse' | 'stdio'>('http')
  let addUrl = $state('')
  let addCommand = $state('')
  let addBearerToken = $state('')
  let addError = $state<string | null>(null)
  let addBusy = $state(false)
  /** Set when the submitted name collides with an existing server —
   *  the primary button becomes "Replace server" and the resubmit
   *  carries overwrite: true. Cleared when the name changes. */
  let addOverwritePending = $state(false)
  let addedNote = $state<string | null>(null)
  /** Server ids whose tool list is expanded. */
  let expandedIds = $state<Record<string, boolean>>({})
  /** Server id whose three-dot menu is open, if any. */
  let menuOpenId = $state<string | null>(null)
  let menuError = $state<string | null>(null)

  async function refreshMcpServers(): Promise<void> {
    const res = await store.getMcpServers()
    mcpServers = res.servers
    mcpAllEnabled = res.allEnabled
  }

  function toggleMcpAll(): void {
    const next = !mcpAllEnabled
    mcpAllEnabled = next
    store.setMcpAllEnabled(next)
    // Re-enabling reconnects the enabled servers on the extension
    // side; poll once for the connection outcomes.
    setTimeout(() => void refreshMcpServers(), 1500)
  }

  function toggleMcpServer(server: AiMcpServerInfo): void {
    const next = !server.enabled
    store.setMcpServerEnabled(server.id, next)
    // Optimistic update — enabling kicks off a connection on the
    // extension side, so reflect "connecting" right away and poll once
    // shortly after for the real outcome.
    if (mcpServers) {
      mcpServers = mcpServers.map((s) =>
        s.id === server.id
          ? {
              ...s,
              enabled: next,
              status:
                s.status === 'external'
                  ? s.status
                  : next
                    ? 'connecting'
                    : 'stopped',
            }
          : s
      )
    }
    setTimeout(() => void refreshMcpServers(), 1500)
  }

  async function runServerAction(
    id: string,
    action: 'start' | 'stop' | 'refresh' | 'remove'
  ): Promise<void> {
    menuOpenId = null
    menuError = null
    if (action === 'start' || action === 'refresh') {
      // The request resolves only once the connection attempt settled;
      // show the intermediate state meanwhile.
      if (mcpServers) {
        mcpServers = mcpServers.map((s) =>
          s.id === id && s.status !== 'external'
            ? { ...s, status: 'connecting' }
            : s
        )
      }
    }
    const res = await store.mcpServerAction(id, action)
    if (!res.ok) menuError = res.error ?? `Could not ${action} the server.`
    await refreshMcpServers()
  }

  function toggleMcpTool(
    server: AiMcpServerInfo,
    tool: { name: string; enabled: boolean }
  ): void {
    const next = !tool.enabled
    store.setMcpToolEnabled(server.id, tool.name, next)
    if (mcpServers) {
      mcpServers = mcpServers.map((s) =>
        s.id === server.id
          ? {
              ...s,
              tools: s.tools.map((t) =>
                t.name === tool.name ? { ...t, enabled: next } : t
              ),
            }
          : s
      )
    }
  }

  async function submitAddServer(): Promise<void> {
    if (addBusy) return
    addError = null
    addBusy = true
    try {
      const res = await store.addMcpServer({
        name: addName,
        transport: addTransport,
        ...(addTransport === 'stdio'
          ? { command: addCommand }
          : {
              url: addUrl,
              ...(addBearerToken.trim()
                ? { bearerToken: addBearerToken.trim() }
                : {}),
            }),
        ...(addOverwritePending ? { overwrite: true } : {}),
      })
      if (!res.ok) {
        if (res.exists) {
          addOverwritePending = true
          addError = `${res.error ?? 'A server with this name already exists.'} Click "Replace server" to override it.`
        } else {
          addError = res.error ?? 'Could not add the server.'
        }
        return
      }
      addName = ''
      addUrl = ''
      addCommand = ''
      addBearerToken = ''
      addOverwritePending = false
      addedNote =
        addTransport === 'stdio'
          ? "Command server added to VS Code's mcp.json — VS Code manages " +
            'its lifecycle, and its tools appear here once VS Code runs it.'
          : null
      showAddForm = false
      await refreshMcpServers()
    } finally {
      addBusy = false
    }
  }

  /** Badge text summarizing the extension's own connection state. */
  function serverStatus(server: AiMcpServerInfo): string {
    switch (server.status) {
      case 'connected':
        return server.tools.length === 1
          ? '1 tool'
          : `${server.tools.length} tools`
      case 'connecting':
        return 'connecting…'
      case 'error':
        return 'connection failed'
      case 'external':
        return server.tools.length > 0
          ? `${server.tools.length} tools (VS Code)`
          : 'managed by VS Code'
      case 'stopped':
      default:
        return server.enabled ? 'not connected' : 'off'
    }
  }

  const CATEGORIES: {
    id: AiPermissionCategory
    label: string
    hint: string
  }[] = [
    {
      id: 'fs.read',
      label: 'Read files and folders',
      hint: 'Accessing files in your workspace for context',
    },
    {
      id: 'fs.create',
      label: 'Create files',
      hint: 'Write new files in your workspace',
    },
    {
      id: 'fs.edit',
      label: 'Edit files',
      hint: 'Edit files in your workspace',
    },
    {
      id: 'fs.delete',
      label: 'Delete files',
      hint: 'Send a file in your workspace to trash',
    },
    {
      id: 'l4.evaluate',
      label: 'Run L4 diagnostics',
      hint: 'Check L4 files for validity and run directives via LSP (#EVAL, #ASSERT, #TRACE, ...)',
    },
    {
      id: 'l4.refactor',
      label: 'Refactor L4 code',
      hint: 'Apply structured L4 refactors (rename across imports; more actions to come)',
    },
    {
      id: 'mcp.l4Rules',
      label: 'Access Legalese Cloud Deployments',
      hint: 'Browse and evaluate deployed L4 rules on Legalese Cloud',
    },
    {
      id: 'meta.askUser',
      label: 'Ask you clarifying questions',
      hint: 'Briefly pause an AI chat to make a choice',
    },
  ]

  onMount(async () => {
    void refreshMcpServers()
    values = await store.getPermissions()
  })

  function update(category: AiPermissionCategory, e: Event): void {
    if (!values) return
    const next = (e.target as HTMLSelectElement).value as AiPermissionValue
    values[category] = next
    store.setPermission(category, next)
  }
</script>

<svelte:window onclick={() => (menuOpenId = null)} />

<div
  class="scrim"
  role="dialog"
  aria-modal="true"
  aria-label="AI chat settings"
>
  <div class="panel">
    <div class="panel-header">
      <span>Settings</span>
      <button class="close" onclick={onClose} aria-label="Close">✕</button>
    </div>
    <div class="panel-body">
      <section>
        <div class="section-title">Preferences</div>
        <label class="pref-row">
          <div class="pref-name">
            <div>Show model reasoning in chat</div>
            <div class="pref-hint">
              Display the assistant's chain-of-thought "Thinking" blocks
              alongside its replies.
            </div>
          </div>
          <span
            class="toggle"
            class:on={aiPrefs.showReasoning}
            aria-hidden="true"
          >
            <span class="knob"></span>
          </span>
          <input
            class="visually-hidden"
            type="checkbox"
            checked={aiPrefs.showReasoning}
            onchange={(e) =>
              aiPrefs.setShowReasoning((e.target as HTMLInputElement).checked)}
          />
        </label>

        <div class="pref-stack">
          <div class="pref-name">
            <div>Methodology</div>
            <div class="pref-hint">
              Used as guidance when encoding natural language rules in L4.
            </div>
          </div>
          <textarea
            class="methodology-input"
            rows="4"
            placeholder="Describe how Legalese AI should think about the encoding of natural language rules"
            value={aiPrefs.methodology}
            oninput={(e) =>
              (aiPrefs.methodology = (e.target as HTMLTextAreaElement).value)}
            onchange={(e) =>
              aiPrefs.setMethodology((e.target as HTMLTextAreaElement).value)}
          ></textarea>
        </div>
      </section>

      <section>
        <div class="section-title">MCP servers</div>
        <div class="section-help">
          Toggled-on servers connect automatically once you're signed in to
          Legalese Cloud. Switching a server off only hides its tools from
          Legalese AI — it stays available to other VS Code features.
        </div>
        {#if mcpServers}
          <div class="perm-grid">
            {#if mcpServers.length > 1}
              <div class="mcp-row mcp-master-row">
                <div class="perm-name mcp-grow">
                  <div>All MCP servers</div>
                  <div class="perm-hint">
                    Turn off to hide every server's tools from Legalese AI at
                    once.
                  </div>
                </div>
                <button
                  class="toggle"
                  class:on={mcpAllEnabled}
                  role="switch"
                  aria-checked={mcpAllEnabled}
                  aria-label="Use MCP servers"
                  onclick={toggleMcpAll}
                >
                  <span class="knob"></span>
                </button>
              </div>
            {/if}
            {#each mcpServers as server (server.id)}
              <div class="mcp-row-wrap" class:all-off={!mcpAllEnabled}>
                <div class="mcp-row">
                  <button
                    class="mcp-expander"
                    class:expanded={!!expandedIds[server.id]}
                    aria-expanded={!!expandedIds[server.id]}
                    aria-label={`Show ${server.name} tools`}
                    onclick={() =>
                      (expandedIds[server.id] = !expandedIds[server.id])}
                    >&#9002;</button
                  >
                  <div class="perm-name mcp-grow">
                    <div class="mcp-name-line">
                      <span>{server.name}</span>
                      <span class="mcp-badge">{serverStatus(server)}</span>
                    </div>
                    <div class="perm-hint">
                      {#if server.status === 'error' && server.error}
                        {server.error}
                      {:else if server.detail}
                        {server.detail}
                      {:else}
                        Registered in VS Code
                      {/if}
                    </div>
                  </div>
                  <div class="mcp-menu-anchor">
                    <button
                      class="mcp-dots"
                      aria-label={`${server.name} actions`}
                      aria-haspopup="menu"
                      onclick={(e) => {
                        e.stopPropagation()
                        menuOpenId = menuOpenId === server.id ? null : server.id
                      }}
                    >
                      ⋯
                    </button>
                    {#if menuOpenId === server.id}
                      <div class="mcp-menu" role="menu">
                        {#if server.status !== 'external'}
                          {#if server.status === 'connected'}
                            <button
                              role="menuitem"
                              onclick={() =>
                                void runServerAction(server.id, 'refresh')}
                            >
                              Refresh tools
                            </button>
                            <button
                              role="menuitem"
                              onclick={() =>
                                void runServerAction(server.id, 'stop')}
                            >
                              Stop
                            </button>
                          {:else}
                            <button
                              role="menuitem"
                              onclick={() =>
                                void runServerAction(server.id, 'start')}
                            >
                              Start
                            </button>
                          {/if}
                        {/if}
                        {#if server.source === 'user'}
                          <button
                            role="menuitem"
                            class="danger"
                            onclick={() =>
                              void runServerAction(server.id, 'remove')}
                          >
                            Remove
                          </button>
                        {/if}
                      </div>
                    {/if}
                  </div>
                  <button
                    class="toggle"
                    class:on={server.enabled}
                    role="switch"
                    aria-checked={server.enabled}
                    aria-label={`Use ${server.name} tools`}
                    onclick={() => toggleMcpServer(server)}
                  >
                    <span class="knob"></span>
                  </button>
                </div>
                {#if expandedIds[server.id]}
                  <div class="mcp-tools">
                    {#each server.tools as tool (tool.name)}
                      <div class="mcp-tool-row" title={tool.description ?? ''}>
                        <div class="mcp-tool-name">{tool.name}</div>
                        <button
                          class="toggle"
                          class:on={tool.enabled}
                          role="switch"
                          aria-checked={tool.enabled}
                          aria-label={`Use ${tool.name}`}
                          onclick={() => toggleMcpTool(server, tool)}
                        >
                          <span class="knob"></span>
                        </button>
                      </div>
                    {/each}
                    {#if server.tools.length === 0}
                      <div class="perm-hint">
                        No tools yet — the server isn't connected.
                      </div>
                    {/if}
                  </div>
                {/if}
              </div>
            {/each}
            {#if mcpServers.length === 0}
              <div class="section-help">
                No third-party MCP servers registered in VS Code yet.
              </div>
            {/if}
          </div>
          {#if menuError}
            <div class="mcp-error">{menuError}</div>
          {/if}
          {#if addedNote}
            <div class="mcp-added-note">{addedNote}</div>
          {/if}
          {#if showAddForm}
            <div class="mcp-add-form">
              <input
                class="mcp-input"
                type="text"
                placeholder="Name (e.g. github)"
                bind:value={addName}
                oninput={() => {
                  // A different name is a fresh add — drop the pending
                  // replace confirmation so it can't silently override
                  // some other existing server.
                  addOverwritePending = false
                }}
              />
              <select class="perm-select" bind:value={addTransport}>
                <option value="http">HTTP</option>
                <option value="sse">SSE</option>
                <option value="stdio">Command</option>
              </select>
              {#if addTransport === 'stdio'}
                <input
                  class="mcp-input"
                  type="text"
                  placeholder="Command (e.g. npx -y @some/mcp-server)"
                  bind:value={addCommand}
                />
              {:else}
                <input
                  class="mcp-input"
                  type="text"
                  placeholder="URL (e.g. https://example.com/mcp)"
                  bind:value={addUrl}
                />
                <input
                  class="mcp-input"
                  type="password"
                  placeholder="Bearer token (optional)"
                  autocomplete="off"
                  bind:value={addBearerToken}
                />
              {/if}
              {#if addError}
                <div class="mcp-error">{addError}</div>
              {/if}
              <div class="mcp-form-actions">
                <button
                  class="mcp-btn primary"
                  disabled={addBusy}
                  onclick={() => void submitAddServer()}
                >
                  {addBusy
                    ? 'Adding…'
                    : addOverwritePending
                      ? 'Replace server'
                      : 'Add server'}
                </button>
                <button
                  class="mcp-btn"
                  onclick={() => {
                    showAddForm = false
                    addError = null
                    addOverwritePending = false
                  }}
                >
                  Cancel
                </button>
              </div>
            </div>
          {:else}
            <div class="mcp-actions-row">
              <button
                class="mcp-btn"
                onclick={() => {
                  showAddForm = true
                  addedNote = null
                }}
              >
                + Add MCP server
              </button>
              <button class="mcp-btn" onclick={() => void refreshMcpServers()}>
                Refresh
              </button>
            </div>
          {/if}
        {:else}
          <div class="loading">Loading…</div>
        {/if}
      </section>

      <section>
        <div class="section-title">Permissions</div>
        <div class="section-help">
          <code>Never</code> blocks the tool, <code>Ask</code> prompts you each
          time, <code>Always</code> runs without asking.
        </div>
        {#if values}
          <div class="perm-grid">
            {#each CATEGORIES as cat (cat.id)}
              <label class="perm-row">
                <div class="perm-name">
                  <div>{cat.label}</div>
                  <div class="perm-hint">{cat.hint}</div>
                </div>
                <select
                  class="perm-select"
                  value={values[cat.id]}
                  onchange={(e) => update(cat.id, e)}
                >
                  <option value="never">Never</option>
                  <option value="ask">Ask</option>
                  <option value="always">Always</option>
                </select>
              </label>
            {/each}
          </div>
        {:else}
          <div class="loading">Loading…</div>
        {/if}
      </section>
    </div>
  </div>
</div>

<style>
  .scrim {
    position: absolute;
    inset: 0;
    display: flex;
    justify-content: center;
    align-items: stretch;
    background: rgba(0, 0, 0, 0.35);
    z-index: 40;
  }
  .panel {
    flex: 1;
    background: var(
      --vscode-sideBar-background,
      var(--vscode-editor-background)
    );
    display: flex;
    flex-direction: column;
    max-width: 100%;
  }
  /* Match the conversation-history panel header: small uppercase
     label on the left, plain ✕ close button on the right. Keeps both
     overlays visually consistent inside the sidebar. */
  .panel-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 8px 12px;
    border-bottom: 1px solid
      var(--vscode-widget-border, rgba(128, 128, 128, 0.3));
    font-size: 12px;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: var(--vscode-descriptionForeground);
  }
  .close {
    background: transparent;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    font-size: 12px;
  }
  .panel-body {
    flex: 1;
    overflow-y: auto;
    padding: 12px;
    display: flex;
    flex-direction: column;
    gap: 18px;
  }
  section {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }
  /* Hairline between sections so Preferences / MCP servers /
     Permissions read as distinct groups when scrolling. */
  section + section {
    border-top: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.25));
    padding-top: 16px;
  }
  /* Match the history panel's group labels (`Today`, `Yesterday`,
     etc.) so section headings in Settings read as the same kind of
     quiet, uppercase subheader instead of a primary foreground title. */
  .section-title {
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    text-transform: uppercase;
    letter-spacing: 0.06em;
  }
  .section-help {
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    line-height: 1.45;
  }
  .section-help code {
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.92em;
  }
  .perm-grid {
    display: flex;
    flex-direction: column;
    gap: 4px;
    margin-top: 4px;
  }
  .perm-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 10px;
    padding: 6px 8px;
    border-radius: 4px;
    cursor: pointer;
  }
  .perm-row:hover {
    background: rgba(128, 128, 128, 0.08);
  }
  .perm-name {
    display: flex;
    flex-direction: column;
    font-size: 12px;
    color: var(--vscode-foreground);
  }
  .perm-hint {
    font-size: 10px;
    color: var(--vscode-descriptionForeground);
    margin-top: 6px;
  }
  .perm-select {
    background: var(--vscode-input-background);
    color: var(--vscode-input-foreground);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    padding: 3px 6px;
    font-size: 12px;
    min-width: 84px;
  }
  .pref-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 10px;
    padding: 6px 8px;
    border-radius: 4px;
    cursor: pointer;
  }
  .pref-row:hover {
    background: rgba(128, 128, 128, 0.08);
  }
  .pref-name {
    display: flex;
    flex-direction: column;
    font-size: 12px;
    color: var(--vscode-foreground);
  }
  .pref-hint {
    font-size: 10px;
    color: var(--vscode-descriptionForeground);
    margin-top: 6px;
  }
  .pref-stack {
    display: flex;
    flex-direction: column;
    gap: 8px;
    padding: 6px 8px;
  }
  .methodology-input {
    width: 100%;
    box-sizing: border-box;
    resize: vertical;
    min-height: 64px;
    background: var(--vscode-input-background);
    color: var(--vscode-input-foreground);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    padding: 6px 8px;
    font-family: var(--vscode-font-family, sans-serif);
    font-size: 12px;
    line-height: 1.45;
  }
  .methodology-input::placeholder {
    color: var(--vscode-input-placeholderForeground);
  }
  /* Match the chat input's focus treatment: the 1px border brightens,
     no extra outline (border + outline reads as a 2px double line). */
  .methodology-input:focus {
    outline: none;
    border-color: var(--vscode-foreground, #ccc);
  }
  .mcp-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 10px;
    padding: 6px 8px;
    border-radius: 4px;
  }
  .mcp-row:hover {
    background: rgba(128, 128, 128, 0.08);
  }
  .mcp-name-line {
    display: flex;
    align-items: baseline;
    gap: 6px;
  }
  .mcp-badge {
    font-size: 10px;
    color: var(--vscode-descriptionForeground);
  }
  .mcp-add-form {
    display: flex;
    flex-direction: column;
    gap: 6px;
    padding: 6px 8px;
  }
  .mcp-input {
    width: 100%;
    box-sizing: border-box;
    background: var(--vscode-input-background);
    color: var(--vscode-input-foreground);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    padding: 4px 8px;
    font-family: var(--vscode-font-family, sans-serif);
    font-size: 12px;
  }
  .mcp-input::placeholder {
    color: var(--vscode-input-placeholderForeground);
  }
  .mcp-input:focus {
    outline: none;
    border-color: var(--vscode-foreground, #ccc);
  }
  .mcp-form-actions {
    display: flex;
    gap: 6px;
  }
  .mcp-btn {
    background: var(--vscode-input-background);
    color: var(--vscode-foreground);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    padding: 3px 10px;
    font-size: 12px;
    cursor: pointer;
  }
  .mcp-btn:hover {
    background: rgba(128, 128, 128, 0.12);
  }
  .mcp-btn.primary {
    background: #c8376a;
    border-color: #c8376a;
    color: #fff;
  }
  .mcp-btn.primary:disabled {
    opacity: 0.6;
    cursor: default;
  }
  .mcp-actions-row {
    display: flex;
    gap: 6px;
    margin: 2px 8px 0;
  }
  .mcp-row-wrap {
    display: flex;
    flex-direction: column;
  }
  /* Master switch off: the server rows read as removed from the AI —
     dimmed and inert; only the master toggle stays interactive. */
  .mcp-row-wrap.all-off {
    opacity: 0.45;
    pointer-events: none;
  }
  .mcp-master-row {
    padding-left: 8px;
  }
  .mcp-grow {
    flex: 1;
    min-width: 0;
  }
  /* Same collapse handle as the chat tool-call rows / deployment
     tool-card: the &#9002; glyph rotating 90° when expanded — in the
     regular foreground here rather than the chat crimson, since
     settings rows carry no status accent. */
  .mcp-expander {
    flex-shrink: 0;
    background: transparent;
    border: none;
    padding: 0 2px;
    font-size: 11px;
    line-height: 1;
    color: var(--vscode-foreground);
    cursor: pointer;
    transition: transform 0.15s;
    transform-origin: 25% 50%;
  }
  .mcp-expander.expanded {
    transform: rotate(90deg);
  }
  .mcp-menu-anchor {
    position: relative;
    flex-shrink: 0;
    display: flex;
    align-items: center;
  }
  .mcp-dots {
    background: transparent;
    border: none;
    color: var(--vscode-descriptionForeground);
    font-size: 14px;
    line-height: 1;
    padding: 2px 4px;
    border-radius: 3px;
    cursor: pointer;
  }
  .mcp-dots:hover {
    background: rgba(128, 128, 128, 0.15);
    color: var(--vscode-foreground);
  }
  .mcp-menu {
    position: absolute;
    top: 100%;
    right: 0;
    z-index: 10;
    min-width: 120px;
    display: flex;
    flex-direction: column;
    background: var(--vscode-menu-background, var(--vscode-editor-background));
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    box-shadow: 0 4px 10px rgba(0, 0, 0, 0.25);
    padding: 3px;
  }
  .mcp-menu button {
    background: transparent;
    border: none;
    text-align: left;
    padding: 4px 8px;
    font-size: 12px;
    color: var(--vscode-menu-foreground, var(--vscode-foreground));
    border-radius: 3px;
    cursor: pointer;
    white-space: nowrap;
  }
  .mcp-menu button:hover {
    background: var(
      --vscode-menu-selectionBackground,
      rgba(128, 128, 128, 0.15)
    );
  }
  .mcp-menu button.danger {
    color: var(--vscode-errorForeground, #f14c4c);
  }
  .mcp-tools {
    display: flex;
    flex-direction: column;
    gap: 2px;
    margin: 0 8px 4px 26px;
    padding: 4px 0 4px 8px;
    border-left: 1px solid
      var(--vscode-widget-border, rgba(128, 128, 128, 0.25));
  }
  .mcp-tool-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 8px;
    padding: 2px 4px;
    border-radius: 3px;
  }
  .mcp-tool-row:hover {
    background: rgba(128, 128, 128, 0.08);
  }
  .mcp-tool-name {
    font-size: 12px;
    color: var(--vscode-foreground);
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }
  .mcp-error {
    font-size: 11px;
    color: var(--vscode-errorForeground, #f14c4c);
    line-height: 1.4;
  }
  .mcp-added-note {
    font-size: 11px;
    color: var(--vscode-descriptionForeground);
    line-height: 1.45;
    padding: 0 8px;
  }
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
    /* The toggle renders both as a decorative <span> (pref rows, with a
       hidden checkbox) and as a real <button> (MCP rows) — neutralize
       the button chrome so both look identical. */
    appearance: none;
    padding: 0;
    margin: 0;
  }
  button.toggle:focus-visible {
    outline: 1px solid #c8376a;
    outline-offset: 1px;
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
  .pref-row:focus-within .toggle {
    outline: 1px solid #c8376a;
    outline-offset: 1px;
  }
  .loading {
    font-size: 12px;
    color: var(--vscode-descriptionForeground);
  }
</style>
