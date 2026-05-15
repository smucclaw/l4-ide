<script lang="ts">
  import { onMount } from 'svelte'
  import type { AiPermissionCategory, AiPermissionValue } from 'jl4-client-rpc'
  import type { AiChatStore } from '$lib/stores/ai-chat.svelte'

  let {
    store,
    onClose,
  }: {
    store: AiChatStore
    onClose: () => void
  } = $props()

  type Values = Record<AiPermissionCategory, AiPermissionValue>

  let values = $state<Values | null>(null)

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
    values = await store.getPermissions()
  })

  function update(category: AiPermissionCategory, e: Event): void {
    if (!values) return
    const next = (e.target as HTMLSelectElement).value as AiPermissionValue
    values[category] = next
    store.setPermission(category, next)
  }
</script>

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

      <!-- <section>
        <div class="section-title">MCP servers</div>
        <div class="section-help">
          The built-in <code>l4-rules</code> server is wired automatically and
          exposes whichever rules you have deployed to Legalese Cloud. Adding
          third-party servers isn't wired yet.
        </div>
      </section> -->
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
  .loading {
    font-size: 12px;
    color: var(--vscode-descriptionForeground);
  }
</style>
