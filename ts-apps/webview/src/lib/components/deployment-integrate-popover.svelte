<script lang="ts">
  // "Integrate" pop-over for a deployment. Renders the connection
  // strings a third party needs to call this deployment's rules. The
  // section set depends on connection mode:
  //   - cloud:       OpenAI v1, MCP, WebMCP embed, OpenAPI JSON
  //   - self-hosted: MCP, WebMCP embed, OpenAPI JSON (no OpenAI v1 —
  //                  a self-hosted jl4-service has no AI endpoint)
  const DOCS_BASE = 'https://legalese.com/l4/tutorials'
  // Assembled from fragments so the embed-snippet's opening and
  // closing tags never appear as literal HTML script tokens anywhere
  // in this component's own code/comments. A raw closing token would
  // terminate this block early (HTML parses script content as raw
  // text, ignoring JS comment/string context), breaking both the
  // Svelte compiler and the eslint-svelte parser.
  const SCRIPT_OPEN = '<' + 'script'
  const SCRIPT_CLOSE = '</' + 'script>'

  let {
    deploymentId,
    mode,
    orgSlug = '',
    host = '',
    onClose,
    onLearnMore,
  }: {
    deploymentId: string
    mode: 'cloud' | 'self-hosted'
    orgSlug?: string
    host?: string
    onClose: () => void
    onLearnMore: (docUrl: string) => void
  } = $props()

  interface Section {
    label: string
    value: string
    hint: string
    doc: string
  }

  const trimmedHost = $derived(host.replace(/\/+$/, ''))

  const sections: Section[] = $derived(
    mode === 'cloud'
      ? [
          {
            label: 'OpenAI v1 compatible AI chat API',
            value: `https://ai.legalese.cloud/${orgSlug}/${deploymentId}/v1`,
            hint: 'Uses legalese-comply-4 for fast, reliable rule evaluation',
            doc: 'legalese-cloud/openai-compatible-api.md',
          },
          {
            label: 'MCP server',
            value: `https://mcp.legalese.cloud/${orgSlug}/${deploymentId}`,
            hint: 'Use with any MCP enabled AI chat',
            doc: 'legalese-cloud/mcp-server.md',
          },
          {
            label: 'WebMCP embed script',
            value: `${SCRIPT_OPEN} src="https://${orgSlug}.legalese.cloud/.webmcp/embed.js" data-scope="${deploymentId}" data-tools="auto" data-api-key="sk_...">${SCRIPT_CLOSE}`,
            hint: 'Enable your website to serve your rules to visitors',
            doc: 'legalese-cloud/webmcp-embed.md',
          },
          {
            label: 'RESTful OpenAPI JSON specification',
            value: `https://${orgSlug}.legalese.cloud/${deploymentId}/openapi.json`,
            hint: 'Allow 3rd party systems to integrate and evaluate these rules',
            doc: 'legalese-cloud/openapi-spec.md',
          },
        ]
      : [
          {
            label: 'MCP server',
            value: `${trimmedHost}/${deploymentId}/.mcp`,
            hint: 'Use with any MCP enabled AI chat',
            doc: 'legalese-cloud/mcp-server.md',
          },
          {
            label: 'WebMCP embed script',
            value: `${SCRIPT_OPEN} src="${trimmedHost}/.webmcp/embed.js" data-scope="${deploymentId}" data-tools="auto" data-api-key="sk_...">${SCRIPT_CLOSE}`,
            hint: 'Enable your website to serve your rules to visitors',
            doc: 'legalese-cloud/webmcp-embed.md',
          },
          {
            label: 'RESTful OpenAPI JSON specification',
            value: `${trimmedHost}/${deploymentId}/openapi.json`,
            hint: 'Allow 3rd party systems to integrate and evaluate these rules',
            doc: 'legalese-cloud/openapi-spec.md',
          },
        ]
  )

  let copiedIndex = $state(-1)

  async function copy(value: string, index: number): Promise<void> {
    try {
      await navigator.clipboard.writeText(value)
      copiedIndex = index
      setTimeout(() => {
        if (copiedIndex === index) copiedIndex = -1
      }, 1200)
    } catch {
      // Webviews can restrict clipboard; the input is selectable as a
      // manual fallback.
    }
  }

  // Esc closes the dialog, matching the backdrop click. Window-level
  // so it works regardless of where focus currently sits.
  function onKeydown(e: KeyboardEvent): void {
    if (e.key === 'Escape') {
      e.stopPropagation()
      onClose()
    }
  }
</script>

<svelte:window on:keydown={onKeydown} />

<!-- Full-viewport backdrop: dims the deployment screen and centers the
     dialog. Clicking the backdrop (but not the dialog itself) closes. -->
<div
  class="integrate-backdrop"
  role="presentation"
  onclick={onClose}
  onkeydown={null}
>
  <div
    class="integrate-dialog"
    role="dialog"
    aria-modal="true"
    aria-label="Integrate deployment"
    onclick={(e) => e.stopPropagation()}
    onkeydown={null}
  >
    <div class="dialog-header">
      <span class="dialog-title">Integrate <strong>{deploymentId}</strong></span
      >
      <button
        class="dialog-close"
        onclick={onClose}
        title="Close"
        aria-label="Close">✕</button
      >
    </div>

    <div class="dialog-body">
      {#each sections as section, i (section.label)}
        <div class="section">
          <div class="section-label">{section.label}</div>
          <div class="input-row">
            <input
              class="value-input"
              type="text"
              readonly
              value={section.value}
              onfocus={(e) => e.currentTarget.select()}
            />
            <button
              class="copy-btn"
              onclick={() => copy(section.value, i)}
              title={copiedIndex === i ? 'Copied' : 'Copy'}
              aria-label="Copy to clipboard"
              >{copiedIndex === i ? '✓' : '⧉'}</button
            >
          </div>
          <div class="section-hint">
            {section.hint}
            <button
              class="learn-more"
              onclick={() => onLearnMore(`${DOCS_BASE}/${section.doc}`)}
              >Learn more</button
            >
          </div>
        </div>
      {/each}
    </div>
  </div>
</div>

<style>
  .integrate-backdrop {
    /* Absolute (not fixed) so the dim + dialog stay inside the
       deployments tab's positioned wrapper — the rest of the
       extension (tab bar, other panels) keeps working. */
    position: absolute;
    /* Fills `.tab-content-frame` (the non-scrolling visible-area box
       it's rendered into), so the dim covers the whole visible tab —
       padding included — and the dialog centres in the viewport
       regardless of scroll. Tab bar / footer sit outside the frame
       and stay interactive. */
    inset: 0;
    z-index: 1000;
    display: flex;
    align-items: center;
    justify-content: center;
    /* 20px gutter on every side → the dialog is a touch narrower
       than the tab. */
    padding: 32px;
    box-sizing: border-box;
    /* Light dim so the dialog reads as foreground without fully
       masking the tab. (15% black was effectively invisible on a
       dark sidebar.) The backdrop also catches outside-clicks. */
    background: rgba(0, 0, 0, 0.4);
  }

  .integrate-dialog {
    width: 100%;
    max-width: 520px;
    max-height: 100%;
    display: flex;
    flex-direction: column;
    /* Same surface the deployments tab uses normally, so the dialog
       reads as a lifted slice of the tab rather than a different
       material. */
    background: var(--vscode-sideBar-background);
    border: 1px solid
      var(
        --vscode-menu-border,
        var(--vscode-widget-border, rgba(128, 128, 128, 0.35))
      );
    border-radius: 6px;
    /* Even 16px-wide shadow on all sides (no offset) so the dialog
       reads as floating above the tab. */
    box-shadow: 0 0 36px rgba(0, 0, 0, 0.95);
    box-sizing: border-box;
  }

  .dialog-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 16px 20px;
    border-bottom: 1px solid
      var(--vscode-panel-border, rgba(128, 128, 128, 0.2));
  }

  .dialog-title {
    font-size: 1em;
  }

  .dialog-close {
    background: none;
    border: none;
    color: var(--vscode-foreground);
    cursor: pointer;
    opacity: 0.6;
    padding: 2px 2px;
    font-size: 1em;
    line-height: 1;
  }
  .dialog-close:hover {
    opacity: 1;
  }

  .dialog-body {
    padding: 4px 20px 16px;
    overflow-y: auto;
  }

  .section {
    padding: 16px 0;
    border-top: 1px solid var(--vscode-panel-border, rgba(128, 128, 128, 0.2));
  }
  .section:first-of-type {
    border-top: none;
  }

  .section-label {
    font-size: 0.9em;
    font-weight: 600;
    margin-bottom: 8px;
  }

  .input-row {
    display: flex;
    align-items: stretch;
    gap: 8px;
  }

  .value-input {
    flex: 1;
    min-width: 0;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 0.9em;
    padding: 7px 10px;
    background: var(--vscode-input-background);
    color: var(--vscode-input-foreground);
    border: 1px solid var(--vscode-input-border, transparent);
    border-radius: 4px;
  }

  .copy-btn {
    flex-shrink: 0;
    background: var(--vscode-button-secondaryBackground, transparent);
    color: var(--vscode-button-secondaryForeground, var(--vscode-foreground));
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    cursor: pointer;
    font-size: 1em;
    padding: 0 12px;
  }
  .copy-btn:hover {
    background: var(
      --vscode-button-secondaryHoverBackground,
      var(--vscode-list-hoverBackground)
    );
  }

  .section-hint {
    font-size: 0.9em;
    color: var(--vscode-descriptionForeground);
    margin-top: 8px;
  }

  .learn-more {
    background: none;
    border: none;
    padding: 0;
    margin-left: 6px;
    /* Extension primary action colour (crimson), matching the
       Submit / Deploy CTAs and the active-tab accent. */
    color: #c8376a;
    cursor: pointer;
    font-size: inherit;
  }
  .learn-more:hover {
    color: #d94d7e;
    text-decoration: underline;
  }
</style>
