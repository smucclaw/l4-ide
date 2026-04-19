<script lang="ts">
  import type { Messenger } from 'vscode-messenger-webview'
  import type { GetSidebarConnectionStatusResponse } from 'jl4-client-rpc'
  import {
    HOST_EXTENSION,
    type WebviewTypeMessageParticipant,
  } from 'vscode-messenger-common'
  import { RequestSidebarLogin } from 'jl4-client-rpc'

  let {
    messenger,
    connectionStatus,
  }: {
    messenger: InstanceType<typeof Messenger> | null
    connectionStatus: GetSidebarConnectionStatusResponse
  } = $props()

  // Scope: Phase 1 scaffold only. This panel currently renders the
  // unauthenticated CTA or the empty-state "Get started" block. Full
  // streaming chat, input, tool rendering, history, and settings arrive
  // in subsequent Phase 1 commits. See ts-apps/vscode/AI_CHAT_PLAN.md.

  const signedIn = $derived(
    connectionStatus.connected && connectionStatus.isLegaleseCloud
  )

  function signIn() {
    messenger?.sendNotification(
      RequestSidebarLogin,
      HOST_EXTENSION as WebviewTypeMessageParticipant | typeof HOST_EXTENSION,
      undefined as never
    )
  }

  // Seed prompts for the empty state. Click behaviors arrive with
  // file-picker + textarea wiring in a later Phase 1 commit; for now
  // clicks are no-ops so the tab is visible and discoverable.
  const seeds = [
    {
      label: 'Turn my policy document into L4 for use as MCP',
      needsFile: true,
    },
    {
      label: 'Convert my spreadsheet into REST APIs I can use as webhooks',
      needsFile: true,
    },
    {
      label:
        'Help me convert a legal text to L4 and create example scenarios challenging the rules',
      needsFile: false,
    },
  ]
</script>

<div class="ai-panel">
  {#if !signedIn}
    <div class="signin-cta">
      <p class="cta-text">
        Sign in to Legalese Cloud to start composing rules with AI.
      </p>
      <button class="cta-button" onclick={signIn}>Sign in</button>
    </div>
  {:else}
    <div class="empty-state">
      <p class="empty-heading">Get started</p>
      <div class="seed-buttons">
        {#each seeds as seed}
          <button class="seed-button" type="button">
            {seed.label}
          </button>
        {/each}
      </div>
    </div>
  {/if}
</div>

<style>
  .ai-panel {
    display: flex;
    flex-direction: column;
    height: 100%;
    min-height: 0;
    padding: 16px;
    box-sizing: border-box;
  }

  .signin-cta {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 12px;
    flex: 1;
    text-align: center;
  }

  .cta-text {
    margin: 0;
    color: var(--vscode-descriptionForeground);
    font-size: 13px;
    max-width: 280px;
    line-height: 1.4;
  }

  .cta-button {
    background: var(--vscode-button-background);
    color: var(--vscode-button-foreground);
    border: none;
    padding: 6px 14px;
    border-radius: 2px;
    cursor: pointer;
    font-size: 13px;
  }

  .cta-button:hover {
    background: var(--vscode-button-hoverBackground);
  }

  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    gap: 12px;
    flex: 1;
  }

  .empty-heading {
    margin: 0;
    color: var(--vscode-descriptionForeground);
    font-size: 12px;
    text-transform: uppercase;
    letter-spacing: 0.06em;
  }

  .seed-buttons {
    display: flex;
    flex-direction: column;
    gap: 8px;
    width: 100%;
    max-width: 320px;
  }

  .seed-button {
    width: 100%;
    text-align: left;
    background: transparent;
    color: var(--vscode-foreground);
    border: 1px solid var(--vscode-widget-border, rgba(128, 128, 128, 0.35));
    border-radius: 4px;
    padding: 10px 12px;
    font-size: 12px;
    line-height: 1.4;
    cursor: pointer;
  }

  .seed-button:hover {
    background: var(--vscode-list-hoverBackground);
    border-color: var(--vscode-focusBorder, rgba(128, 128, 128, 0.55));
  }
</style>
