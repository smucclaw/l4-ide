<script lang="ts">
  import { onMount } from 'svelte'
  import { auth } from '$lib/auth/session.svelte'

  // Root landing. The chat is always opened at /{org}/{deployment}; this page
  // just orients an authenticated user or offers sign-in.
  onMount(() => {
    void auth.init()
  })
</script>

<main class="shell">
  <div class="card">
    <div class="brand">Legalese AI</div>
    {#if auth.phase === 'loading'}
      <span class="spinner"></span>
    {:else if auth.phase === 'unauthenticated'}
      <p>Sign in to access your deployment chats.</p>
      <button class="btn" onclick={() => auth.login()}>Sign in</button>
    {:else}
      <p>
        Open a deployment chat at
        <code>/{auth.orgSlug}/{'{deployment}'}</code>.
      </p>
    {/if}
  </div>
</main>

<style>
  .shell {
    display: grid;
    place-items: center;
    height: 100vh;
    background: var(--chat-bg);
  }
  .card {
    text-align: center;
    color: var(--vscode-foreground);
  }
  .brand {
    font-weight: 600;
    color: var(--brand);
    font-size: 1.2rem;
    margin-bottom: 0.5rem;
  }
  p {
    color: var(--vscode-descriptionForeground);
  }
  code {
    background: var(--vscode-list-hoverBackground);
    padding: 0.1rem 0.35rem;
    border-radius: 4px;
  }
  .btn {
    padding: 0.5rem 1.25rem;
    border: none;
    border-radius: 8px;
    background: var(--brand);
    color: #fff;
    font: inherit;
    font-weight: 500;
    cursor: pointer;
  }
  .btn:hover {
    background: var(--brand-hover);
  }
  .spinner {
    display: inline-block;
    width: 24px;
    height: 24px;
    border: 3px solid var(--vscode-widget-border);
    border-top-color: var(--brand);
    border-radius: 50%;
    animation: spin 0.7s linear infinite;
  }
  @keyframes spin {
    to {
      transform: rotate(360deg);
    }
  }
</style>
