<script lang="ts">
  // Shown when the requested org is not the one the current session speaks for.
  // We deliberately render a generic 404 (never a 403) so deployments in other
  // orgs are not enumerable.
  let {
    title = 'Not found',
    detail = '',
  }: { title?: string; detail?: string } = $props()
</script>

<main class="nf">
  <div class="nf-card">
    <div class="code">404</div>
    <h1>{title}</h1>
    {#if detail}<p>{detail}</p>{/if}
  </div>
</main>

<style>
  .nf {
    display: grid;
    place-items: center;
    height: 100vh;
    background: var(--chat-bg);
  }
  .nf-card {
    text-align: center;
    color: var(--vscode-foreground);
    max-width: 24rem;
    padding: 0 1.5rem;
  }
  .code {
    font-size: 3rem;
    font-weight: 700;
    color: var(--brand);
    line-height: 1;
  }
  h1 {
    font-weight: 600;
    margin: 0.75rem 0 0.5rem;
  }
  p {
    color: var(--vscode-descriptionForeground);
    margin: 0;
  }
</style>
