<script lang="ts">
  // Sign-in promo shown in the AI and Deployments tabs when the user
  // isn't connected to Legalese Cloud. Replaces the old one-line
  // "Sign in with Legalese Cloud to ..." hint with a small pitch +
  // a sign-in call to action. `context` tweaks the lead line so each
  // tab reads naturally; the benefit list is shared.
  let {
    context = 'ai',
    onSignIn,
  }: {
    context?: 'ai' | 'deployments'
    onSignIn: () => void
  } = $props()

  const lead = $derived(
    context === 'deployments'
      ? 'Connect to Legalese Cloud to host and share your deployments.'
      : 'Connect to Legalese Cloud to compose and check rules with AI.'
  )
</script>

<div class="cloud-upsell">
  <div class="card">
    <div class="brand">
      <span class="brand-mark" aria-hidden="true">§</span>
      <span class="brand-name">Legalese Cloud</span>
    </div>

    <p class="lead">{lead}</p>

    <ul class="benefits">
      <li>
        <strong>Free daily AI usage</strong> — easily draft and check L4 rules from
        existing policy and contracts.
      </li>
      <li>
        <strong>One-click deployments</strong> — your rules as downloadable AI skills,
        MCP tools, REST APIs, and AI endpoints.
      </li>
      <li>
        <strong>Share &amp; collaborate</strong> — make rules available to teammates
        and other agents.
      </li>
      <li>
        <strong>Enterprise grade</strong> — Observability, SSO and strong security
        built-in.
      </li>
    </ul>

    <button class="signin-btn" onclick={onSignIn}>
      Sign in with Legalese Cloud
    </button>

    <p class="footnote">Free to start — no credit card required.</p>
  </div>
</div>

<style>
  .cloud-upsell {
    display: flex;
    align-items: center;
    justify-content: center;
    /* Grow to fill the parent flex column (.ai-panel /
       .deployments-tab-body) so the card centres in the full tab
       height. flex-grow works off the container's used height, which
       a bare `min-height: 100%` ancestor wouldn't make definite enough
       for a percentage `height` to resolve against. */
    flex: 1 1 auto;
    height: 100%;
    min-height: 40vh;
    padding: 32px;
    box-sizing: border-box;
  }

  .card {
    width: 100%;
    max-width: 336px;
    padding: 29px 32px 32px;
    border: 1px solid var(--vscode-panel-border, #444);
    border-radius: 4px;
    background: var(
      --vscode-editorWidget-background,
      var(--vscode-sideBar-background)
    );
  }

  .brand {
    display: flex;
    align-items: center;
    gap: 6px;
    margin-bottom: 10px;
  }

  .brand-mark {
    padding: 0 3px;
    color: #c8376a;
    /* A 90°-rotated section sign reads as the Legalese swoosh. */
    font-size: 2em;
    line-height: 1;
    display: inline-block;
    transform: rotate(90deg);
  }

  .brand-name {
    font-weight: 600;
    font-size: 1.3em;
  }

  .lead {
    margin: 0 0 12px;
    line-height: 1.35;
    color: var(--vscode-descriptionForeground);
  }

  .benefits {
    list-style: none;
    margin: 0 0 14px;
    padding: 0;
    display: flex;
    flex-direction: column;
    gap: 10px;
  }

  .benefits li {
    position: relative;
    padding-left: 18px;
    line-height: 1.35;
  }

  .benefits li::before {
    content: '✓';
    position: absolute;
    left: 0;
    top: 0;
    color: #c8376a;
    font-weight: 700;
  }

  .benefits strong {
    font-weight: 600;
  }

  .signin-btn {
    width: 100%;
    /* The footer action button looks taller than its 5px padding
       because it stretches to the two-line footer's height (~38px).
       Match that height here with extra vertical padding. */
    padding: 11px 10px;
    font-size: 0.88em;
    font-weight: 500;
    border: none;
    border-radius: 4px;
    background: #c8376a;
    color: #fff;
    cursor: pointer;
  }

  .signin-btn:hover {
    background: #d94d7e;
  }

  .footnote {
    margin: 10px 0 0;
    text-align: center;
    color: var(--vscode-descriptionForeground);
  }
</style>
