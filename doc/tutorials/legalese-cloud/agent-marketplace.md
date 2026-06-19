# Publish your deployments to AI agents (Agent Skills + MCP)

Expose an org's deployed rules to AI coding agents using **open standards** — [**Agent Skills**](https://agentskills.io) (`SKILL.md`) so the agent knows _when_ to use a deployment, and [**MCP**](https://modelcontextprotocol.io) so it can _call_ the rules — distributed through a **marketplace manifest**. Claude Code consumes the whole flow with one command; other agents can consume the portable pieces (`SKILL.md` + MCP server) directly.

**Audience:** Anyone who wants an org's deployed rules installable into an AI agent with a single credential
**Prerequisites:** A Legalese Cloud **API key** (`sk_…`) for the org with the `l4:rules` permission; at least one deployment with a non-empty **Intended use** description ([Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md))
**Time:** 5 minutes
**Goal:** Publish the org's deployments so an agent discovers them as skills and calls them as MCP tools

> **Credential:** anywhere this tutorial says "token", use your API key (`sk_…`). (An AuthKit JWT also works as a Bearer credential, but those are short-lived and expire — the API key is the long-lived one.) Prefer no stored secret at all? See [Alternative: OAuth instead of a stored token](#alternative-oauth-instead-of-a-stored-token).

> **New: single gateway discovery (recommended).** The marketplace is now a **global, org-agnostic** manifest at `https://skills.legalese.cloud/marketplace.json` that lists **one** plugin — a discovery skill — instead of enumerating every deployment. Add it and install the gateway:
>
> ```
> /plugin marketplace add https://skills.legalese.cloud/marketplace.json
> /plugin install rules@legalese-cloud
> ```
>
> The gateway skill then **searches** your account's rules and runs only the relevant one (progressive disclosure — it never loads the whole catalogue), with the org resolved from your credentials. That search/run flow is driven by the **`legalese` CLI** (the auth broker), which is in progress. The per-deployment instructions below remain valid for installing **one specific deployment** directly via its gated repo.

---

## The three standards involved

Your deployments are published through three layers, each an independent, increasingly-portable standard:

| Layer       | What it is                                                                                     | Standard                                         | Where it's hosted                                                         |
| ----------- | ---------------------------------------------------------------------------------------------- | ------------------------------------------------ | ------------------------------------------------------------------------- |
| **Catalog** | Global manifest listing one gateway discovery plugin (org resolved from auth, not the URL)     | Plugin marketplace manifest (`marketplace.json`) | **public** `https://skills.legalese.cloud/marketplace.json`               |
| **Skill**   | A `SKILL.md` that tells the agent _when_ to use a deployment, plus its `.mcp.json` and schemas | [Agent Skills](https://agentskills.io)           | **gated** git repo `https://skills.legalese.cloud/{org}/{deployment}.git` |
| **Tools**   | The actual callable decision functions                                                         | [MCP](https://modelcontextprotocol.io)           | **gated** `https://mcp.legalese.cloud/{org}/{deployment}`                 |

One token (your API key) authenticates the two gated layers — as the git password when installing, and as the MCP Bearer at runtime. It's never written into the public catalog.

```
marketplace.json   (public, no auth)        ── the directory: what exists
  └─ SKILL.md      (gated → token)          ── when to use it
     └─ MCP server (gated → token / OAuth)  ── the actual decision tools
```

## Which agents can use this

Support differs per layer. The **`SKILL.md` and MCP server are the portable core**; the one-command marketplace _install_ is, for now, mostly a Claude Code convenience.

| Agent                       |     Marketplace install      | Agent Skills (`SKILL.md`) | MCP tools |
| --------------------------- | :--------------------------: | :-----------------------: | :-------: |
| **Claude Code**             | ✅ `/plugin marketplace add` |            ✅             |    ✅     |
| Claude Desktop / claude.ai  |              —               |            ✅             |    ✅     |
| GitHub Copilot (agent mode) |              —               |    ✅ (since Apr 2026)    |    ✅     |
| Cursor                      |              —               |            ✅             |    ✅     |
| OpenAI Codex CLI            |              —               |            ✅             |    ✅     |
| Gemini CLI                  |              —               |            ✅             |    ✅     |
| Windsurf / Cline / Zed / …  |              —               |          varies           |    ✅     |

The Agent Skills format is an open spec ([agentskills.io](https://agentskills.io)) that a growing list of agents read; MCP is supported nearly everywhere. So even agents that don't consume the marketplace manifest can use the **same** `SKILL.md` and the **same** MCP server — see [Other agents](#other-agents) below. (Cross-tool support is evolving fast; verify your agent's current docs.)

---

## Walkthrough — Claude Code (worked example)

This is the one-command path, verified end to end.

### Step 1 — Give git your token (authenticates the install)

The skill repos answer an unauthenticated clone with `401` and an HTTP Basic challenge; git supplies your token from its credential store. Run once (the username is ignored — only the password matters):

```sh
printf 'protocol=https\nhost=skills.legalese.cloud\nusername=x\npassword=YOUR_TOKEN\n' | git credential approve
```

Prefer a credentials file:

```sh
printf 'https://x:YOUR_TOKEN@skills.legalese.cloud\n' >> ~/.git-credentials
git config --global credential.helper store   # if not already enabled
```

> The `git-credential-store` (plaintext file) helper is the most reliable choice with Claude Code; it sidesteps reported cases where Claude Code's bundled git doesn't pick up the macOS Keychain helper. If `/plugin install` fails even though a plain `git clone` of the repo works in your terminal, switch to `store`.

### Step 2 — Export your token for MCP (authenticates tool calls)

The installed `.mcp.json` sends `Authorization: Bearer ${LEGALESE_TOKEN}`. Set the variable to the **same** token from Step 1, **before launching Claude Code** (add it to your shell profile to persist):

```sh
export LEGALESE_TOKEN=YOUR_TOKEN
```

> If `LEGALESE_TOKEN` is unset, Claude Code refuses to load the MCP config (a loud error) rather than silently sending an empty credential.

### Step 3 — Add the marketplace

```
/plugin marketplace add https://skills.legalese.cloud/marketplace.json
```

This global, org-agnostic manifest lists the single **gateway discovery** plugin (`rules@legalese-cloud`); see the banner at the top. (The previous per-org `…/{org}/marketplace.json` has been removed — org scope is now resolved from your credentials, not the URL.) To install **one specific deployment** directly instead of the gateway, point a marketplace.json you host at that deployment's gated repo (`https://skills.legalese.cloud/{org}/{deployment}.git`) — the gated repos described below still serve per-deployment skills.

### Step 4 — Install a deployment

Browse with `/plugin`, or install directly — each deployment is a plugin named `{deployment}` in the marketplace named `{org}` (the `@org` scope supplies the org, so the plugin name doesn't repeat it):

```
/plugin install rent-rules@nerdherd
```

> Plugin names are the deployment id, lowercased with non-alphanumeric characters turned into hyphens. If unsure, open `/plugin` and copy the exact name. On install, Claude Code clones the gated `…/rent-rules.git` repo, supplying the token you stored in Step 1.

### Step 5 — Let the agent find and use it

The installed plugin contributes a `SKILL.md` whose **"When to use"** text triggers the agent, plus a `.mcp.json` registering the deployment's MCP server. **Restart Claude Code** (or run `/mcp`) so the server connects, then ask something the rules cover — the agent matches the skill and calls the decision functions over MCP, authenticating with `${LEGALESE_TOKEN}`. Confirm with `/mcp`.

---

## Other agents

Agents that don't consume the marketplace manifest can still use the two portable layers directly:

- **Skills** — the same `SKILL.md` works in any [Agent Skills](https://agentskills.io)-compatible agent (Cursor, GitHub Copilot agent mode, Codex CLI, Gemini CLI, …). Fetch it from the gated repo (`git clone https://skills.legalese.cloud/{org}/{deployment}.git`, with your token as the git password) and place it where your agent reads skills. For VS Code Chat specifically, the [VS Code one-click install](./agent-plugin.md) writes the skill to the path it reads natively.
- **Tools** — register the deployment's MCP server directly in your agent's MCP config; no skills or marketplace needed:

  ```jsonc
  {
    "mcpServers": {
      "nerdherd-rent-rules": {
        "type": "http",
        "url": "https://mcp.legalese.cloud/nerdherd/rent-rules",
        "headers": { "Authorization": "Bearer YOUR_TOKEN" },
      },
    },
  }
  ```

  See [MCP Server](./mcp-server.md) for the full MCP setup. Most MCP clients also support the OAuth flow (omit the header) instead of a static token.

The marketplace manifest itself is plain JSON ([`/{org}/marketplace.json`](https://skills.legalese.cloud)), so any tooling can read it to discover an org's deployments even without native "marketplace" support.

---

## Alternative: OAuth instead of a stored token

The token above is the simplest path and works headless (CI, shared key, no browser). To avoid storing a credential for MCP, use the MCP server's built-in **OAuth** flow instead:

1. After installing, open the plugin's `.mcp.json` and **remove the `headers` block** (leaving just `type` and `url`).
2. Restart the agent and (re)connect — on first connect it does a browser sign-in (WorkOS), then mints and **auto-refreshes** a short-lived JWT. No `LEGALESE_TOKEN` needed.

Two things to know:

- It's **all-or-nothing per server**: a config with an `Authorization` header uses the token and never falls back to OAuth; one without it always uses OAuth. A single static config can't "use the token if set, otherwise OAuth."
- The **install** (Step 1) still needs a stored git credential — a short-lived OAuth token would expire in the credential store — so keep an API key for git even if MCP uses OAuth.

## The mental model

- **`marketplace.json`** (public) — the directory: _what exists_.
- **`SKILL.md`** (gated) — _when to use it_ and how it's wired.
- **MCP server** (gated) — _the actual decision tools_.

## Troubleshooting

- **`marketplace add` fails** — check the org slug and that `https://skills.legalese.cloud/{org}/marketplace.json` loads in a browser (it's public). An org with no described deployments returns an empty `plugins` list.
- **`install` fails with an auth/clone error** — your git credential isn't being offered. Re-run Step 1, and verify with `git credential fill` (type `protocol=https`, `host=skills.legalese.cloud`, then two blank lines) that it echoes your token.
- **MCP server won't connect / tools missing** — the token env var wasn't set in the environment the agent launched from, or the token lacks the `l4:rules` permission. Set it, then restart.
- **Install fails but `git clone` works in your terminal** — a known Claude Code issue where its bundled git doesn't always use the configured credential helper. Use the `git-credential-store` setup in Step 1; as a last resort, `git clone` the `.git` URL manually to confirm your token works.
- **A deployment is missing from the catalog** — it has no **Intended use** description. Add one in the sidebar and redeploy; deployments without a description are never published.

## Notes

- The catalog at `/{org}/marketplace.json` is **public** and lists every published deployment in the org (names + blurbs only — no rule logic). Anyone who knows the org slug can read it; the skill bodies and tool calls remain gated by your token.
- A plugin installs as `{deployment}@{org}`. Its bundled MCP server and skill are org-qualified internally (`{org}-{deployment}`) so multiple orgs' deployments never collide once installed, each scoped to its own ruleset.
- **Updates are manual in Claude Code.** Its _background_ auto-update doesn't use the git credential helper (it only authenticates against GitHub/GitLab/Bitbucket via their token env vars, which don't apply to `skills.legalese.cloud`). Run `/plugin marketplace update {org}` (and reinstall if a deployment changed) to pull the latest.
- Prefer the [VS Code one-click install](./agent-plugin.md) for a single deployment when you're already in the extension; prefer this marketplace flow for a whole org's catalog or a single-token (or OAuth) setup across agents.
- Cross-agent support for these standards is evolving quickly — the support table above reflects what was known when written; check your agent's current docs.
