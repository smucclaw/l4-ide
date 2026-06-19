# Publish your deployments to AI agents (gateway discovery)

Make an org's deployed rules available to an AI coding agent without dumping
hundreds of tools into its context. The agent installs **one** discovery skill,
then **searches** for the right rule and runs only that one — through the
[**`legalese` CLI**](#step-1--install--authenticate-the-cli), which brokers auth
so your credential never touches a command line.

It's built on open standards — [Agent Skills](https://agentskills.io) for the
skill and [MCP](https://modelcontextprotocol.io) / REST for execution —
distributed through a Claude Code plugin marketplace.

**Audience:** Anyone who wants an org's deployed rules usable from an AI agent
**Prerequisites:** the `legalese` CLI (see Step 1) + a Legalese Cloud credential (`legalese login`, or an `sk_…` API key for CI); at least one deployment with a non-empty **Intended use** description ([Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md))
**Time:** 5 minutes
**Goal:** Install the gateway discovery skill and run a rule through it

---

## How it works

The marketplace is **global and org-agnostic**: `https://skills.legalese.cloud/marketplace.json` lists a **single gateway discovery plugin** (`rules@legalese-cloud`), not every deployment. Installing it gives the agent a small `SKILL.md` that teaches it to:

1. **search** your account's rules — `legalese search "<topic>"`
2. **run** only the relevant one — `legalese eval <deployment> <function> '<json>'`

Org scope is resolved from your credentials, and only the matched rule's schema enters context (**progressive disclosure**) — so this scales to orgs with hundreds of deployments without flooding the model.

```
marketplace.json (public)  ──▶  one gateway discovery skill (rules@legalese-cloud)
        gateway SKILL.md    ──▶  legalese search   → find the rule        (gated)
                            ──▶  legalese eval      → run it              (gated)
   one `legalese login` (or LEGALESE_TOKEN) brokers auth for all of it
```

The full catalogue is never enumerated in public, and the agent never preloads a tool per deployment.

---

## Step 1 — Install & authenticate the CLI

The `legalese` CLI is the auth broker and execution surface.

```sh
npm install -g @legalese/cli      # early access — or build from the legalese-cli repo
```

Authenticate once. Interactive (browser OAuth, org resolved into the session):

```sh
legalese login nerdherd
```

Or, for CI / headless, use an org-scoped API key:

```sh
export LEGALESE_TOKEN=sk_your_key
export LEGALESE_ORG=nerdherd
```

> **Early access:** `legalese login` needs the CLI's WorkOS OAuth client, which is being provisioned — until it lands, use `LEGALESE_TOKEN`. The CLI's auth-resolution order is `--token` → `LEGALESE_TOKEN` → stored session, so the same commands work either way.

Verify:

```sh
legalese whoami
legalese search "rent overdue"     # confirms discovery works against your org
```

## Step 2 — Add the marketplace & install the gateway

In Claude Code:

```
/plugin marketplace add https://skills.legalese.cloud/marketplace.json
/plugin install rules@legalese-cloud
```

The gateway plugin carries no secrets (just the discovery skill), so the install needs **no credential**. The marketplace is named `legalese-cloud`; the plugin is `rules`.

## Step 3 — Let the agent use it

Ask the agent something the rules cover. Guided by the gateway skill, it will:

```sh
legalese search "is the rent overdue"
#   → matched: rent-rules → rentIsOverdue("…")
legalese eval rent-rules rentIsOverdue '{"dueDate":"2026-01-01","today":"2026-06-19"}'
#   → the authoritative decision
```

Only the matched rule's schema enters context, and the result is authoritative for what it covers — the agent should cite it rather than re-deriving the answer.

---

## Which agents can use this

The gateway skill + `legalese` CLI work in **any agent that can run a shell command**. The skill itself rides the [Agent Skills](https://agentskills.io) standard; execution is plain authenticated REST via the CLI (with MCP available for harnesses that prefer typed tools).

| Agent                       | Marketplace install (Claude Code) | Agent Skills (`SKILL.md`) | Runs the CLI |
| --------------------------- | :-------------------------------: | :-----------------------: | :----------: |
| **Claude Code**             |   ✅ `/plugin marketplace add`    |            ✅             |      ✅      |
| Claude Desktop / claude.ai  |                 —                 |            ✅             |      ✅      |
| GitHub Copilot (agent mode) |                 —                 |    ✅ (since Apr 2026)    |      ✅      |
| Cursor                      |                 —                 |            ✅             |      ✅      |
| OpenAI Codex CLI            |                 —                 |            ✅             |      ✅      |
| Gemini CLI                  |                 —                 |            ✅             |      ✅      |

For agents without the one-command marketplace install, drop the gateway `SKILL.md` where your agent reads skills (or just run the `legalese` CLI yourself). Cross-agent support is evolving — check your agent's current docs.

---

## Advanced: install one specific deployment directly

The gateway is the default. But if you want a single, sharply-scoped skill for one deployment (so the agent triggers on _that_ ruleset specifically), you can still install it on its own — the per-deployment artifacts are still served, just not listed in the global marketplace.

- **VS Code:** the [one-click install](./agent-plugin.md) writes a single deployment's skill + MCP server.
- **A gated skill repo** lives at `https://skills.legalese.cloud/{org}/{deployment}.git` (HTTP Basic → your token as the password). Point a `marketplace.json` you host at it, or `git clone` it directly. Set the git credential once:

  ```sh
  printf 'protocol=https\nhost=skills.legalese.cloud\nusername=x\npassword=YOUR_TOKEN\n' | git credential approve
  ```

  Or let the CLI broker it: `git config --global credential.https://skills.legalese.cloud.helper "legalese git-credential"`.

- **Call the tools over MCP** instead of the CLI by registering the deployment's MCP server (`https://mcp.legalese.cloud/{org}/{deployment}`) — see [MCP Server](./mcp-server.md). Use a `Bearer ${LEGALESE_TOKEN}` header, or omit it for the OAuth flow.

---

## Troubleshooting

- **`not logged in`** — run `legalese login <org>`, or set `LEGALESE_TOKEN` (+ `LEGALESE_ORG`).
- **`legalese search` returns nothing** — broaden the query, or confirm the org has deployments with an **Intended use** description (those without one aren't searchable).
- **`forbidden`** — your credential lacks the `l4:rules` permission, or it's scoped to a different org than `--org`/the session.
- **`marketplace add` fails** — check that `https://skills.legalese.cloud/marketplace.json` loads in a browser (it's public).
- **Gateway install needs no token** — if `/plugin install rules@legalese-cloud` asks for git credentials, you're on an older per-deployment repo URL, not the gateway.

## Notes

- The public catalog reveals **nothing** about your deployments — only the single gateway plugin. Deployment names, descriptions, and tools surface only through the authenticated `legalese search`.
- **Org scope is resolved from auth**, not the URL. This is the seam for a future where one account spans its own org plus public rules shared from other orgs — discovery just widens; nothing in the public manifest changes.
- The CLI spec (commands, token store, OAuth client) lives in the `jl4-auth-proxy` repo at `docs/legalese-cli.md`.
