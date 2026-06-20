# Use an org's deployed rules from an AI agent (MCP)

Make an org's deployed decision rules callable by an AI agent. The rules are
served as **[MCP](https://modelcontextprotocol.io) tools** at `mcp.legalese.cloud`
— the agent connects once (OAuth), finds the rule a question turns on, and calls
it instead of reasoning the determination itself. A small **gateway skill**
(distributed via a Claude Code marketplace) primes the model on _when_ to reach
for the rules; the **`legalese` CLI** is an alternative for shells/CI.

**Audience:** Anyone who wants an org's deployed rules usable from an AI agent
**Prerequisites:** a Legalese Cloud org with deployed rules (each with an **Intended use** description, see [Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md)); an MCP-capable agent (Claude Code/Desktop, Cursor, …) or a shell for the CLI; a credential (sign in via OAuth, or an `sk_…` API key)
**Time:** 5 minutes
**Goal:** Connect an agent to the org's rules and have it answer from them

---

## How it works

`https://mcp.legalese.cloud/<org>` exposes the org's deployed rules as MCP tools (one per exported rule) plus search, behind OAuth (scopes `l4:rules` / `l4:read` / `l4:evaluate`). The agent connects once, discovers the relevant rule, and calls it — only the rules it uses enter context.

Optionally, the global marketplace `https://skills.legalese.cloud/marketplace.json` ships one **gateway skill** (`rules@legalese-cloud`) that tells the model when to use the rules and where the MCP server is — so it reaches for a deployed rule rather than guessing a determination.

## Step 1 — Connect the rules MCP server

Point your agent at the org-wide endpoint (the client runs the OAuth flow on first use):

```sh
claude mcp add --transport http legalese-rules https://mcp.legalese.cloud/<org>
```

- Prefer an API key over OAuth? Add `--header "Authorization: Bearer sk_..."`.
- Want a single ruleset only? Use the deployment endpoint `…/<org>/<deployment>`.
- Any MCP client works — add the same URL in Cursor, Claude Desktop, etc.

## Step 2 (optional) — Add the gateway skill for _when to use_

In Claude Code:

```
/plugin marketplace add https://skills.legalese.cloud/marketplace.json
/plugin install rules@legalese-cloud
```

The gateway plugin carries no secrets, so this needs **no credential**. It nudges the model to consult the rules MCP and answer from a rule rather than reasoning the determination itself.

## Step 3 — Use it

Ask something the rules cover. The agent lists the MCP tools, calls the matching rule, and answers from its decision (citing it). Only the rules it uses enter context — so this works whether the org has five deployments or five hundred.

## Alternative — the `legalese` CLI (shell / CI)

For shells or non-MCP environments, the CLI hits the same rules:

```sh
npm install -g @legalese/cli            # early access; or build from the legalese-cli repo
legalese login <org>                    # or: export LEGALESE_TOKEN=sk_… ; export LEGALESE_ORG=<org>
legalese search "rent overdue"          # find a rule
legalese eval <deployment> <function> '<json>'   # run it
```

> **Early access:** `legalese login` needs the CLI's WorkOS OAuth client (being provisioned) — until it lands, use `LEGALESE_TOKEN`.

---

## Which agents can use this

Any **MCP-capable** agent can connect to the rules server (Step 1). The optional gateway **skill** ([Agent Skills](https://agentskills.io) standard) adds the _when-to-use_ priming where supported; the **CLI** covers shells / non-MCP environments.

| Agent                         | MCP server (Step 1) |  Gateway skill (`SKILL.md`)  | CLI |
| ----------------------------- | :-----------------: | :--------------------------: | :-: |
| **Claude Code**               |         ✅          | ✅ `/plugin marketplace add` | ✅  |
| Claude Desktop / claude.ai    |         ✅          |              ✅              |  —  |
| Cursor / Windsurf / Cline     |         ✅          |            varies            |  —  |
| GitHub Copilot (agent mode)   |         ✅          |     ✅ (since Apr 2026)      | ✅  |
| OpenAI Codex CLI / Gemini CLI |         ✅          |              ✅              | ✅  |

MCP is the common surface; the skill and CLI are conveniences on top. Cross-agent support is evolving — check your agent's current docs.

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
