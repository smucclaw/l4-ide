# Use an org's deployed rules from an AI agent (MCP)

Make an org's deployed decision rules usable by an AI agent, behind the org's permission barrier. The agent **discovers** the rule a question needs, then **runs** it — instead of reasoning the determination itself — over [MCP](https://modelcontextprotocol.io).

**Audience:** Anyone who wants an org's deployed rules usable from an AI agent
**Prerequisites:** a Legalese Cloud org with deployed rules (each with an **Intended use** description, see [Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md)); an MCP-capable agent (Claude Code/Desktop, Cursor, …) or a shell for the CLI; a credential (OAuth sign-in, or an `sk_…` API key)
**Time:** 5 minutes
**Goal:** Connect an agent to the org's rules and have it answer from them

---

## How it works

A small **discovery MCP** at `https://mcp.legalese.cloud` (no org in the URL — it's resolved from your sign-in) exposes a couple of tools: **search** the org's rules, and **get a function's schema**. Searching returns the matching **deployments** and, for each, the **per-deployment MCP endpoint** (`mcp.legalese.cloud/{org}/{deployment}`) where its rules live as typed tools.

So the agent searches, then connects to the matched deployment to run the rule — only the rules it actually uses ever enter context, whether the org has five deployments or five hundred.

```
discovery MCP  (mcp.legalese.cloud)        search_rules → a deployment + its mcp endpoint
       └─ per-deployment MCP (…/{org}/{deployment})   ← connect here; call its rule tools
```

## Step 1 — Add the rules

**Claude Code (one step):** install the gateway plugin — it bundles the discovery MCP (no token; OAuth on first use) plus a skill that primes the model on _when_ to use it:

```
/plugin marketplace add https://skills.legalese.cloud/marketplace.json
/plugin install rules@legalese-cloud
```

**Any MCP client:** add the discovery server directly (org resolved from auth — no slug in the URL):

```sh
claude mcp add --transport http legalese-rules https://mcp.legalese.cloud
```

OAuth runs on first use; for an API key instead, add `--header "Authorization: Bearer sk_..."`.

## Step 2 — Use it

Ask something the rules cover. The agent calls `search_rules` to find the deployment, then runs that deployment's rule — answering from the decision and citing it, rather than reasoning it out.

> Connecting the matched deployment's MCP server is, in most harnesses, a user/config action: the agent surfaces the endpoint; you (or a harness with dynamic-add support) connect it. For a ruleset you use often, add `https://mcp.legalese.cloud/<org>/<deployment>` up front.

## Alternative — the `legalese` CLI (shell / CI)

In a shell or non-MCP environment, the CLI hits the same rules end to end:

```sh
curl -fsSL https://legalese.cloud/cli/install.sh | sh   # or: npm i -g @legalese/cli
legalese login <org>                                    # or: export LEGALESE_TOKEN=sk_… ; export LEGALESE_ORG=<org>
legalese search "rent overdue"                          # find a rule
legalese eval <deployment> <function> '<json>'          # run it
```

> **Early access:** `legalese login` awaits the CLI's WorkOS OAuth client — until it lands, use `LEGALESE_TOKEN`.

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

- **Rules MCP won't connect** — the server URL is `https://mcp.legalese.cloud` (no org slug; the org is resolved from auth). Your harness runs OAuth on first use; or supply an `Authorization: Bearer sk_…` header.
- **`not logged in`** (CLI) — run `legalese login <org>`, or set `LEGALESE_TOKEN` (+ `LEGALESE_ORG`).
- **`legalese search` returns nothing** — broaden the query, or confirm the org has deployments with an **Intended use** description (those without one aren't searchable).
- **`forbidden`** — your credential lacks the `l4:rules` permission, or it's scoped to a different org than `--org`/the session.
- **`marketplace add` fails** — check that `https://skills.legalese.cloud/marketplace.json` loads in a browser (it's public).
- **Gateway install needs no token** — if `/plugin install rules@legalese-cloud` asks for git credentials, you're on an older per-deployment repo URL, not the gateway.

## Notes

- The public catalog reveals **nothing** about your deployments — only the single gateway plugin. Deployment names, descriptions, and tools surface only behind auth, via the discovery MCP's `search_rules` (or `legalese search`).
- **Org scope is resolved from auth**, not the URL. This is the seam for a future where one account spans its own org plus public rules shared from other orgs — discovery just widens; nothing in the public manifest changes.
- The CLI spec (commands, token store, OAuth client) lives in the `jl4-auth-proxy` repo at `docs/legalese-cli.md`.
