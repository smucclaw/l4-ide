# Install an org's deployed rules as AI agent skills (Skills Marketplace)

The **Skills Marketplace** turns an org's deployed decision rules into an installable **agent skill** — one entry that any supported AI harness can add in a single step. Once installed, the agent **discovers** the rule a question needs and **runs** it (behind the org's permission barrier) instead of reasoning the determination itself.

The marketplace is the org-agnostic **gateway**: one public entry (`https://skills.legalese.cloud/marketplace.json`) that, on install, wires up two things behind your sign-in — a **skill** that primes the model on _when_ to reach for the org's rules, and the **rules MCP server** it calls to find and run them. Org scope is resolved from auth, so nothing about your deployments is exposed in the public catalog.

**Audience:** Anyone who wants an org's deployed rules usable from an AI agent
**Prerequisites:** a Legalese Cloud org with deployed rules (each with an **Intended use** description, see [Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md)); a supported harness (Claude Code/Desktop, VS Code, Cursor, Windsurf, Cline, …) or a shell for the CLI; a credential (OAuth sign-in, or an `sk_…` API key)
**Time:** 5 minutes
**Goal:** Install the marketplace into your harness and have the agent answer from the org's rules

---

## What the marketplace enables

Without it, connecting an agent to an org's rules means hand-wiring an MCP server per harness and hoping the model knows when to use it. The marketplace collapses that into one install that gives every harness the same capability:

- **One source, many harnesses** — the same gateway entry installs into Claude Code, VS Code (Copilot), Cursor, Windsurf, Cline, and Claude Desktop.
- **A skill, not just a server** — bundles `SKILL.md` ([Agent Skills](https://agentskills.io) standard) so the model knows _when_ a question should be answered from the org's rules, not only _how_ to call them.
- **Scoped by sign-in, not URL** — the gateway URL carries no org slug; the org (and your permissions) come from auth. The public catalog reveals only the single gateway plugin — never your deployment names, descriptions, or tools.
- **Scales to any number of rules** — only the rules the agent actually uses ever enter context, whether the org has five deployments or five hundred.

### How it works under the hood

The skill calls a **discovery MCP** at `https://mcp.legalese.cloud` (no org in the URL — resolved from your sign-in) exposing two tools: **search** the org's rules, and **get a function's schema**. Searching returns the matching **deployments** and, for each, the **per-deployment MCP endpoint** (`mcp.legalese.cloud/{org}/{deployment}`) where its rules live as typed tools.

```
discovery MCP  (mcp.legalese.cloud)        search_rules → a deployment + its mcp endpoint
       └─ per-deployment MCP (…/{org}/{deployment})   ← connect here; call its rule tools
```

So the agent searches, then connects to the matched deployment to run the rule — answering from the decision and citing it, rather than reasoning it out.

---

## Install across harnesses

### The L4 VS Code extension (one click, any harness)

The easiest path for **any** supported harness is the L4 VS Code extension: open the **Deployments** tab and use **Install Skills Marketplace**, then pick your harness from the dropdown (Claude Code, VS Code/Copilot, Cursor, Windsurf, Cline, Claude Desktop).

It registers the rules MCP server (`https://mcp.legalese.cloud`, org from auth) directly in that harness's own config — and for Claude Code it installs the full plugin (marketplace + skill) via the Claude CLI when available. No token is baked in; OAuth runs on first use.

### Claude Code (native plugin commands)

If you'd rather drive Claude Code directly, add the marketplace and install the gateway plugin — it bundles the discovery MCP plus the when-to-use skill:

```
/plugin marketplace add https://skills.legalese.cloud/marketplace.json
/plugin install rules@legalese-cloud
```

(If `install` reports "not found in any marketplace," confirm `/plugin
marketplace list` shows `legalese-cloud`. A git form,
`https://skills.legalese.cloud/marketplace.git`, is also served — only needed
as a fallback for the URL-based relative-path limitation, which this
marketplace doesn't hit.)

### Any MCP client (manual wiring)

To configure the rules server by hand, point any MCP client at the bare discovery endpoint (org resolved from auth — no slug in the URL):

```sh
claude mcp add --transport http legalese-rules https://mcp.legalese.cloud
```

OAuth runs on first use; for an API key instead, add `--header "Authorization: Bearer sk_..."`. For a single ruleset, use `https://mcp.legalese.cloud/<org>/<deployment>`. This gives you the rules server without the when-to-use skill.

---

## Use it

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

## Which harnesses can use this

The marketplace installs the **rules MCP server** everywhere; the bundled **skill** adds _when-to-use_ priming where the harness supports skills; the **CLI** covers shells / non-MCP environments. The VS Code extension's **Install Skills Marketplace** automates the wiring for every harness in this table.

| Harness                       | Rules MCP server | One-click (VS Code ext) |  Gateway skill (`SKILL.md`)  | CLI |
| ----------------------------- | :--------------: | :---------------------: | :--------------------------: | :-: |
| **Claude Code**               |        ✅        |           ✅            | ✅ `/plugin marketplace add` | ✅  |
| Claude Desktop / claude.ai    |        ✅        |           ✅            |              ✅              |  —  |
| VS Code (Copilot)             |        ✅        |           ✅            |            varies            | ✅  |
| Cursor / Windsurf / Cline     |        ✅        |           ✅            |            varies            |  —  |
| OpenAI Codex CLI / Gemini CLI |        ✅        |            —            |              ✅              | ✅  |

MCP is the common surface; the skill and CLI are conveniences on top. Cross-harness support is evolving — check your harness's current docs.

---

## Advanced: install one specific deployment directly

The gateway is the default. But if you want a single, sharply-scoped skill for one deployment (so the agent triggers on _that_ ruleset specifically), you can still install it on its own — the per-deployment artifacts are still served, just not listed in the global marketplace.

- **VS Code:** the [one-click install](./agent-plugin.md) writes a single deployment's skill + MCP server (or use **Download plugin zip** in the deployment's **Integrate** dialog to grab the bundle).
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
