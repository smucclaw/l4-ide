# Install a deployment as an AI agent plugin

Bundle a deployment's rules into a Claude Code / VS Code Chat plugin so an agent can both **read the rules' "Intended use"** (a SKILL.md the model matches by description) and **call the rules as MCP tools**.

**Prerequisites:** A deployment with a non-empty **Intended use** description ([Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md))
**Time:** 1 minutes
**Goal:** One-click install the deployment so the agent knows when to use it

---

## When to Use This

A bare [MCP server](./mcp-server.md) entry registers the tools but leaves the agent guessing about _when_ to reach for them. A plugin install adds the deployment's `SKILL.md` — the operator-supplied "Intended use" text — so the agent knows the domain of the rules and triggers the right tool on the right user question.

Prefer the plugin install over the raw MCP entry whenever you want the agent to invoke rules proactively (rather than only when the user explicitly asks).

## What gets installed

A single plugin bundle that contains:

- `SKILL.md` — frontmatter `name` + `description` come from the deployment's **Intended use**; body lists every exported decision with its `@desc`.
- `.mcp.json` — pre-registers `https://mcp.legalese.cloud/{orgSlug}/{deploymentId}` as the MCP server.
- `references/schemas.json` — the deployment's function schemas (the same JSON the agent gets from MCP `tools/list`), bundled so it can be inspected offline.
- `.claude-plugin/plugin.json` — the Claude Code / VS Code Chat plugin manifest.

Everything is templated deterministically from the deployment's metadata — there is no LLM step. Editing the **Intended use** field in the sidebar and redeploying regenerates the SKILL.md the next time you install.

## Install

In the L4 VS Code extension, open **Deployments → Integrate → Install as AI agent plugin** and pick a target:

### Add to Claude Code

Writes the skill to `~/.claude/skills/{orgSlug}-{deploymentId}/` and registers the MCP server in `~/.claude.json` under the same name. Restart Claude Code to pick it up.

### Add to VS Code Chat

Writes the skill to `~/.claude/skills/{orgSlug}-{deploymentId}/` (VS Code Chat reads this path natively per the [Agent Skills spec](https://code.visualstudio.com/docs/copilot/customization/agent-skills)) and adds an entry to VS Code's user-level `mcp.json`. Reload the window to pick it up.

### Download Zip (⤓)

Saves the raw plugin zip to disk. Useful for:

- `claude --plugin-url file:///path/to/plugin.zip` — installs into Claude Code from the CLI
- Sharing with a teammate who isn't signed in to the org
- Inspecting the generated content before installing

## Endpoint

The same bundle is also served directly from the MCP host, with two URL aliases:

```
GET https://mcp.legalese.cloud/{orgSlug}/{deploymentId}/.plugin
```

Both return identical bytes; pick whichever name reads better in your tooling. Authenticated with the same WorkOS session or API key as the MCP endpoint itself.

## Authentication

Authenticate with your Legalese Cloud session or an API key (`Authorization: Bearer sk_...`) with the `l4:rules` permission.

## Notes

- A deployment **must have an "Intended use" description** before it can be installed as a plugin. Without one the endpoint returns `422` and the install flow surfaces a clear error — a plugin with no description is one the agent will never trigger.
- The skill folder is shared across Claude Code and VS Code Chat (both read `~/.claude/skills/`), so installing for one target after the other doesn't duplicate files; only the MCP server registration changes.
- Re-installing refreshes the skill in place — no need to uninstall first.
- Per-deployment names (`{orgSlug}-{deploymentId}`) mean multiple deployments of the same org coexist as separate skills + MCP servers, each scoped to its own ruleset.
