// The global "skills marketplace" gateway plugin, templated locally so the
// sidebar can offer it as a downloadable zip (next to "Install Skills
// Marketplace"). It mirrors the artifact the auth-proxy serves at
// `skills.legalese.cloud` (see jl4-auth-proxy `buildGatewayPluginFiles` /
// `buildGatewayMarketplace`): the org-agnostic discovery skill plus the
// account-wide rules MCP (org resolved from sign-in — no token baked in).
//
// The zip is self-contained: it carries `.claude-plugin/marketplace.json`
// so an extracted copy works as a local marketplace
// (`/plugin marketplace add <dir>`), the plugin manifest, the discovery
// `.mcp.json`, and the priming `SKILL.md`.

/** Account-wide discovery MCP (org resolved from auth, no token baked in). */
const DISCOVERY_MCP_HOST = 'mcp.legalese.cloud'

const MARKETPLACE_NAME = 'legalese-cloud'
const PLUGIN_NAME = 'rules'
const SKILL_NAME = 'legalese-rules'

const PLUGIN_DESCRIPTION =
  "Find and run your account's deployed legal & policy decision rules on " +
  'Legalese Cloud. Discovers the right rule on demand — no need to preload ' +
  'every deployment as a tool.'

/** Single top-level folder so extracting the zip yields one tidy directory. */
const ROOT = 'legalese-cloud-rules'

// The L4 authoring plugin lives in the public l4-ide repo — advertised here
// alongside the rules gateway so one marketplace add surfaces both surfaces:
// *write* L4, and *run* your deployed rules.
const L4_AUTHORING_PLUGIN_NAME = 'l4-computational-law'
const L4_AUTHORING_PLUGIN_DESCRIPTION =
  'L4 programming language for computational law — encode contracts, ' +
  'regulations, and policies as executable, testable, verifiable programs.'

function marketplaceJson(): string {
  return (
    JSON.stringify(
      {
        name: MARKETPLACE_NAME,
        owner: { name: 'Legalese Cloud' },
        plugins: [
          {
            name: PLUGIN_NAME,
            description: PLUGIN_DESCRIPTION,
            // Relative source: the plugin lives at the repo/zip root.
            source: '.',
          },
          {
            name: L4_AUTHORING_PLUGIN_NAME,
            description: L4_AUTHORING_PLUGIN_DESCRIPTION,
            source: { source: 'github', repo: 'legalese/l4-ide', path: '.' },
          },
        ],
      },
      null,
      2
    ) + '\n'
  )
}

function pluginJson(): string {
  return (
    JSON.stringify(
      { name: PLUGIN_NAME, version: '1.0.0', description: PLUGIN_DESCRIPTION },
      null,
      2
    ) + '\n'
  )
}

/** `.mcp.json` registering the account-wide discovery MCP (org from auth, no
 *  token — the client runs the OAuth flow on first use). */
function mcpJson(): string {
  return (
    JSON.stringify(
      {
        mcpServers: {
          [SKILL_NAME]: { type: 'http', url: `https://${DISCOVERY_MCP_HOST}` },
        },
      },
      null,
      2
    ) + '\n'
  )
}

function skillMd(): string {
  return `---
name: ${SKILL_NAME}
description: >-
  Find and run an account's deployed legal & policy decision rules on Legalese
  Cloud. Use when a question needs a formal, authoritative determination —
  eligibility, entitlements, deadlines, premiums, compliance, or a contract /
  regulation outcome — that should be computed by a deployed ruleset rather than
  reasoned out by hand.
---

# Legalese Cloud rules

This account's deployed decision rules are authoritative for what they cover.
When a question turns on one — eligibility, entitlements, deadlines, compliance,
a contract or regulation outcome — find the matching rule and run it; don't
reason the determination yourself, and if none matches, say so.

This plugin registers the **rules MCP** (\`https://${DISCOVERY_MCP_HOST}\`; org
resolved from your sign-in). Do everything through that one server — no
per-deployment or per-org endpoint needed:

- \`search_rules\` — find the rule(s) a question needs (space-separated keywords,
  matched as OR).
- \`get_schema\` — get a rule's input/output schema before you call it.
- \`evaluate\` — run the rule and answer from the authoritative result.

Cite the rule you ran.
`
}

function readmeMd(): string {
  return `# Legalese Cloud rules — agent skills marketplace

Install your Legalese Cloud account's deployed decision rules as an AI agent
skill. The agent **discovers** the rule a question needs and **runs** it (behind
your permission barrier) instead of reasoning the determination itself.

## Install (Claude Code / claude.ai)

\`\`\`
/plugin marketplace add legalese/cloud-rules
/plugin install ${PLUGIN_NAME}@${MARKETPLACE_NAME}
\`\`\`

The plugin registers the account-wide rules MCP server
(\`https://${DISCOVERY_MCP_HOST}\`, org resolved from your sign-in — no token is
baked in; OAuth runs on first use) plus a skill that primes the model on _when_
to use it.

The same marketplace also advertises the **L4 authoring plugin** (from
\`legalese/l4-ide\`) — expert guidance for writing L4:

\`\`\`
/plugin install ${L4_AUTHORING_PLUGIN_NAME}@${MARKETPLACE_NAME}
\`\`\`

## Other harnesses

Use the L4 VS Code extension's **Install Skills Marketplace** to add the rules to
Cursor, Windsurf, Cline, VS Code (Copilot), or Claude Desktop in one click, or
point any MCP client at \`https://${DISCOVERY_MCP_HOST}\`.
`
}

/** One file in the gateway plugin bundle. */
export interface GatewayFile {
  name: string
  content: Uint8Array
}

/**
 * The gateway plugin as a list of zip entries under a single root folder.
 * Suitable for the extension's `createZip`.
 */
export function buildGatewayPluginFiles(): GatewayFile[] {
  const enc = new TextEncoder()
  const file = (rel: string, text: string): GatewayFile => ({
    name: `${ROOT}/${rel}`,
    content: enc.encode(text),
  })
  return [
    file('.claude-plugin/marketplace.json', marketplaceJson()),
    file('.claude-plugin/plugin.json', pluginJson()),
    file('.mcp.json', mcpJson()),
    file(`skills/${SKILL_NAME}/SKILL.md`, skillMd()),
    file('README.md', readmeMd()),
  ]
}

/** Default filename for the saved zip. */
export const GATEWAY_ZIP_NAME = `${ROOT}.zip`
