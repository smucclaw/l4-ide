# WebMCP Embed Script

Drop a single `<script>` tag into your website so visitors' AI assistants can evaluate your deployed rules directly in the browser, via [WebMCP](https://modelcontextprotocol.io).

**Prerequisites:** A deployment ([Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md)) and the ability to edit your site's HTML
**Time:** 5 minutes
**Goal:** Embed the WebMCP loader and expose your rules to on-page assistants

---

## When to Use This

Use the WebMCP embed when you want **your website itself** to offer your rules as tools — e.g. an eligibility checker or a contract clause evaluator that a visitor's browser-side AI agent can call without any backend integration on their part. The script registers your deployment's rules as WebMCP tools scoped to the page.

For server-side or agent integrations, use the [MCP server](./mcp-server.md) or [OpenAPI spec](./openapi-spec.md) instead.

## The Script

**Legalese Cloud:**

```html
<script
  src="https://api.legalese.cloud/.webmcp/embed.js"
  data-org="{orgSlug}"
  data-scope="*"
  data-tools="auto"
  data-api-key="sk_..."
></script>
```

**Self-hosted jl4-service:**

```html
<script
  src="http://{serviceUrl}/.webmcp/embed.js"
  data-scope="*"
  data-tools="auto"
  data-api-key="sk_..."
></script>
```

The VS Code **Integrate** dialog pre-fills the correct `src` for your connection.

### Attributes

| Attribute      | Meaning                                                               |
| -------------- | --------------------------------------------------------------------- |
| `src`          | The WebMCP loader served by Legalese Cloud / your jl4-service         |
| `data-org`     | Your organization slug on Legalese Cloud. Unnecessary for jl4-service |
| `data-scope`   | Which pages the tools are offered on (`*` = the whole site)           |
| `data-tools`   | `auto` registers every exported rule; or a comma-separated allow-list |
| `data-api-key` | A Legalese Cloud API key authorizing rule evaluation                  |

## Getting an API Key

Replace `sk_...` with a real key:

1. Open the [Legalese Cloud console](https://legalese.cloud).
2. Create an API key scoped to the deployment (or organization) you are embedding and with `l4:rules` and `l4:evaluate` permissions.
3. Paste your organization slug into `data-org` and your API key into `data-api-key`.

### Legalese Cloud Permissions

| Permission    | Enables                                                                                             |
| ------------- | --------------------------------------------------------------------------------------------------- |
| `l4:rules`    | The embed script registers each exported rule as a WebMCP tool the visitor's on-page agent can see. |
| `l4:evaluate` | The agent can actually run a rule and get its decision back.                                        |

Deliberately **omit** `l4:read` here: the browser only needs to discover and call the rules, not download their full source/schema. And because the key ships in client-side HTML, scope it to **rule evaluation only**, restrict it to this deployment, and rotate it if exposed. Treat it like a publishable key, not a secret.

## Verify

Load a page that includes the script and open a WebMCP-aware assistant. Your exported rules appear as available tools; invoking one runs the deployed rule and returns its typed decision.

## Notes

- `data-tools="auto"` keeps the tool list in sync with redeploys.
- A self-hosted `jl4-service` serves the loader at `http://{serviceUrl}/.webmcp/embed.js`.
