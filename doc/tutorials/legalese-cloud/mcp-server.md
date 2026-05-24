# MCP Server

Expose a deployment's rules as [Model Context Protocol](https://modelcontextprotocol.io) tools so any MCP-enabled AI client can call them.

**Audience:** Developers wiring deployed rules into Claude, an IDE agent, or a custom MCP host
**Prerequisites:** A deployment ([Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md))
**Time:** 10 minutes
**Goal:** Connect an AI chat/MCP client to your deployment

---

## When to Use This

Use the MCP server when an AI client (Claude Desktop, Claude Code, an IDE agent, or your own MCP host) should be able to **call your exported rules as tools** during a conversation. Each exported function becomes one MCP tool with a typed input schema derived from its `GIVEN` parameters.

Prefer the [OpenAPI spec](./openapi-spec.md) for deterministic server-to-server calls, or the [OpenAI- and Anthropic-compatible AI APIs](./openai-compatible-api.md) for a hosted chat experience.

## Endpoint

**Legalese Cloud:**

```
https://mcp.legalese.cloud/{orgSlug}/{deploymentId}
```

**Self-hosted jl4-service:**

```
{serviceUrl}/{deploymentId}/.mcp
```

`{serviceUrl}` is the jl4-service URL configured in the extension settings. The VS Code **Integrate** dialog pre-fills the correct form for whichever connection you are on.

## Authentication

- **Legalese Cloud:** authenticate with your Legalese Cloud session via OAuth, or use an API key (`Authorization: Bearer sk_...`) created in the [console](https://legalese.cloud). Uses the `l4:rules`, `l4:evaluate` and `l4:read` permissions.
- **Self-hosted:** whatever auth your `jl4-service` is configured with (often none for a local instance).

### Permissions

An MCP client does two things — list the tools, then call them — so the key needs the matching scopes:

| Permission    | Enables                                                                                                                                  |
| ------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| `l4:rules`    | The MCP `tools/list` response: one tool per exported rule, with its name and description. Without it the client sees an empty tool list. |
| `l4:read`     | The full input/output **schema** for each tool, so the client can validate arguments and the model knows exactly what each rule expects. |
| `l4:evaluate` | The MCP `tools/call` operation: actually running a rule with arguments and returning its typed decision.                                 |

`l4:rules` + `l4:read` alone make a read-only catalog (tools are visible but uncallable); add `l4:evaluate` for a client that can also execute rules.

## Use It

### Claude Code

```bash
claude mcp add --transport http legalese-rules \
  https://mcp.legalese.cloud/{orgSlug}/{deploymentId} \
  --header "Authorization: Bearer sk_..."
```

### Claude Desktop / generic MCP client

Add an HTTP MCP server entry pointing at the endpoint above and supply the `Authorization` header. The client will list one tool per exported rule; the model invokes them with structured arguments and receives the rule's typed decision back.

### Verifying

Once connected, the client's tool list shows your exported rule names. Ask the model a question that needs one of the rules — it will call the matching tool and answer from the rule's output rather than guessing.

## Notes

- Tools and their input schemas update automatically when you redeploy.
- The server only exposes the rules in that one deployment.
- A self-hosted `jl4-service` serves MCP at `{serviceUrl}/{deploymentId}/.mcp`; there is no separate `mcp.` host.
