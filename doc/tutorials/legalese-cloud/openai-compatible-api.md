# OpenAI-Compatible AI API

Talk to a deployed set of L4 rules using any OpenAI-compatible client. Legalese Cloud serves each deployment as a chat-completions endpoint backed by `legalese-comply-4`, a model pipeline tuned for fast, reliable rule evaluation.

**Audience:** Developers integrating deployed rules into an app or agent
**Prerequisites:** A deployment on [Legalese Cloud](https://legalese.cloud) ([Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md))
**Time:** 10 minutes
**Goal:** Call your deployment's rules from the OpenAI SDK / curl

---

## When to Use This

Use the OpenAI-compatible API when you want **conversational** rule evaluation — the caller asks a question in natural language and the model decides which exported rules to run, gathers the inputs, and explains the outcome. It is the same surface the **"Use in chat"** button in the VS Code Deployment tab connects to.

Reach for the [MCP server](./mcp-server.md) instead when you want a tool-calling client to drive the rules, or the [OpenAPI spec](./openapi-spec.md) when you want to call individual rules deterministically as plain REST.

## Endpoint

```
https://ai.legalese.cloud/{orgSlug}/{deploymentId}/v1
```

`{orgSlug}` is your Legalese Cloud organization; `{deploymentId}` is the deployment's name. The VS Code **Integrate** dialog pre-fills both for you. Append the standard OpenAI path — `/chat/completions` — when calling it directly.

## Authentication

Send a bearer token in the `Authorization` header. Either:

- **Legalese Cloud session** — sign in from the VS Code sidebar; the extension uses your session automatically.
- **API key** (for API use only) — create a key in the [Legalese Cloud console](https://legalese.cloud) with the `ai:chat`, `l4:rules` and `l4:evaluate` permissions to use this feature.

### Permissions

A key (or session) used against this endpoint needs three scopes — each gates a different step of a chat turn:

| Permission    | Enables                                                                                                                                                                       |
| ------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ai:chat`     | Open a conversation against the deployment's `legalese-comply-4` pipeline — the hosted chat-completions surface itself. Without it the request is rejected before any rule runs. |
| `l4:rules`    | Let the model **discover** the deployment's exported rules: their names, descriptions and input shapes, so it can pick the right one for the question.                        |
| `l4:evaluate` | Let the model **run** a chosen rule with the inputs it gathered and read back the typed decision it then explains to the user.                                                |

Grant only what the integration needs. A key with `ai:chat` but missing `l4:evaluate`, for example, can hold a conversation but every rule call will fail — the model will say it cannot compute the answer.

## Use It

### curl

```bash
curl https://ai.legalese.cloud/{orgSlug}/{deploymentId}/v1/chat/completions \
  -H "Authorization: Bearer sk_..." \
  -H "Content-Type: application/json" \
  -d '{
    "model": "legalese-comply-4",
    "messages": [
      { "role": "user", "content": "Is a tenant who is 40 days late on rent in breach?" }
    ]
  }'
```

### OpenAI SDK (Python)

```python
from openai import OpenAI

client = OpenAI(
    base_url="https://ai.legalese.cloud/{orgSlug}/{deploymentId}/v1",
    api_key="sk_...",
)

resp = client.chat.completions.create(
    model="legalese-comply-4",
    messages=[{"role": "user", "content": "Is a tenant 40 days late in breach?"}],
)
print(resp.choices[0].message.content)
```

### OpenAI SDK (TypeScript)

```ts
import OpenAI from "openai";

const client = new OpenAI({
  baseURL: "https://ai.legalese.cloud/{orgSlug}/{deploymentId}/v1",
  apiKey: "sk_...",
});

const resp = await client.chat.completions.create({
  model: "legalese-comply-4",
  messages: [{ role: "user", content: "Is a tenant 40 days late in breach?" }],
});
console.log(resp.choices[0].message.content);
```

Streaming (`stream: true`) and multi-turn conversations work exactly as they do against `api.openai.com`.

## Notes

- The deployment endpoint only knows the rules you exported in that deployment — it does not have access to your editor, workspace, or other deployments.
- The model name is always `legalese-comply-4` for our internal comply model pipeline; other model names are ignored.
- Self-hosted `jl4-service` does **not** expose this AI endpoint. Use the [OpenAPI spec](./openapi-spec.md) or [MCP server](./mcp-server.md) there instead.
