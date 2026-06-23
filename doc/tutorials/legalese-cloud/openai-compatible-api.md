# OpenAI- and Anthropic-Compatible AI APIs

Talk to a deployed set of L4 rules using either an OpenAI-compatible or an Anthropic-compatible client. Legalese Cloud serves each deployment behind both wire formats, backed by the same `legalese-comply-4` pipeline — a model pipeline tuned for fast, reliable rule evaluation. Use whichever SDK your stack already speaks.

**Prerequisites:** A deployment on [Legalese Cloud](https://legalese.cloud) ([Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md))

---

## When to Use This

Use the AI chat APIs when you want **conversational** rule evaluation — the caller asks a question in natural language and the model decides which exported rules to run, gathers the inputs, and explains the outcome. It is the same surface the **"Use in chat"** button in the VS Code Deployment tab connects to.

Reach for the [MCP server](./mcp-server.md) instead when you want a tool-calling client to drive the rules, or the [OpenAPI spec](./openapi-spec.md) when you want to call individual rules deterministically as plain REST.

## Endpoint

```
https://ai.legalese.cloud/{orgSlug}/{deploymentId}/v1
```

`{orgSlug}` is your Legalese Cloud organization; `{deploymentId}` is the deployment's name. The VS Code **Integrate** dialog pre-fills both for you. Append the path that matches your client's wire format:

| Client wire format | Append              |
| ------------------ | ------------------- |
| OpenAI             | `/chat/completions` |
| Anthropic Messages | `/messages`         |

Both paths front the same pipeline and the same conversation store — pick whichever SDK is more convenient. The two surfaces share auth, daily token quota, and tool-loop behaviour; only the request/response shape on the wire differs.

## Authentication

Either of these headers carries your credential — the value (sealed Legalese Cloud session or `sk_*` API key) is the same; only the header name differs so each SDK works out of the box:

- `Authorization: Bearer {token}` — the OpenAI-SDK default, and what the VS Code extension uses with your Legalese Cloud session.
- `x-api-key: {token}` — the Anthropic-SDK default.

For credentials:

- **Legalese Cloud session** — sign in from the VS Code sidebar; the extension uses your session automatically.
- **API key** (for API use only) — create a key in the [Legalese Cloud console](https://legalese.cloud) with the `ai:chat`, `l4:rules` and `l4:evaluate` permissions to use this feature.

### Legalese Cloud Permissions

A key (or session) used against this endpoint needs three scopes — each gates a different step of a chat turn:

| Permission    | Enables                                                                                                                                                                          |
| ------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ai:chat`     | Open a conversation against the deployment's `legalese-comply-4` pipeline — the hosted chat-completions surface itself. Without it the request is rejected before any rule runs. |
| `l4:rules`    | Let the model **discover** the deployment's exported rules: their names, descriptions and input shapes, so it can pick the right one for the question.                           |
| `l4:evaluate` | Let the model **run** a chosen rule with the inputs it gathered and read back the typed decision it then explains to the user.                                                   |

Grant only what the integration needs. A key with `ai:chat` but missing `l4:evaluate`, for example, can hold a conversation but every rule call will fail — the model will say it cannot compute the answer.

## Use It

### OpenAI clients

#### curl

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

#### OpenAI SDK (Python)

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

#### OpenAI SDK (TypeScript)

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

### Anthropic clients

#### curl

```bash
curl https://ai.legalese.cloud/{orgSlug}/{deploymentId}/v1/messages \
  -H "x-api-key: sk_..." \
  -H "anthropic-version: 2023-06-01" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "legalese-comply-4",
    "max_tokens": 1024,
    "messages": [
      { "role": "user", "content": "Is a tenant who is 40 days late on rent in breach?" }
    ]
  }'
```

#### Anthropic SDK (Python)

```python
import anthropic

# Anthropic's SDK appends "/v1/messages" itself, so base_url stops at the deployment root.
client = anthropic.Anthropic(
    base_url="https://ai.legalese.cloud/{orgSlug}/{deploymentId}",
    api_key="sk_...",
)

resp = client.messages.create(
    model="legalese-comply-4",
    max_tokens=1024,
    messages=[{"role": "user", "content": "Is a tenant 40 days late in breach?"}],
)
print(resp.content[0].text)
```

#### Anthropic SDK (TypeScript)

```ts
import Anthropic from "@anthropic-ai/sdk";

// baseURL stops at the deployment root; the SDK appends "/v1/messages".
const client = new Anthropic({
  baseURL: "https://ai.legalese.cloud/{orgSlug}/{deploymentId}",
  apiKey: "sk_...",
});

const resp = await client.messages.create({
  model: "legalese-comply-4",
  max_tokens: 1024,
  messages: [{ role: "user", content: "Is a tenant 40 days late in breach?" }],
});
console.log(resp.content[0].type === "text" ? resp.content[0].text : "");
```

Streaming (`stream: true`), extended thinking, and multi-turn conversations work exactly as they do against `api.anthropic.com`. Reasoning surfaces as native `thinking` content blocks (no proxy-specific event extension needed on this surface).

## Notes

- The deployment endpoint only knows the rules you exported in that deployment — it does not have access to your editor, workspace, or other deployments.
- The model name is always `legalese-comply-4` on this scoped endpoint; both surfaces reject raw upstream ids (e.g. `claude-opus-4-7`) with `400 model_not_found` — pipelines, not upstream models, are the product of record.
- Self-hosted `jl4-service` does **not** expose these AI endpoints. Use the [OpenAPI spec](./openapi-spec.md) or [MCP server](./mcp-server.md) there instead.
