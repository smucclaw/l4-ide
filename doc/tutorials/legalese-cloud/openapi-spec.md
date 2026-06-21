# RESTful OpenAPI Specification

Every deployment publishes an OpenAPI (Swagger) JSON document so third-party systems can generate clients and call your rules as a plain REST API.

**Prerequisites:** A deployment ([Exporting Rules for Deployment](../deploying-rules/exporting-rules-for-deployment.md))
**Time:** 10 minutes
**Goal:** Fetch the OpenAPI spec and call a rule deterministically over REST

---

## When to Use This

Use the OpenAPI spec for **deterministic, server-to-server** integration: each exported rule is a typed REST operation with a documented request and response schema. No model is involved — the same inputs always produce the same decision, which is what you want for backend workflows, batch jobs, and audited systems.

For conversational or agent-driven use, see the [OpenAI- and Anthropic-compatible AI APIs](./openai-compatible-api.md) or [MCP server](./mcp-server.md).

## Endpoint

**Legalese Cloud:**

```
https://api.legalese.cloud/{orgSlug}/{deploymentId}/openapi.json
```

**Self-hosted jl4-service:**

```
http://{serviceUrl}/{deploymentId}/openapi.json
```

The VS Code **Integrate** dialog pre-fills the correct URL for your connection.

## Authentication

- **Legalese Cloud:** bearer token — your signed-in session or an API key (`Authorization: Bearer sk_...`) from the [console](https://legalese.cloud). Uses the `l4:rules`, `l4:evaluate` and `l4:read` permissions.
- **Self-hosted:** whatever auth your `jl4-service` is configured with.

### Legalese Cloud Permissions

| Permission    | Enables                                                                                                                      |
| ------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `l4:rules`    | Enumerate the deployment's rules — which REST operations (paths) the spec exposes.                                           |
| `l4:read`     | Fetch `openapi.json` itself: the request/response **schemas** for every operation. Code generators and Swagger UI need this. |
| `l4:evaluate` | Call an operation — POST inputs to a rule and receive its computed decision.                                                 |

A common split: a build-time key with `l4:rules` + `l4:read` to (re)generate clients from the spec, and a separate runtime key that also has `l4:evaluate` for the service that actually calls the rules.

## Use It

### Inspect the spec

```bash
curl https://{orgSlug}.legalese.cloud/{deploymentId}/openapi.json \
  -H "Authorization: Bearer sk_..."
```

The document lists one path per exported rule, with the `GIVEN` parameters as the typed request body and the rule's decision as the response.

### Generate a client

Feed the URL to any OpenAPI tool:

```bash
# openapi-generator
openapi-generator-cli generate \
  -i https://api.legalese.cloud/{orgSlug}/{deploymentId}/openapi.json \
  -g typescript-fetch -o ./generated

# or import the URL into Postman / Insomnia / Swagger UI
```

### Call a rule

```bash
curl https://api.legalese.cloud/{orgSlug}/{deploymentId}/<rule-operation> \
  -H "Authorization: Bearer sk_..." \
  -H "Content-Type: application/json" \
  -d '{ "applicant": { "age": 40, "risk score": 0.8 } }'
```

The exact path and request shape for each rule come straight from the spec.

## Notes

- The spec regenerates on every redeploy, so generated clients stay in sync — re-run codegen after a schema-changing deploy.
- Responses are the rule's typed decision, suitable for storing as an auditable record.
- A self-hosted `jl4-service` serves the spec at `http://{serviceUrl}/{deploymentId}/openapi.json`.
