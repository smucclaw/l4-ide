# How to Query LLMs from L4

This guide shows how to accomplish specific tasks using the LLM library.

## How to Query with Dynamic Content

Embed L4 variables into your prompts:

```l4
DECIDE legalClause IS
  "The Buyer shall pay within 30 days."

DECIDE analyzeClause IS
  CONSIDER ENV "OPENROUTER_API_KEY"
    WHEN JUST apiKey THEN
      POST
        "https://openrouter.ai/api/v1/chat/completions"
        (CONCAT
          "Content-Type: application/json\n",
          "Authorization: Bearer ", apiKey)
        (CONCAT
          "{\"model\": \"anthropic/claude-3.5-sonnet\", \"messages\": [{\"role\": \"user\", \"content\": \"Analyze: ",
          legalClause,
          "\"}], \"max_tokens\": 500}")
    WHEN NOTHING THEN
      "ERROR: API key not set"
```

## How to Use System Prompts

Control LLM behavior with system messages:

```l4
DECIDE structuredQuery IS
  CONSIDER ENV "OPENROUTER_API_KEY"
    WHEN JUST apiKey THEN
      POST
        "https://openrouter.ai/api/v1/chat/completions"
        (CONCAT
          "Content-Type: application/json\n",
          "Authorization: Bearer ", apiKey)
        "{\"model\": \"gpt-4\", \"messages\": [{\"role\": \"system\", \"content\": \"You are a legal expert. Always respond in JSON format.\"}, {\"role\": \"user\", \"content\": \"Define consideration.\"}], \"max_tokens\": 300}"
    WHEN NOTHING THEN
      "ERROR: API key not set"
```

## How to Implement Fallback Strategies

Try one provider, fall back to another if unavailable:

```l4
DECIDE queryWithFallback IS
  CONSIDER ENV "OPENROUTER_API_KEY"
    WHEN JUST routerKey THEN
      POST
        "https://openrouter.ai/api/v1/chat/completions"
        (CONCAT "Content-Type: application/json\nAuthorization: Bearer ", routerKey)
        "{\"model\": \"gpt-3.5-turbo\", \"messages\": [{\"role\": \"user\", \"content\": \"Test\"}], \"max_tokens\": 50}"
    WHEN NOTHING THEN
      CONSIDER ENV "OPENAI_API_KEY"
        WHEN JUST openaiKey THEN
          POST
            "https://api.openai.com/v1/chat/completions"
            (CONCAT "Content-Type: application/json\nAuthorization: Bearer ", openaiKey)
            "{\"model\": \"gpt-3.5-turbo\", \"messages\": [{\"role\": \"user\", \"content\": \"Test\"}], \"max_tokens\": 50}"
        WHEN NOTHING THEN
          "ERROR: No API keys configured"
```

## Legal Domain Use Cases

### Contract Analysis

Analyze a contract clause for ambiguities:

```l4
DECIDE contractAnalysis IS
  POST openrouterUrl headers
    "{\"model\": \"anthropic/claude-3-opus\", \"messages\": [{\"role\": \"system\", \"content\": \"You are a contract law expert. Identify ambiguities and potential issues.\"}, {\"role\": \"user\", \"content\": \"Analyze: The party shall deliver the goods in a timely manner.\"}], \"max_tokens\": 1000}"
```

### Legal Definition Lookup

Get precise definitions of legal terms:

```l4
DECIDE defineTerm IS
  POST openrouterUrl headers
    "{\"model\": \"gpt-4\", \"messages\": [{\"role\": \"system\", \"content\": \"You are a legal dictionary. Provide concise, accurate definitions.\"}, {\"role\": \"user\", \"content\": \"Define: consideration\"}], \"max_tokens\": 200}"
```

### Compliance Checking

Check if a clause complies with regulations:

```l4
DECIDE checkCompliance IS
  POST openrouterUrl headers
    "{\"model\": \"gpt-4\", \"messages\": [{\"role\": \"system\", \"content\": \"You are a regulatory compliance expert.\"}, {\"role\": \"user\", \"content\": \"Does this clause comply with GDPR? [clause text]\"}], \"max_tokens\": 500}"
```

## How to Make Judgment Calls with LLMs

Use LLMs to evaluate predicates that require human-like judgment:

See `jl4/examples/advanced/llm-judgment-calls.l4` for the full pattern.

**Pattern:**
1. Define stub predicate (returns FALSE placeholder)
2. Create prompt template function with decision boundaries
3. Query LLM with template
4. Parse YES/NO response
5. Use as boolean in formal logic

**Example domains:**
- Temporal boundaries: "Has the day begun based on illumination?"
- Tone analysis: "Is this language professional?"
- Document classification: "Is this a legal contract?"

## How to Extract Rules from Legislative Text

Use LLMs to assist with formalizing legal text:

See `jl4/examples/advanced/legislative-ingestion.l4` for the full workflow.

**Workflow:**
1. **LLM extraction**: Ask LLM to extract structured rules from natural language
2. **Human review**: Review LLM output for accuracy
3. **Formalization**: Write precise L4 rules based on extraction
4. **Verification**: Test rules to find edge cases
5. **Explanation**: Use LLM to explain findings in plain language

This workflow has found critical bugs in production systems (race conditions, payout formula errors).

## Contributing

When adding new LLM querying examples:

1. Test with at least one provider (OpenRouter recommended)
2. Include error handling for missing API keys
3. Document the expected response format
4. Keep `max_tokens` reasonable to avoid high costs
5. Add comments explaining the use case
