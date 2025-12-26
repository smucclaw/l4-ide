# Querying LLMs from L4

This branch demonstrates how to query Large Language Models (LLMs) from L4 code using the language's built-in HTTP and JSON capabilities.

## Overview

L4 can interact with AI APIs using these built-in operators:

- **`POST`** - Make HTTP POST requests with custom headers and body
- **`FETCH`** - Make HTTP GET requests
- **`JSONENCODE`** - Convert L4 values to JSON strings
- **`JSONDECODE`** - Parse JSON strings to L4 values (returns `EITHER STRING value`)
- **`ENV`** - Read environment variables (returns `MAYBE STRING`)
- **`CONCAT`** - Concatenate strings for building request bodies

## Quick Start

### 1. Get an API Key

The easiest way to get started is with **OpenRouter**, which provides access to 500+ models through a single API:

- Sign up at https://openrouter.ai/
- Get your API key (starts with `sk-or-v1-...`)
- Set it as an environment variable:

```bash
export OPENROUTER_API_KEY="sk-or-v1-YOUR_KEY_HERE"
```

### 2. Run the Example

```bash
# View the example
cat jl4/examples/ok/ai-simple.l4

# Run it (uncomment the #EVAL line first)
cabal run jl4-cli -- jl4/examples/ok/ai-simple.l4
```

## Simple Example

```l4
-- Query OpenRouter to ask a question to GPT-3.5 Turbo
DECIDE simpleAIQuery IS
  CONSIDER ENV "OPENROUTER_API_KEY"
    WHEN JUST apiKey THEN
      POST
        "https://openrouter.ai/api/v1/chat/completions"
        (CONCAT
          "Content-Type: application/json\n",
          "Authorization: Bearer ", apiKey, "\n",
          "HTTP-Referer: https://l4-lang.org")
        "{\"model\": \"openai/gpt-3.5-turbo\", \"messages\": [{\"role\": \"user\", \"content\": \"What is 2+2?\"}], \"max_tokens\": 50}"
    WHEN NOTHING THEN
      "ERROR: OPENROUTER_API_KEY not set"
```

## LLM Provider Options

### 1. OpenRouter (Recommended)

**Advantages:**

- Single API for 500+ models (OpenAI, Anthropic, Meta, Google, etc.)
- OpenAI-compatible API format
- Automatic failover and load balancing
- Free tier available
- No vendor lock-in

**Endpoint:** `https://openrouter.ai/api/v1/chat/completions`

**Popular models:**

- `openai/gpt-4` - Most capable OpenAI model
- `openai/gpt-3.5-turbo` - Fast and cheap
- `anthropic/claude-3.5-sonnet` - Excellent for complex reasoning
- `anthropic/claude-3-opus` - Most capable Claude model
- `meta-llama/llama-3.1-70b-instruct` - Open source
- `meta-llama/llama-3.1-70b-instruct:free` - Free tier
- `google/gemini-pro` - Google's model

### 2. OpenAI Direct

Query OpenAI models directly without a gateway.

**Endpoint:** `https://api.openai.com/v1/chat/completions`  
**API Key:** Set `OPENAI_API_KEY`  
**Models:** `gpt-4`, `gpt-4-turbo`, `gpt-3.5-turbo`

**Headers:**

```
Content-Type: application/json
Authorization: Bearer <your-key>
```

### 3. Anthropic Claude Direct

Query Claude models directly with Anthropic's native API (different format).

**Endpoint:** `https://api.anthropic.com/v1/messages`  
**API Key:** Set `ANTHROPIC_API_KEY`  
**Models:** `claude-3-5-sonnet-20241022`, `claude-3-opus-20240229`

**Headers:**

```
Content-Type: application/json
x-api-key: <your-key>
anthropic-version: 2023-06-01
```

**Request format (different from OpenAI):**

```json
{
  "model": "claude-3-5-sonnet-20241022",
  "messages": [{ "role": "user", "content": "Your question" }],
  "max_tokens": 1000
}
```

## API Request Format

### OpenAI-Compatible Format (OpenRouter, OpenAI)

**Request:**

```json
{
  "model": "openai/gpt-4",
  "messages": [
    { "role": "system", "content": "You are a helpful assistant." },
    { "role": "user", "content": "Your question here" }
  ],
  "max_tokens": 1000
}
```

**Response:**

```json
{
  "choices": [
    {
      "message": {
        "role": "assistant",
        "content": "The answer is..."
      }
    }
  ]
}
```

## Advanced Examples

### Dynamic Content

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

### System Prompts

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

### Parsing JSON Responses

The `llm.l4` library provides automatic response parsing for all supported providers:

```l4
IMPORT "jl4-core/libraries/llm.l4"

-- Method 1: Use the library's unified extraction helper
DECIDE rawResponse IS queryLLMWithDefaults "What is consideration in contract law?"
DECIDE extractedContent IS extractLLMResponse "openrouter" rawResponse

DECIDE finalAnswer IS
  CONSIDER extractedContent
    WHEN RIGHT text THEN text  -- Got the actual LLM response text
    WHEN LEFT error THEN CONCAT "Extraction error: " error

-- Method 2: Manual parsing for custom scenarios
DECIDE manualParse IS
  CONSIDER JSONDECODE rawResponse
    WHEN RIGHT jsonData THEN
      extractOpenAIContent jsonData  -- Returns EITHER STRING String
    WHEN LEFT errorMsg THEN
      LEFT (CONCAT "Parse error: " errorMsg)
```

**Supported response formats:**

- **OpenAI/OpenRouter**: `{"choices": [{"message": {"content": "..."}}]}`
- **Anthropic**: `{"content": [{"text": "..."}]}`

The library automatically handles the different response structures, extracting the actual text content and providing helpful error messages if parsing fails.

### Fallback Strategy

```l4
-- Try OpenRouter first, fall back to OpenAI
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

## Use Cases for Legal Domain

### 1. Contract Analysis

```l4
-- Analyze a contract clause for ambiguities
DECIDE contractAnalysis IS
  POST openrouterUrl headers
    "{\"model\": \"anthropic/claude-3-opus\", \"messages\": [{\"role\": \"system\", \"content\": \"You are a contract law expert. Identify ambiguities and potential issues.\"}, {\"role\": \"user\", \"content\": \"Analyze: The party shall deliver the goods in a timely manner.\"}], \"max_tokens\": 1000}"
```

### 2. Legal Definition Lookup

```l4
-- Get definition of a legal term
DECIDE defineTerm IS
  POST openrouterUrl headers
    "{\"model\": \"gpt-4\", \"messages\": [{\"role\": \"system\", \"content\": \"You are a legal dictionary. Provide concise, accurate definitions.\"}, {\"role\": \"user\", \"content\": \"Define: consideration\"}], \"max_tokens\": 200}"
```

### 3. Compliance Checking

```l4
-- Check if a clause complies with regulations
DECIDE checkCompliance IS
  POST openrouterUrl headers
    "{\"model\": \"gpt-4\", \"messages\": [{\"role\": \"system\", \"content\": \"You are a regulatory compliance expert.\"}, {\"role\": \"user\", \"content\": \"Does this clause comply with GDPR? [clause text]\"}], \"max_tokens\": 500}"
```

## Gateway Comparison

| Feature   | OpenRouter        | OpenAI Direct  | Anthropic Direct |
| --------- | ----------------- | -------------- | ---------------- |
| Setup     | One API key       | Per-provider   | Per-provider     |
| Models    | 500+              | OpenAI only    | Claude only      |
| Format    | OpenAI-compatible | OpenAI format  | Anthropic format |
| Latency   | +25-40ms          | Direct         | Direct           |
| Cost      | 5% markup OR BYOK | Standard rates | Standard rates   |
| Failover  | Automatic         | Manual         | Manual           |
| Free tier | Yes (limited)     | No             | No               |

## Best Practices

### 1. Security

- **Never hardcode API keys** - Always use `ENV` to read from environment
- **Use HTTPS only** - All endpoints must use `https://` (L4 enforces this)
- **Sanitize inputs** - Be careful with user-provided content in prompts
- **Limit token usage** - Set reasonable `max_tokens` to control costs

### 2. Error Handling

Always handle the `NOTHING` case for environment variables:

```l4
CONSIDER ENV "API_KEY"
  WHEN JUST key THEN makeRequest key
  WHEN NOTHING THEN "ERROR: API key not configured"
```

### 3. Cost Optimization

- Use cheaper models for simple queries (`gpt-3.5-turbo`)
- Use free models for development (`meta-llama/llama-3.1-70b-instruct:free`)
- Set appropriate `max_tokens` limits
- Cache responses when possible (future enhancement)

### 4. Model Selection

- **GPT-3.5 Turbo**: Fast, cheap, good for simple queries
- **GPT-4**: Most capable, expensive, for complex reasoning
- **Claude 3.5 Sonnet**: Excellent balance of capability and speed
- **Claude 3 Opus**: Most capable, for critical analysis
- **Llama 3.1 70B**: Open source, free tier available

## Recently Added Features

✅ **Response parsing** - Automatic content extraction from OpenAI, Anthropic, and OpenRouter responses with `extractLLMResponse`
✅ **Helper library** - Complete `llm.l4` library with multi-provider support, fallback strategies, and unified API

## Future Enhancements

Potential improvements to L4's AI integration:

1. **Streaming responses** - Support SSE/streaming for long outputs
2. **Retry logic** - Automatic retries with exponential backoff
3. **Rate limiting** - Built-in rate limit handling
4. **Response caching** - Avoid duplicate API calls
5. **Token counting** - Estimate costs before making requests
6. **Batch queries** - Process multiple prompts efficiently
7. **Fine-tuned models** - Support for custom models
8. **Function calling** - Structured tool use with GPT-4/Claude

## Resources

### Provider Documentation

- **OpenRouter**: https://openrouter.ai/
- **OpenAI API**: https://platform.openai.com/docs/api-reference
- **Anthropic API**: https://docs.anthropic.com/claude/reference

### L4 Examples and Tests

- **Simple examples**: `jl4/examples/ok/ai-simple.l4`
- **Multi-provider fallback**: `jl4/examples/ok/ai-multi-provider.l4`
- **Response parsing patterns**: `jl4/examples/ok/ai-with-parsing.l4`
- **Unit tests** (no API calls): `jl4/examples/ok/llm-parsing-tests.l4`

### Library Documentation

- **LLM Library**: `jl4-core/libraries/llm.l4`
- **Integration Spec**: `doc/todo/LLM-INTEGRATION-SPEC.md`

## Gateway Abstraction Projects

If you need more advanced features (caching, load balancing, observability), consider these LLM gateway projects:

- **OpenRouter** - Managed service, 500+ models, easiest to get started
- **Portkey** - Open source, enterprise features, <1ms latency
- **Bifrost** - Fastest open source gateway (11µs overhead)
- **Helicone** - Focused on observability and monitoring

For L4, **OpenRouter** is recommended for its simplicity and broad model support.

## Testing

To test the AI querying functionality:

```bash
# 1. Set your API key
export OPENROUTER_API_KEY="sk-or-v1-YOUR_KEY"

# 2. Run the example (after uncommenting #EVAL)
cabal run jl4-cli -- jl4/examples/ok/ai-simple.l4

# 3. Check the output - you should see a JSON response from the LLM
```

## Contributing

When adding new AI querying examples:

1. Test with at least one provider (OpenRouter recommended)
2. Include error handling for missing API keys
3. Document the expected response format
4. Keep `max_tokens` reasonable to avoid high costs
5. Add comments explaining the use case
