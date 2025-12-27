# LLM API Reference

Technical reference for L4's LLM integration capabilities.

## Built-in Operators

L4 provides these operators for LLM interaction:

- **`POST`** - Make HTTP POST requests with custom headers and body
- **`FETCH`** - Make HTTP GET requests
- **`JSONENCODE`** - Convert L4 values to JSON strings
- **`JSONDECODE`** - Parse JSON strings to L4 values (returns `EITHER STRING value`)
- **`ENV`** - Read environment variables (returns `MAYBE STRING`)
- **`CONCAT`** - Concatenate strings for building request bodies

## LLM Library Functions

Located in `jl4-core/libraries/llm.l4`

### Core Query Functions

**`queryLLMWithDefaults :: String -> String`**

- Queries LLM with automatic provider fallback
- Tries: OpenRouter → OpenAI → Anthropic
- Uses sensible defaults (1000 max tokens, best available model)

**`queryLLM :: String -> String -> Int -> String`**

- Parameters: model, prompt, maxTokens
- More control than `queryLLMWithDefaults`

### Provider-Specific Functions

**`callAnthropic :: String -> String -> String -> Int -> String`**

- Parameters: apiKey, model, prompt, maxTokens
- Endpoint: `https://api.anthropic.com/v1/messages`

**`callOpenAI :: String -> String -> String -> Int -> String`**

- Parameters: apiKey, model, prompt, maxTokens
- Endpoint: `https://api.openai.com/v1/chat/completions`

**`callOpenRouter :: String -> String -> String -> Int -> String`**

- Parameters: apiKey, model, prompt, maxTokens
- Endpoint: `https://openrouter.ai/api/v1/chat/completions`

### Convenience Wrappers

**`callClaude :: String -> String -> String`**

- Calls Anthropic's Claude Sonnet with 1000 max tokens
- Parameters: apiKey, prompt

**`callGPT4 :: String -> String -> String`**

- Calls OpenAI GPT-4 via OpenRouter with 1000 max tokens
- Parameters: apiKey, prompt

### Model Constants

For OpenRouter, these model identifiers are predefined:

- `claudeOpusModel` = `"anthropic/claude-3-opus"`
- `claudeSonnetModel` = `"anthropic/claude-3.5-sonnet"`
- `gpt4Model` = `"openai/gpt-4"`
- `gpt4TurboModel` = `"openai/gpt-4-turbo"`
- `gpt35Model` = `"openai/gpt-3.5-turbo"`
- `llama3Model` = `"meta-llama/llama-3.1-70b-instruct"`
- `llama3FreeModel` = `"meta-llama/llama-3.1-70b-instruct:free"`
- `geminiProModel` = `"google/gemini-pro"`

## Provider Comparison

| Feature   | OpenRouter        | OpenAI Direct  | Anthropic Direct |
| --------- | ----------------- | -------------- | ---------------- |
| Setup     | One API key       | Per-provider   | Per-provider     |
| Models    | 500+              | OpenAI only    | Claude only      |
| Format    | OpenAI-compatible | OpenAI format  | Anthropic format |
| Latency   | +25-40ms          | Direct         | Direct           |
| Cost      | 5% markup OR BYOK | Standard rates | Standard rates   |
| Failover  | Automatic         | Manual         | Manual           |
| Free tier | Yes (limited)     | No             | No               |

## API Request Formats

### OpenAI-Compatible (OpenRouter, OpenAI)

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

### Anthropic Format

**Endpoint:** `https://api.anthropic.com/v1/messages`

**Headers:**

```
Content-Type: application/json
x-api-key: <your-key>
anthropic-version: 2023-06-01
```

**Request:**

```json
{
  "model": "claude-3-5-sonnet-20241022",
  "messages": [{ "role": "user", "content": "Your question" }],
  "max_tokens": 1000
}
```

**Response:**

```json
{
  "content": [{ "text": "The answer is..." }],
  "stop_reason": "end_turn"
}
```

## Model Selection Guide

- **GPT-3.5 Turbo**: Fast, cheap, good for simple queries
- **GPT-4**: Most capable OpenAI model, expensive, for complex reasoning
- **Claude 3.5 Sonnet**: Excellent balance of capability and speed
- **Claude 3 Opus**: Most capable Claude model, for critical analysis
- **Llama 3.1 70B**: Open source, free tier available

## Best Practices

### Security

- **Never hardcode API keys** - Always use `ENV` to read from environment
- **Use HTTPS only** - All endpoints must use `https://` (L4 enforces this)
- **Sanitize inputs** - Be careful with user-provided content in prompts
- **Limit token usage** - Set reasonable `max_tokens` to control costs

### Error Handling

Always handle the `NOTHING` case for environment variables:

```l4
CONSIDER ENV "API_KEY"
  WHEN JUST key THEN makeRequest key
  WHEN NOTHING THEN "ERROR: API key not configured"
```

### Cost Optimization

- Use cheaper models for simple queries (`gpt-3.5-turbo`)
- Use free models for development (`meta-llama/llama-3.1-70b-instruct:free`)
- Set appropriate `max_tokens` limits
- Cache responses when possible

### Indented LIST Syntax

L4 supports indented list literals by aligning elements to the right of the `LIST` keyword:

**✅ Correct - Elements aligned to right of LIST:**

```l4
LET attempts BE
      LIST tryOpenRouter prompt
           tryOpenAI prompt
           tryAnthropic prompt
IN fromMaybe "ERROR" (firstJust attempts)
```

**❌ Wrong - Elements on separate lines below LIST:**

```l4
LET attempts BE
  LIST
    tryOpenRouter prompt
    tryOpenAI prompt
    tryAnthropic prompt
IN ...
```

**❌ Wrong - Using FOLLOWED BY for multi-element lists:**

```l4
LET attempts BE
  LIST tryOpenRouter prompt
  FOLLOWED BY tryOpenAI prompt
  FOLLOWED BY tryAnthropic prompt
IN ...
```

**Why this matters:** The indentation must align elements horizontally with `LIST` to form a valid list literal. Using `FOLLOWED BY` creates nested list types instead of a flat list.

## External Resources

### Provider Documentation

- **OpenRouter**: https://openrouter.ai/
- **OpenAI API**: https://platform.openai.com/docs/api-reference
- **Anthropic API**: https://docs.anthropic.com/claude/reference

### Gateway Projects

If you need advanced features (caching, load balancing, observability):

- **OpenRouter** - Managed service, 500+ models, easiest to get started
- **Portkey** - Open source, enterprise features, <1ms latency
- **Bifrost** - Fastest open source gateway (11µs overhead)
- **Helicone** - Focused on observability and monitoring

For L4, **OpenRouter** is recommended for its simplicity and broad model support.
