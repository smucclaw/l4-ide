# Getting Started with LLM Integration in L4

This tutorial will guide you through your first LLM query in L4, from setup to execution.

## Prerequisites

- L4 development environment set up
- Cabal build tool installed
- Command line access

## Step 1: Get an API Key

The easiest way to get started is with **OpenRouter**, which provides access to 500+ models through a single API:

1. Sign up at https://openrouter.ai/
2. Get your API key (starts with `sk-or-v1-...`)
3. Set it as an environment variable:

```bash
export OPENROUTER_API_KEY="sk-or-v1-YOUR_KEY_HERE"
```

## Step 2: Your First LLM Query

Create or view the simple example:

```bash
cat jl4/examples/ok/ai-simple.l4
```

The simplest possible query looks like this:

```l4
IMPORT llm

DECIDE simpleQuery IS
  queryLLMWithDefaults "What is 2+2?"

-- #EVAL simpleQuery
```

## Step 3: Run Your Query

```bash
# Run the example (uncomment the #EVAL line first)
cabal run jl4-cli -- jl4/examples/ok/ai-simple.l4
```

You should see a JSON response from the LLM containing the answer.

## Step 4: Understanding the Response

The response will be a JSON object containing the LLM's answer:

```json
{
  "choices": [{
    "message": {
      "role": "assistant",
      "content": "2+2 equals 4"
    }
  }]
}
```

## What's Happening Under the Hood

When you call `queryLLMWithDefaults`:

1. **Checks for API keys** in this order: `OPENROUTER_API_KEY` → `OPENAI_API_KEY` → `ANTHROPIC_API_KEY`
2. **Makes HTTP POST request** to the first available provider
3. **Sends your prompt** wrapped in the provider's required format
4. **Returns the response** as a JSON string

## Next Steps

Now that you've made your first query, you can:

- **Learn about providers**: See `doc/reference/llm-api.md` for provider comparison
- **Try advanced patterns**: See `doc/how-to/llm-querying.md` for use cases
- **Understand the vision**: See `doc/explanation/hybrid-reasoning.md` for architecture
- **Explore examples**: Check `jl4/examples/advanced/` for real-world workflows

## Troubleshooting

**"ERROR: API key not set"**
- Make sure you've exported the environment variable in your current shell
- Verify the variable is set: `echo $OPENROUTER_API_KEY`

**Build errors**
- Run `cabal clean` and rebuild: `cabal build jl4-cli`

**No response or timeout**
- Check your internet connection
- Verify your API key is valid (test at openrouter.ai)
- Try a shorter prompt or smaller `max_tokens`
