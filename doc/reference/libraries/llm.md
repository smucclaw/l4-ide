# LLM Library

LLM API integration for calling language models from L4 code. Import it into L4 files with `IMPORT llm`.

### Location

[jl4-core/libraries/llm.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/llm.l4)

### Supported Providers

- **Anthropic Claude** (direct API)
- **OpenAI** (direct API)
- **OpenRouter** (unified gateway for 500+ models)

### Key Functions

- `callAnthropic apiKey model prompt maxTokens`
- `callOpenAI apiKey model prompt maxTokens`
- `callOpenRouter apiKey model prompt maxTokens`
- `queryLLMWithDefaults prompt` - Auto-detects available API keys
- `callClaude apiKey prompt` - Convenience for Claude Sonnet
- `callGPT4 apiKey prompt` - Convenience for GPT-4

### Model Constants

- `claudeOpusModel`, `claudeSonnetModel`
- `gpt4Model`, `gpt4TurboModel`, `gpt35Model`
- `llama3Model`, `geminiProModel`

### Configuration

Set environment variables: `ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, or `OPENROUTER_API_KEY`
