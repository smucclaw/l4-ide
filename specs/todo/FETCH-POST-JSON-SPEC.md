# L4 HTTP and JSON Enhancement Specification

**Project**: L4 IDE (l4-ide codebase)
**Component**: Runtime / Standard Library
**Purpose**: Enable HTTP POST requests and JSON encoding/decoding for LLM API integration
**Related**: Thailand Cosmetics Compliance Checker

---

## Executive Summary

The L4 language currently has a `FETCH` operator that performs HTTP GET requests and returns the response body as a STRING. To enable integration with the Anthropic Claude API (and other REST APIs), we need to extend L4 with:

1. **HTTP POST** capability with headers and JSON body
2. **JSON encoding** (L4 data structures → JSON STRING)
3. **JSON decoding** (JSON STRING → L4 data structures)

---

## Current State

### Existing `FETCH` Operator

**Signature**: `FETCH :: STRING -> STRING`

**Example**:

```l4
DECIDE fetchUUID IS
  FETCH "https://www.uuidtools.com/api/generate/v4"

#EVAL fetchUUID
// Returns: "[\"4270299b-511a-4503-8b4f-e22fde620f94\"]"
```

**Limitations**:

- ✅ HTTP GET requests work
- ❌ No way to specify HTTP method (POST, PUT, DELETE)
- ❌ No way to add custom headers (Authorization, Content-Type)
- ❌ No way to send request body
- ✅ Returns raw response as STRING
- ❌ No JSON parsing/encoding built-in

---

## Requirements

### Use Case: Anthropic Claude API Integration

We need to call the Anthropic Messages API to evaluate cosmetic advertising claims:

**API Endpoint**: `https://api.anthropic.com/v1/messages`

**Required HTTP Headers**:

```
x-api-key: sk-ant-api03-...
anthropic-version: 2023-06-01
content-type: application/json
```

**Required JSON Request Body**:

```json
{
  "model": "claude-sonnet-4-5-20250929",
  "max_tokens": 500,
  "messages": [
    {
      "role": "user",
      "content": "Does this claim violate regulations: 'Treats acne permanently'?"
    }
  ]
}
```

**Expected JSON Response**:

```json
{
  "id": "msg_...",
  "type": "message",
  "role": "assistant",
  "content": [
    {
      "type": "text",
      "text": "YES\nConfidence: 95%\nReasoning: ..."
    }
  ],
  "model": "claude-sonnet-4-5-20250929",
  "stop_reason": "end_turn",
  "usage": {
    "input_tokens": 1234,
    "output_tokens": 567
  }
}
```

---

## Proposed Solution

### Option A: Extend `FETCH` with Optional Parameters

Extend the existing `FETCH` operator to accept optional configuration:

**Proposed Signature**:

```l4
FETCH :: STRING -> STRING
FETCH :: STRING -> HTTP Config -> STRING

TYPE `HTTP Config` HAS
  `method`           IS A MAYBE STRING       // "GET", "POST", "PUT", etc.
  `headers`          IS A MAYBE (LIST OF (PAIR OF STRING, STRING))
  `body`             IS A MAYBE STRING
  `timeout seconds`  IS A MAYBE NUMBER
```

**Example Usage**:

```l4
IMPORT prelude

DECLARE `anthropic config` HAS
  `method`           IS A MAYBE STRING
  `headers`          IS A MAYBE (LIST OF (PAIR OF STRING, STRING))
  `body`             IS A MAYBE STRING

DECIDE `make api call` IS
  FETCH
    "https://api.anthropic.com/v1/messages"
    `anthropic config` {
      `method` IS JUST "POST"
      `headers` IS JUST (
        LIST
          (PAIR OF "x-api-key", "sk-ant-api03-...")
          (PAIR OF "anthropic-version", "2023-06-01")
          (PAIR OF "content-type", "application/json")
      )
      `body` IS JUST "{\"model\":\"claude-sonnet-4-5\",\"max_tokens\":500}"
    }

#EVAL `make api call`
```

### Option B: Separate `POST` Operator

Create a new operator specifically for POST requests:

**Proposed Signature**:

```l4
POST :: STRING -> LIST OF (PAIR OF STRING, STRING) -> STRING -> STRING
//      URL        headers                              body     response
```

**Example Usage**:

```l4
DECIDE `api response` IS
  POST
    "https://api.anthropic.com/v1/messages"
    (LIST
      (PAIR OF "x-api-key", "sk-ant-api03-...")
      (PAIR OF "anthropic-version", "2023-06-01")
      (PAIR OF "content-type", "application/json"))
    "{\"model\":\"claude-sonnet-4-5\",\"max_tokens\":500}"
```

### Option C: HTTP Function Family

Create a family of HTTP functions:

```l4
HTTP GET  :: STRING -> STRING
HTTP POST :: STRING -> HTTP Headers -> HTTP Body -> STRING
HTTP PUT  :: STRING -> HTTP Headers -> HTTP Body -> STRING

TYPE `HTTP Headers` IS A LIST OF (PAIR OF STRING, STRING)
TYPE `HTTP Body`    IS A STRING
```

---

## JSON Encoding/Decoding

### JSON Encoding Functions

**Proposed Signatures**:

```l4
// Encode L4 values to JSON STRING
JSON ENCODE :: a -> STRING

// Type-specific encoders
JSON ENCODE STRING  :: STRING -> STRING
JSON ENCODE NUMBER  :: NUMBER -> STRING
JSON ENCODE BOOLEAN :: BOOLEAN -> STRING
JSON ENCODE LIST    :: LIST OF a -> STRING
JSON ENCODE RECORD  :: {fields} -> STRING

// Example: Encode a request
DECLARE `API Request` HAS
  `model`      IS A STRING
  `max_tokens` IS A NUMBER
  `messages`   IS A LIST OF `Message`

DECLARE `Message` HAS
  `role`    IS A STRING
  `content` IS A STRING

DECIDE `request json` IS
  JSON ENCODE (`API Request` {
    `model` IS "claude-sonnet-4-5-20250929"
    `max_tokens` IS 500
    `messages` IS LIST (
      `Message` {
        `role` IS "user"
        `content` IS "Evaluate this claim..."
      }
    )
  })

// Result: "{\"model\":\"claude-sonnet-4-5-20250929\",\"max_tokens\":500,\"messages\":[{\"role\":\"user\",\"content\":\"Evaluate this claim...\"}]}"
```

### JSON Decoding Functions

**Proposed Signatures**:

```l4
// Parse JSON STRING to L4 value
JSON DECODE :: STRING -> MAYBE a

// Type-specific decoders
JSON DECODE STRING  :: STRING -> MAYBE STRING
JSON DECODE NUMBER  :: STRING -> MAYBE NUMBER
JSON DECODE BOOLEAN :: STRING -> MAYBE BOOLEAN
JSON DECODE LIST    :: STRING -> MAYBE (LIST OF a)
JSON DECODE OBJECT  :: STRING -> MAYBE {fields}

// Example: Parse API response
DECLARE `API Response` HAS
  `id`           IS A STRING
  `content`      IS A LIST OF `Content Block`
  `model`        IS A STRING
  `stop_reason`  IS A STRING
  `usage`        IS A `Usage Stats`

DECLARE `Content Block` HAS
  `type` IS A STRING
  `text` IS A STRING

DECLARE `Usage Stats` HAS
  `input_tokens`  IS A NUMBER
  `output_tokens` IS A NUMBER

DECIDE `parsed response` IS
  JSON DECODE `API Response` (
    "{\"id\":\"msg_123\",\"content\":[{\"type\":\"text\",\"text\":\"YES\"}],\"model\":\"claude-sonnet\",\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":100,\"output_tokens\":50}}"
  )

CONSIDER `parsed response`
  WHEN JUST response THEN
    response.content
  WHEN NOTHING THEN
    LIST ("Parse error")
```

### Simplified JSON Access (Optional Enhancement)

For simpler use cases, provide path-based JSON accessors:

```l4
JSON GET :: STRING -> STRING -> MAYBE STRING
//          JSON      path      value

DECIDE `extract text` IS
  JSON GET
    "{\"content\":[{\"type\":\"text\",\"text\":\"YES\"}]}"
    "content[0].text"

// Result: JUST "YES"
```

---

## Recommended Implementation Strategy

### Phase 1: HTTP POST (Minimal Viable)

Implement **Option B** (separate `POST` operator) as the quickest path:

```l4
POST :: STRING -> LIST OF (PAIR OF STRING, STRING) -> STRING -> STRING
```

**Rationale**:

- Minimal changes to existing `FETCH`
- Clear separation of concerns (GET vs POST)
- Simple signature easy to understand
- Can implement quickly

**Implementation Notes**:

- Use same HTTP client library as `FETCH`
- Add header support (key-value pairs)
- Support arbitrary request body as STRING
- Return response body as STRING
- Handle HTTP errors gracefully (return empty STRING or special error format)

### Phase 2: JSON Encoding

Implement basic JSON encoding for L4 primitives and structures:

```l4
JSON ENCODE :: a -> STRING
```

**Must Support**:

- STRING → JSON string (with escaping: `"hello"` → `"\"hello\""`)
- NUMBER → JSON number (`42` → `"42"`, `3.14` → `"3.14"`)
- BOOLEAN → JSON boolean (`TRUE` → `"true"`, `FALSE` → `"false"`)
- LIST → JSON array (`LIST 1, 2, 3` → `"[1,2,3]"`)
- MAYBE → JSON null/value (`NOTHING` → `"null"`, `JUST x` → encode x)
- Records → JSON objects (field names become keys)

**Implementation Notes**:

- Use standard JSON encoding library (e.g., Aeson in Haskell)
- Proper string escaping for special characters
- Handle nested structures recursively
- Pretty-print option for debugging?

### Phase 3: JSON Decoding

Implement JSON parsing to L4 values:

```l4
JSON DECODE :: STRING -> MAYBE a
```

**Must Support**:

- JSON string → STRING
- JSON number → NUMBER
- JSON boolean → BOOLEAN
- JSON array → LIST OF a
- JSON null → NOTHING
- JSON object → Record type (requires type annotation/schema)

**Implementation Notes**:

- Return `NOTHING` on parse errors
- Type-driven decoding (need target type specified)
- Graceful handling of missing fields (use MAYBE)
- Array/object nesting support

**Challenges**:

- L4 is statically typed; JSON objects need target type
- May need schema/type annotation at call site
- Consider simpler path-based accessors as alternative

### Phase 4: Enhanced HTTP (Optional)

If needed, add **Option A** (enhanced `FETCH` with config) or **Option C** (HTTP function family) for:

- PUT, DELETE, PATCH methods
- Timeout configuration
- Response headers access
- HTTP status code checking
- Cookie support
- Redirect handling

---

## Test Cases

### Test 1: Simple POST Request

```l4
DECIDE `test post` IS
  POST
    "https://httpbin.org/post"
    (LIST (PAIR OF "Content-Type", "application/json"))
    "{\"test\": \"data\"}"

#EVAL `test post`
// Should return httpbin echo response containing our JSON
```

### Test 2: Anthropic API Call (with manual JSON)

```l4
DECIDE `call anthropic` IS
  POST
    "https://api.anthropic.com/v1/messages"
    (LIST
      (PAIR OF "x-api-key", "sk-ant-api03-YOUR-KEY")
      (PAIR OF "anthropic-version", "2023-06-01")
      (PAIR OF "content-type", "application/json"))
    "{\"model\":\"claude-sonnet-4-5-20250929\",\"max_tokens\":100,\"messages\":[{\"role\":\"user\",\"content\":\"Say hello\"}]}"

#EVAL `call anthropic`
// Should return Claude's response as JSON STRING
```

### Test 3: JSON Encoding

```l4
DECLARE `Person` HAS
  `name` IS A STRING
  `age`  IS A NUMBER

DECIDE `encode person` IS
  JSON ENCODE (`Person` {
    `name` IS "Alice"
    `age` IS 30
  })

#EVAL `encode person`
// Should return: "{\"name\":\"Alice\",\"age\":30}"
```

### Test 4: JSON Decoding

```l4
DECLARE `Response` HAS
  `success` IS A BOOLEAN
  `message` IS A STRING

DECIDE `parse response` IS
  JSON DECODE `Response` "{\"success\":true,\"message\":\"OK\"}"

#EVAL `parse response`
// Should return: JUST (`Response` { `success` IS TRUE, `message` IS "OK" })
```

### Test 5: End-to-End API Integration

```l4
IMPORT prelude

// 1. Encode request
DECLARE `Claude Request` HAS
  `model`      IS A STRING
  `max_tokens` IS A NUMBER
  `messages`   IS A LIST OF `Message`

DECLARE `Message` HAS
  `role`    IS A STRING
  `content` IS A STRING

DECIDE `request body` IS
  JSON ENCODE (`Claude Request` {
    `model` IS "claude-sonnet-4-5-20250929"
    `max_tokens` IS 100
    `messages` IS LIST (
      `Message` { `role` IS "user", `content` IS "Say hello" }
    )
  })

// 2. Make API call
DECIDE `api response` IS
  POST
    "https://api.anthropic.com/v1/messages"
    (LIST
      (PAIR OF "x-api-key", "YOUR-KEY")
      (PAIR OF "anthropic-version", "2023-06-01")
      (PAIR OF "content-type", "application/json"))
    `request body`

// 3. Parse response
DECLARE `Claude Response` HAS
  `content` IS A LIST OF `Content Block`

DECLARE `Content Block` HAS
  `text` IS A STRING

DECIDE `parsed response` IS
  JSON DECODE `Claude Response` `api response`

// 4. Extract text
DECIDE `response text` IS
  CONSIDER `parsed response`
    WHEN JUST response THEN
      CONSIDER response.content
        WHEN EMPTY THEN "No response"
        WHEN first FOLLOWED BY rest THEN first.text
    WHEN NOTHING THEN "Parse error"

#EVAL `response text`
// Should return: "Hello!"
```

---

## Error Handling

### HTTP Errors

**Requirement**: Handle HTTP error responses (4xx, 5xx) gracefully

**Options**:

1. Return error JSON in response STRING (caller parses)
2. Return MAYBE STRING (NOTHING on error)
3. Return EITHER STRING STRING (left = error, right = success)

**Recommendation**: Option 1 (return raw response) for simplicity, let L4 code handle errors

### JSON Parse Errors

**Requirement**: Handle malformed JSON gracefully

**Approach**: Return `NOTHING` from `JSON DECODE` on parse failures

**Example**:

```l4
DECIDE `parse bad json` IS
  JSON DECODE `Response` "{not valid json"

#EVAL `parse bad json`
// Returns: NOTHING
```

### Network Errors

**Requirement**: Handle network failures (timeout, connection refused, DNS errors)

**Options**:

1. Return empty STRING on network error
2. Return special error format: `"{\"error\": \"Network timeout\"}"`
3. Throw L4 exception (if exception mechanism exists)

**Recommendation**: Option 2 (structured error JSON) for consistent handling

---

## Performance Considerations

### Caching

**Consideration**: HTTP requests are expensive and may be called multiple times during evaluation

**Recommendation**:

- Implement lazy evaluation (current behavior)
- Consider memoization/caching for identical requests within single evaluation
- Document caching behavior clearly

### Async Evaluation

**Consideration**: HTTP calls block evaluation

**Future Enhancement**:

- Parallel evaluation of independent FETCH/POST calls
- Async primitives (FETCH ASYNC, AWAIT, etc.)
- Not required for MVP

### Rate Limiting

**Consideration**: External APIs have rate limits

**Recommendation**:

- Document that rate limiting is caller's responsibility
- Consider adding rate limiter as library function later
- For Anthropic API: ~50 requests/min for Sonnet

---

## Security Considerations

### API Key Management

**Issue**: API keys in L4 source code are visible

**Mitigations**:

- Document best practices (use environment variables)
- Consider `ENV :: STRING -> MAYBE STRING` operator to read env vars
- Example: `ENV "ANTHROPIC_API_KEY"`

### Input Validation

**Issue**: User-supplied strings in HTTP requests could be malicious

**Mitigations**:

- Validate URLs (reject non-HTTP(S) schemes?)
- Sanitize header values
- Document security considerations

### SSL/TLS

**Requirement**: HTTPS requests must verify certificates

**Implementation**: Use standard HTTP client with SSL verification enabled

---

## Documentation Requirements

### Function Documentation

For each new operator, provide:

- Type signature
- Plain English description
- Parameter descriptions
- Return value description
- Example usage (simple and complex)
- Error conditions
- Performance characteristics

### Tutorial

Create tutorial showing:

1. Simple GET request (existing `FETCH`)
2. POST request with headers
3. JSON encoding basics
4. JSON decoding basics
5. Complete API integration example (Anthropic)
6. Error handling patterns

### Migration Guide

If changing existing `FETCH`:

- Document breaking changes
- Provide migration examples
- Deprecation timeline

---

## Implementation Checklist

### Phase 1: POST Operator

- [ ] Define `POST` function signature in prelude/standard library
- [ ] Implement HTTP POST with headers and body
- [ ] Add to type checker
- [ ] Add to evaluator/interpreter
- [ ] Unit tests (httpbin.org)
- [ ] Documentation
- [ ] Integration test (real API call)

### Phase 2: JSON Encoding

- [ ] Define `JSON ENCODE` function signature
- [ ] Implement encoding for primitives (STRING, NUMBER, BOOLEAN)
- [ ] Implement encoding for LIST
- [ ] Implement encoding for MAYBE (null handling)
- [ ] Implement encoding for records/objects
- [ ] Handle special characters/escaping
- [ ] Unit tests
- [ ] Documentation

### Phase 3: JSON Decoding

- [ ] Define `JSON DECODE` function signature
- [ ] Implement decoding for primitives
- [ ] Implement decoding for arrays → LIST
- [ ] Implement decoding for objects → records (type-driven)
- [ ] Handle parse errors → NOTHING
- [ ] Handle null → NOTHING
- [ ] Unit tests
- [ ] Documentation

### Phase 4: Integration Testing

- [ ] Test POST + JSON ENCODE + JSON DECODE together
- [ ] Test with real Anthropic API
- [ ] Test error conditions
- [ ] Performance testing
- [ ] Security review

### Phase 5: Documentation & Examples

- [ ] Update language reference
- [ ] Create tutorial
- [ ] Add example files
- [ ] Update VSCode extension (syntax highlighting, IntelliSense)

---

## Questions for Implementation Team

1. **Which option do you prefer for HTTP POST?**
   - Option A: Extend FETCH with config record
   - Option B: Separate POST operator (recommended)
   - Option C: HTTP function family (GET, POST, PUT, etc.)

2. **For JSON decoding, how should we handle type specification?**
   - Type annotation at call site? `JSON DECODE `TargetType` json_string`
   - Infer from usage context?
   - Separate decoders per type? `JSON DECODE STRING`, `JSON DECODE NUMBER`, etc.

3. **Should we support environment variables for API keys?**
   - Add `ENV :: STRING -> MAYBE STRING` operator?
   - Or handle externally?

4. **Error handling strategy?**
   - HTTP errors: return error JSON, MAYBE STRING, or EITHER?
   - JSON parse errors: NOTHING sufficient?

5. **Performance: Should HTTP calls be memoized/cached within evaluation?**

6. **Timeline: What's realistic for Phase 1 (POST) implementation?**

---

## Priority

**HIGH** - This functionality is blocking the Thailand Cosmetics Compliance Checker project, which requires calling Anthropic Claude API to evaluate 51 regulatory prompts against advertising claims.

**Target**: Phase 1 (POST operator) is MVP, enables manual JSON encoding. Phases 2-3 (JSON encoding/decoding) are quality-of-life improvements.

---

## References

- **Anthropic API Documentation**: https://docs.anthropic.com/claude/reference/messages_post
- **L4 Prelude**: https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/prelude.l4
- **Production L4 Projects**: Real-world L4 projects with LLM integration
- **Existing FETCH Sample**: `l4-encodings/sample-fetch-uuid.l4`
