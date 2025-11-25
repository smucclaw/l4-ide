# Issue: POST Operator Header Support

## Summary

The recently implemented `POST` operator in L4 does not currently support custom HTTP headers. Testing with httpbin.org confirms that the headers parameter is either ignored or not implemented.

## Current Status

**POST Signature (as observed):**
```l4
POST :: STRING -> STRING -> STRING -> STRING
//      URL        headers   body       response
```

**Tested:** 2025-11-24
**Result:** Custom headers are NOT being passed through to HTTP requests

## Test Evidence

### Test Files Created
- `l4-encodings/test-post.l4` - Basic POST (✅ works)
- `l4-encodings/test-post-headers.l4` - Newline-separated headers (❌ headers ignored)
- `l4-encodings/test-post-headers3.l4` - Multiple headers (❌ headers ignored)
- `l4-encodings/test-post-headers4.l4` - Single header tests (❌ headers ignored)

### Test Results

**Test 1: Newline-separated headers**
```l4
DECIDE testHeaderFormat IS
  POST
    "https://httpbin.org/post"
    "x-api-key: test-key\nanthropic-version: 2023-06-01\ncontent-type: application/json"
    "{\"test\": \"check if headers appear\"}"
```

**Response headers field:**
```json
{
  "headers": {
    "Accept-Encoding": "gzip",
    "Content-Length": "35",
    "Host": "httpbin.org",
    "X-Amzn-Trace-Id": "Root=1-692504af-05634895331d078d61053f1b"
  }
}
```
❌ **Custom headers (x-api-key, anthropic-version, content-type) do NOT appear**

**Test 2: Single custom header**
```l4
DECIDE testSingleHeader IS
  POST
    "https://httpbin.org/post"
    "X-Test-Header: test-value"
    "{\"message\": \"single header test\"}"
```

**Response headers field:**
```json
{
  "headers": {
    "Accept-Encoding": "gzip",
    "Content-Length": "33",
    "Host": "httpbin.org",
    "X-Amzn-Trace-Id": "Root=1-692504c0-143f5fca7523f1057cd292c9"
  }
}
```
❌ **Custom header (X-Test-Header) does NOT appear**

**Test 3: Content-Type header**
```l4
DECIDE testContentType IS
  POST
    "https://httpbin.org/post"
    "Content-Type: application/json"
    "{\"message\": \"content-type test\"}"
```

**Response headers field:**
```json
{
  "headers": {
    "Accept-Encoding": "gzip",
    "Content-Length": "32",
    "Host": "httpbin.org",
    "X-Amzn-Trace-Id": "Root=1-692504c1-4ab5f9ca1847bdf644da3ac1"
  }
}
```
❌ **Custom header (Content-Type) does NOT appear**

## Impact

This blocks the primary use case: **calling the Anthropic Claude API from L4**.

The Anthropic Messages API requires these headers:
```
POST https://api.anthropic.com/v1/messages
Headers:
  x-api-key: sk-ant-api03-...              ← REQUIRED for authentication
  anthropic-version: 2023-06-01            ← REQUIRED API version
  content-type: application/json           ← REQUIRED for JSON body
Body: {"model":"...","max_tokens":500,...}
```

Without header support, we cannot:
- Authenticate with the API (x-api-key)
- Specify API version (anthropic-version)
- Set content type properly (content-type)

## Requested Implementation

### Option A: Fix existing POST to support headers

**Current (broken):**
```l4
POST :: STRING -> STRING -> STRING -> STRING
//      URL        headers   body       response
```

**Proposed header format (newline-separated):**
```l4
DECIDE callAnthropicAPI IS
  POST
    "https://api.anthropic.com/v1/messages"
    "x-api-key: sk-ant-api03-...\nanthropic-version: 2023-06-01\ncontent-type: application/json"
    "{\"model\":\"claude-sonnet-4-5-20250929\",\"max_tokens\":500,\"messages\":[{\"role\":\"user\",\"content\":\"test\"}]}"
```

Each header should be on a separate line (separated by `\n`), following the format: `Header-Name: value`

### Option B: Use structured headers (LIST OF PAIR)

```l4
POST :: STRING -> LIST OF (PAIR OF STRING, STRING) -> STRING -> STRING
//      URL        [(header, value), ...]                body       response

DECIDE callAnthropicAPI IS
  POST
    "https://api.anthropic.com/v1/messages"
    LIST
      ("x-api-key", "sk-ant-api03-...")
      ("anthropic-version", "2023-06-01")
      ("content-type", "application/json")
    "{\"model\":\"...\",\"max_tokens\":500,...}"
```

### Recommendation

**Option A (newline-separated string)** is simpler and consistent with the current signature.

**Option B** is more type-safe but requires changing the signature.

Prefer **Option A** for faster implementation.

## Test Case for Verification

Once header support is implemented, this test should pass:

```l4
// Test custom header support
DECIDE testCustomHeaders IS
  POST
    "https://httpbin.org/post"
    "X-Custom-Header: my-value\nX-Another-Header: another-value"
    "{\"test\": \"headers\"}"

#EVAL testCustomHeaders
```

**Expected result:** The httpbin response should include in its `headers` field:
```json
{
  "headers": {
    "X-Custom-Header": "my-value",
    "X-Another-Header": "another-value",
    ...other standard headers...
  }
}
```

## Priority

**HIGH** - This blocks integration with external APIs that require authentication headers (Anthropic, OpenAI, etc.)

## Related Files

- `l4-encodings/test-post.l4` - Confirms basic POST works
- `l4-encodings/test-post-headers.l4` - First header test
- `l4-encodings/test-post-headers3.l4` - Multiple headers test
- `l4-encodings/test-post-headers4.l4` - Various header formats
- `l4-encodings/11-article41-complete-prompts.l4` - Contains 51 prompts waiting to be used with Anthropic API

## Workaround

None available. External API service would be required until header support is implemented.

---

## Resolution

**Status:** ✅ RESOLVED - Header support implemented and tested
**Created:** 2025-11-24
**Resolved:** 2025-11-25
**Tested with:** jl4-cli (commit 3f756dc3)

### Implementation Details

Headers are now parsed from the newline-separated format and passed to HTTP requests:

```haskell
-- Parse headers from newline-separated format: "Header-Name: value\nAnother-Header: value"
headerLines = filter (not . Text.null) $ Text.splitOn "\n" headersStr
parseHeader line =
  let (name, rest) = Text.breakOn ":" line
      value = Text.strip $ Text.drop 1 rest
  in if Text.null rest
     then Nothing
     else Just (Req.header (TE.encodeUtf8 name) (TE.encodeUtf8 value))
headerOptions = mapMaybe parseHeader headerLines
```

### Verification Tests

All three test cases now pass:

✅ **Test 1** (Single header): X-Test-Header appears in httpbin response
✅ **Test 2** (Multiple headers): X-Custom-Header, X-Another-Header, Content-Type all appear
✅ **Test 3** (API headers): x-api-key, anthropic-version, content-type all appear

Test file: `jl4/experiments/test-post-headers.l4`

### Next Steps

The POST operator is now ready for integration with external APIs:
- ✅ Anthropic Claude API authentication
- ✅ OpenAI API authentication
- ✅ Any HTTP API requiring custom headers

**Original Issue Documentation Below:**

---
