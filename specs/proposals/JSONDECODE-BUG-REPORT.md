# JSONDECODE Bug Report: Nested Structures Not Decoding

## Summary

JSONDECODE fails to properly decode:
1. **Lists of custom types** (`LIST OF CustomType`)
2. **Nested custom types** (records containing other record types)

Primitive types (STRING, NUMBER) decode correctly, but custom types in nested positions return `NOTHING` or `(LIST NOTHING)`.

## Test Case

File: `l4-encodings/test-jsondecode-nested.l4`

This file contains three minimal test cases demonstrating the bug:

### Test 1: List of Records ❌

**Input JSON:**
```json
{"items":[{"name":"first","value":1},{"name":"second","value":2}]}
```

**Type Definition:**
```l4
DECLARE `Item` HAS
  `name` IS A STRING
  `value` IS A NUMBER

DECLARE `List Container` HAS
  `items` IS A LIST OF `Item`
```

**Expected:** `JUST OF (List Container OF (LIST (Item OF "first", 1), (Item OF "second", 2)))`

**Actual:** `JUST OF (List Container OF (LIST NOTHING, NOTHING))`

**Result:** ❌ List elements are NOTHING instead of Item records

### Test 2: Nested Record ❌

**Input JSON:**
```json
{"nested":{"field":"hello"},"value":42}
```

**Type Definition:**
```l4
DECLARE `Inner` HAS
  `field` IS A STRING

DECLARE `Outer` HAS
  `nested` IS A `Inner`
  `value` IS A NUMBER
```

**Expected:** `JUST OF (Outer OF (Inner OF "hello"), 42)`

**Actual:** `JUST OF (Outer OF NOTHING, 42)`

**Result:** ❌ Nested `Inner` record is NOTHING, but primitive `value` works

### Test 3: Content Block List ❌

**Input JSON:**
```json
{"content":[{"type":"text","text":"Hello world"}]}
```

**Type Definition:**
```l4
DECLARE `Content Block` HAS
  `type` IS A STRING
  `text` IS A STRING

DECLARE `Content Container` HAS
  `content` IS A LIST OF `Content Block`
```

**Expected:** `JUST OF (Content Container OF (LIST (Content Block OF "text", "Hello world")))`

**Actual:** `JUST OF (Content Container OF (LIST NOTHING))`

**Result:** ❌ Content blocks in list are NOTHING

## Real-World Impact

This bug blocks the Thailand Cosmetics compliance project's ability to parse Anthropic Claude API responses.

**Real API response excerpt:**
```json
{
  "id": "msg_018cEZHkWRhKEXR7u5kGzMd2",
  "content": [{"type": "text", "text": "2+2 equals 4."}],
  "usage": {"input_tokens": 20, "output_tokens": 12, ...}
}
```

**Current behavior:**
- `id` ✅ decodes correctly (primitive STRING)
- `content` ❌ becomes `(LIST NOTHING)` (should be list of Content Block records)
- `usage` ❌ becomes `NOTHING` (should be nested Usage record)

## Steps to Reproduce

```bash
jl4-cli l4-encodings/test-jsondecode-nested.l4
```

Observe that all three `#EVAL` outputs show `NOTHING` or `(LIST NOTHING)` for nested structures.

## Expected Behavior

JSONDECODE should recursively decode nested structures:
- Lists should contain properly decoded custom type instances
- Nested record fields should contain properly decoded custom type instances
- This should work to arbitrary nesting depth

## Workaround

Until fixed, we can use string parsing on the raw JSON response, but this defeats the purpose of having typed JSONDECODE and loses all type safety.

## Related Files

- `l4-encodings/test-jsondecode-nested.l4` - Minimal test case (THIS IS THE KEY FILE)
- `l4-encodings/test-api-call.l4` - Real-world API response decoding attempt
- `l4-encodings/anthropicClient.l4` - API client library affected by this bug

## Environment

- jl4-cli version: Current (as of 2025-11-25)
- Platform: macOS (Darwin 24.6.0)
