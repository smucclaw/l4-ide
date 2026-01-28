# HTTP and JSON Built-ins

L4 provides built-in functions for HTTP requests and JSON serialization. These enable integration with external APIs and data interchange.

## HTTP Functions

### FETCH

Performs an HTTP GET request.

**Signature:** `STRING → STRING`

**Parameters:**

- `url` - The URL to fetch

**Returns:** Response body as a STRING

**Example:** [http-json-example.l4](http-json-example.l4)

```l4
-- Fetch data from an API
DECIDE uuid IS FETCH "https://www.uuidtools.com/api/generate/v4"
```

**Note:** FETCH is evaluated lazily. Multiple references to the same FETCH call return the same cached result.

---

### POST

Performs an HTTP POST request.

**Signature:** `STRING → STRING → STRING → STRING`

**Parameters:**

- `url` - The URL to post to
- `headers` - HTTP headers as a JSON string (e.g., `"{\"Content-Type\": \"application/json\"}"`)
- `body` - Request body as a STRING

**Returns:** Response body as a STRING

**Example:** [http-json-example.l4](http-json-example.l4)

```l4
-- Post JSON to an API
DECIDE response IS POST
  "https://api.example.com/data"
  "{\"Content-Type\": \"application/json\"}"
  "{\"name\": \"test\"}"
```

---

### ENV

Reads an environment variable.

**Signature:** `STRING → STRING`

**Parameters:**

- `name` - The environment variable name

**Returns:** The value of the environment variable, or empty string if not set

**Example:** [http-json-example.l4](http-json-example.l4)

```l4
-- Read API key from environment
DECIDE apiKey IS ENV "API_KEY"
```

**Security Note:** Use ENV to keep sensitive values like API keys out of source code.

---

## JSON Functions

### JSONENCODE

Converts an L4 value to its JSON string representation.

**Signature:** `a → STRING` (polymorphic)

**Behavior:**

- Numbers render as JSON numbers
- Strings render as JSON strings (with escaping)
- Booleans render as `true` or `false`
- Lists render as JSON arrays
- Records render as JSON objects
- NOTHING renders as `null`
- JUST x renders as the encoding of x
- Enum constructors render as JSON objects with constructor name

**Example:** [http-json-example.l4](http-json-example.l4)

```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

DECIDE alice IS Person WITH name IS "Alice", age IS 30

-- Produces: {"name":"Alice","age":30}
DECIDE aliceJson IS JSONENCODE alice
```

---

### JSONDECODE

Parses a JSON string into an L4 value.

**Signature:** `STRING → EITHER STRING a` (polymorphic)

**Returns:**

- `RIGHT value` on successful parse
- `LEFT errorMessage` on parse failure

**Behavior:**

- JSON objects decode to records (field names must match)
- JSON arrays decode to lists
- JSON numbers decode to NUMBER
- JSON strings decode to STRING
- JSON booleans decode to BOOLEAN
- JSON null decodes to NOTHING (for MAYBE types)

**Example:** [http-json-example.l4](http-json-example.l4)

```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

-- Parse JSON into a Person record
DECIDE parsed IS JSONDECODE "{\"name\":\"Bob\",\"age\":25}"

-- Handle parse result
DECIDE person IS
  CONSIDER parsed
  WHEN RIGHT p THEN p
  WHEN LEFT err THEN Person WITH name IS "Unknown", age IS 0
```

**Type Inference:** JSONDECODE uses bidirectional type checking. The expected type guides parsing:

```l4
-- Type annotation guides decoding
ASSUME result IS A EITHER STRING Person
DECIDE result IS JSONDECODE jsonString
```

---

## Common Patterns

### API Integration

```l4
-- Fetch and parse JSON from an API
DECIDE fetchUser userId IS
  LET url IS CONCAT "https://api.example.com/users/", userId
  IN LET response IS FETCH url
     IN JSONDECODE response
```

### Authenticated Requests

```l4
-- Use environment variable for API key
DECIDE callApi endpoint body IS
  LET apiKey IS ENV "API_KEY"
  IN LET headers IS CONCAT "{\"Authorization\": \"Bearer ", apiKey, "\"}"
     IN POST endpoint headers body
```

### Error Handling

```l4
-- Safely decode with fallback
GIVEN jsonStr IS A STRING
GIVEN fallback IS A Person
GIVETH A Person
safeDecode jsonStr fallback MEANS
  CONSIDER JSONDECODE jsonStr
  WHEN RIGHT person THEN person
  WHEN LEFT _ THEN fallback
```

---

## See Also

- **[LLM Library](../libraries/llm.md)** - Higher-level LLM API integration
- **[Type Coercions](../types/coercions.md)** - TOSTRING and other conversions
- **[EITHER Type](../types/README.md)** - Error handling with EITHER
