# Feature Request: String Concatenation Operator

## Summary

Add string concatenation support to L4 to enable dynamic construction of strings from multiple components. This is critical for API integration where JSON bodies, headers, and URLs need to be built with variable substitution.

## Motivation

### Current Blocker: Cannot Substitute Variables into Strings

We have 51 prompt templates for evaluating cosmetic advertising claims against Thai regulations (see `l4-encodings/11-article41-complete-prompts.l4`). Each template contains a `{claim_text}` placeholder that needs to be substituted with actual advertising text before sending to the Anthropic Claude API.

**Example prompt template:**

```l4
`PROMPT acne therapeutic claims` MEANS
  "REGULATORY TEST: Acne Therapeutic Claims (Acne Category)
Context: Thai Cosmetics Manual BE 2567, Category 3 (Acne-Prone Skin), p.24

Principle: Cosmetics for acne-prone skin can only identify themselves as
formulated for that skin type. They cannot claim therapeutic efficacy...

Evaluate this claim: {claim_text}

Question: Does this claim imply therapeutic efficacy in PREVENTING, TREATING,
REDUCING INFLAMMATION, or ELIMINATING acne?

Answer with:
- YES (violates acne category rules - prohibited) or NO (acceptable)
- Confidence: [0-100%]
- Reasoning: [1-2 sentences]"
```

**What we need to do:**

```l4
// Substitute {claim_text} placeholder with actual claim
DECIDE evaluateAcneClaim MEANS
  ... blah blah prompt blah ...
  WHERE
    prompt MEANS
      SUBSTITUTE (`PROMPT acne therapeutic claims`) "{claim_text}" "This cream eliminates acne permanently"
  IN
  ...
```

**Without string operations, we cannot:**

1. Substitute placeholders in prompt templates
2. Construct JSON request bodies dynamically
3. Build headers with variable API keys
4. Create URLs with query parameters

## Current Workaround and Limitations

### Workaround: Hardcode Everything

```l4
// Must hardcode the entire string for each specific input
DECIDE evaluateSpecificClaim IS
  POST
    "https://api.anthropic.com/v1/messages"
    "x-api-key: sk-ant-...\nanthropic-version: 2023-06-01\ncontent-type: application/json"
    "{\"model\":\"claude-sonnet-4-5-20250929\",\"messages\":[{\"role\":\"user\",\"content\":\"REGULATORY TEST: Acne Therapeutic Claims...\\n\\nEvaluate this claim: This cream eliminates acne permanently\\n\\nQuestion: Does this claim...\"}]}"
```

**Limitations:**

- Cannot reuse prompt templates
- Must copy-paste entire prompt for each claim evaluation
- No abstraction or modularity
- Error-prone (easy to introduce inconsistencies)

## Proposed Solution: CONCAT Operator

Add a `CONCAT` operator that works similarly to how `LIST` constructs lists from multiple elements.

### Syntax Option A: Variadic CONCAT Function

````l4
// Concatenate multiple strings into one
DECIDE fullName IS
  CONCAT "John", " ", "Doe"
// Result: "John Doe"

// With variables. we have `name` IS A STRING
DECIDE greeting IS
  CONCAT "Hello, ", name, "!"
// If name = "Alice", result: "Hello, Alice!"

```l4
CONCAT :: function from STRING+ to STRING
// Variadic: accepts 1 or more STRING arguments, returns STRING
// if it helps, we could think of CONCAT as being implicitly
// being CONCAT OF LIST OF ...
// but the LIST is "hidden".
````

## Detailed Examples with CONCAT

### Example 1: Template Substitution

```l4
// Define a prompt template as a function that takes parameters
GIVEN claimText IS A STRING
GIVETH STRING
`evaluate acne claim with` claimText MEANS
  CONCAT
    "REGULATORY TEST: Acne Therapeutic Claims\n",
    "Context: Thai Cosmetics Manual BE 2567, Category 3\n\n",
    "Evaluate this claim: ", claimText, "\n\n",
    "Question: Does this claim imply therapeutic efficacy?\n",
    "Answer with YES or NO."

// Use it
DECIDE prompt1 IS
  `evaluate acne claim with` "This cream eliminates acne"

DECIDE prompt2 IS
  `evaluate acne claim with` "Formulated for acne-prone skin"
```

### Example 2: JSON Construction

```l4
GIVEN apiKey IS A STRING
      userMessage IS A STRING
GIVETH STRING
`build anthropic request` apiKey userMessage MEANS
  CONCAT
    "{\"model\":\"claude-sonnet-4-5-20250929\",",
    "\"max_tokens\":500,",
    "\"messages\":[{\"role\":\"user\",\"content\":\"",
    userMessage,
    "\"}]}"

// Headers
GIVEN apiKey IS A STRING
GIVETH STRING
`build headers with` apiKey MEANS
  CONCAT
    "x-api-key: ", apiKey, "\n",
    "anthropic-version: 2023-06-01\n",
    "content-type: application/json"
```

### Example 3: URL Construction

```l4
GIVEN endpoint IS A STRING
      queryParam IS A STRING
GIVETH STRING
`build url` endpoint queryParam MEANS
  CONCAT "https://api.example.com/", endpoint, "?q=", queryParam

DECIDE searchUrl IS
  `build url` "search" "cosmetics"
// Result: "https://api.example.com/search?q=cosmetics"
```

### Example 4: Complex Template with Multiple Substitutions

```l4
GIVEN
  testName IS A STRING
  principle IS A STRING
  acceptableExample IS A STRING
  prohibitedExample IS A STRING
  claimText IS A STRING
  question IS A STRING
GIVETH STRING
`build regulatory test prompt`
  testName principle acceptableExample prohibitedExample claimText question
MEANS
  CONCAT
    "REGULATORY TEST: ", testName, "\n\n",
    "Principle: ", principle, "\n\n",
    "Decision Boundary:\n",
    "✅ ACCEPTABLE: ", acceptableExample, "\n",
    "❌ PROHIBITED: ", prohibitedExample, "\n\n",
    "Evaluate this claim: ", claimText, "\n\n",
    "Question: ", question, "\n\n",
    "Answer with YES or NO, Confidence [0-100%], and Reasoning."
```

## Type Behavior

L4 doesn't currently expose stringification as an operator,
though adding such a thing would not be so hard because the Haskell
codebase already supports a show/print style stringification for
source text. In the following code the `number AS STRING` snippets
represent a feature to be implemented.

### Input Types

- **CONCAT** should accept only STRING arguments
- Type error if non-STRING provided without explicit conversion
- NUMBER can be converted with `AS STRING` (TODO)

```l4
DECIDE valid IS
  CONCAT "Age: ", (42 AS STRING)  // ✅ Valid - explicit conversion

DECIDE invalid IS
  CONCAT "Age: ", 42  // ❌ Type error - cannot concat STRING and NUMBER
```

### Empty Cases

```l4
DECIDE empty IS
  CONCAT  // ❌ Type error - requires at least one argument

DECIDE single IS
  CONCAT "hello"  // ✅ Valid - returns "hello" (identity)

DECIDE withEmpty IS
  CONCAT "hello", "", "world"  // ✅ Valid - returns "helloworld"
```

### Nested CONCAT

```l4
DECIDE nested IS
  CONCAT
    "outer ",
    CONCAT "inner1 ", "inner2",
    " outer"
// Result: "outer inner1 inner2 outer"
```

## Test Cases

### Basic Concatenation

```l4
// Test 1: Two strings
DECIDE test1 IS CONCAT "hello", "world"
#ASSERT test1 EQUALS "helloworld"

// Test 2: Three strings
DECIDE test2 IS CONCAT "hello", " ", "world"
#ASSERT test2 EQUALS "hello world"

// Test 3: Many strings
DECIDE test3 IS CONCAT "a", "b", "c", "d", "e"
#ASSERT test3 EQUALS "abcde"
```

### With Variables

```l4
DECIDE firstName IS "Alice"
DECIDE lastName IS "Smith"

DECIDE fullName IS CONCAT firstName, " ", lastName
#ASSERT fullName EQUALS "Alice Smith"
```

### With Number Conversion

```l4
DECIDE age IS 25
DECIDE message IS CONCAT "Age is ", (age AS STRING)
#ASSERT message EQUALS "Age is 25"
```

### Special Characters

```l4
// Test escaped characters
DECIDE jsonString IS CONCAT "{\"key\":\"", "value", "\"}"
#ASSERT jsonString EQUALS "{\"key\":\"value\"}"

// Test newlines
DECIDE multiline IS CONCAT "line1", "\n", "line2"
#ASSERT multiline EQUALS "line1\nline2"
```

### Empty Strings

```l4
DECIDE withEmpty IS CONCAT "", "hello", ""
#ASSERT withEmpty EQUALS "hello"

DECIDE allEmpty IS CONCAT "", "", ""
#ASSERT allEmpty EQUALS ""
```

### Nested Concatenation

```l4
DECIDE inner IS CONCAT "b", "c"
DECIDE outer IS CONCAT "a", inner, "d"
#ASSERT outer EQUALS "abcd"
```

### Realistic API Example

```l4
DECIDE apiKey IS "sk-ant-api03-12345"
DECIDE claimText IS "This cream eliminates acne permanently"

DECIDE headers IS
  CONCAT
    "x-api-key: ", apiKey, "\n",
    "content-type: application/json"

DECIDE body IS
  CONCAT
    "{\"model\":\"claude-sonnet-4-5-20250929\",",
    "\"max_tokens\":500,",
    "\"messages\":[{\"role\":\"user\",\"content\":\"",
    claimText,
    "\"}]}"

DECIDE response IS
  POST "https://api.anthropic.com/v1/messages" headers body

// Verify headers constructed correctly
#ASSERT headers EQUALS "x-api-key: sk-ant-api03-12345\ncontent-type: application/json"
```

## Implementation Considerations

### Performance

- For small number of strings (< 10), naive concatenation is fine
- For many strings, consider using a string builder approach internally, though if this complicates the code, go with something that is readable and straightforward, vs something that is industrial-grade code that only an advanced Haskell programmer could love.
- Since L4 is functional, CONCAT should not mutate but return new string

### Memory

- Create new string objects (immutable strings)
- L4's functional nature means strings are immutable anyway

### Precedence

- CONCAT as a function doesn't have precedence issues

### Error Messages

```
Type error: CONCAT expects STRING arguments, but received NUMBER
  at line 42: CONCAT "Age: ", 25
                                ^^
  help: convert to string with (25 AS STRING)
```

We probably don't want to be too helpful and attempt automatic
stringification for everything because how would that work for
things like complex record data types? Would stirngification be
tantamount to JSONification?

## Alternative: String Interpolation (Future Enhancement)

Once CONCAT exists, consider adding string interpolation syntax as syntactic sugar:

```l4
// Future possibility (not part of this spec)
DECIDE greeting IS
  "Hello, ${name}!"  // Desugars to: CONCAT "Hello, ", name, "!"

DECIDE jsonBody IS
  "{\"name\":\"${name}\",\"age\":${age}}"
```

But for now, explicit CONCAT is sufficient and clearer.

## Impact

### Immediate Use Cases Unblocked

1. **Thailand Cosmetics Compliance Checker**: 51 prompt templates can be used with variable substitution
2. **Anthropic API Integration**: Dynamic construction of JSON request bodies
3. **Any API Integration**: Headers, URLs, request bodies can be built dynamically
4. **General String Manipulation**: Any L4 program needing string operations

### Code Examples That Will Work

From `l4-encodings/11-article41-complete-prompts.l4`:

```l4
// Current (static template)
`PROMPT acne therapeutic claims` MEANS "...{claim_text}..."

// After CONCAT (can substitute)
GIVEN claim IS A STRING
GIVETH STRING
`evaluate acne claim` claim MEANS
  CONCAT
    // First part of prompt before {claim_text}
    "REGULATORY TEST: Acne Therapeutic Claims\n...\nEvaluate this claim: "
    claim
    "\n\nQuestion: Does this claim imply therapeutic efficacy?\n...
```

## Priority

**HIGH** - This is a fundamental string operation blocking:

- All 51 regulatory test prompts from being usable
- API integration (Anthropic, any other API)
- Dynamic JSON/URL construction
- Practical L4 programs that interact with external services

## Related Issues

- POST operator with headers: ✅ DONE (implemented 2025-11-24)
- JSON encoding/decoding: possibly donee (would complement CONCAT)
- String interpolation: Future (syntactic sugar over CONCAT)
- Stringification for primitive types (TODO, could be done as part of this work if you feel up for it.)

## Summary

Please implement `CONCAT` as a variadic function that takes 1+ STRING arguments and returns a STRING by concatenating them in order. This is a critical primitive for building practical L4 applications that interact with external APIs.

---

**Status**: 🟡 REQUESTED
**Priority**: HIGH
**Created**: 2025-11-24
**Use Case**: Thailand Cosmetics Compliance Checker (51 regulatory test prompts waiting to be used)
**Dependencies**: None (POST with headers already works)
