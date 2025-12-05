# Testing Guide: Web Sessions & Mixfix Notation

**Created:** 2025-12-05
**Purpose:** Practical guide for testing recent L4 features

---

## Part 1: Web-Sessions → Decision Service Integration

### Overview

As of PR #649, the web-sessions playground automatically pushes saved programs to the decision service, making them immediately available for API evaluation.

### Architecture

```
User writes L4 in web editor
    ↓
Saves/shares (generates UUID)
    ↓
jl4-websessions server saves to SQLite
    ↓ (auto-push if configured)
Decision Service loads as /functions/{uuid}:{name}
    ↓
Available for REST API calls
```

### Prerequisites

1. **Start the Decision Service**

   ```bash
   cd jl4-decision-service
   cabal run jl4-decision-service
   # Runs on http://localhost:8081 by default
   ```

2. **Start the Web Sessions Server**

   ```bash
   cd jl4-websessions
   cabal run jl4-websessions -- 3000 sessions.db http://localhost:8081
   # Args: <port> <db-path> [decision-service-url]
   ```

3. **Verify Services**

   ```bash
   # Check decision service
   curl http://localhost:8081/health

   # Check web sessions
   curl http://localhost:3000/health

   # View decision service Swagger docs
   open http://localhost:8081/swagger/
   ```

### Test Workflow

#### Step 1: Create a Simple L4 Program

Create a file `test-eligibility.l4`:

```l4
@export default Check if person is eligible for a program
GIVEN
  age IS A NUMBER @desc The person's age in years
  hasLicense IS A BOOLEAN @desc Whether person has a driver's license
GIVETH A BOOLEAN
isEligible age hasLicense MEANS
  age >= 18 AND hasLicense
```

#### Step 2: Upload to Web Sessions

**Option A: Via Web Interface**

1. Open `http://localhost:3000` in browser
2. Paste the L4 code
3. Click "Save"
4. Note the UUID in the URL (e.g., `http://localhost:3000/session/abc123`)

**Option B: Via API**

```bash
curl -X POST http://localhost:3000/api/sessions \
  -H "Content-Type: application/json" \
  -d @- <<'EOF'
{
  "source": "@export default Check eligibility\nGIVEN age IS A NUMBER, hasLicense IS A BOOLEAN\nGIVETH A BOOLEAN\nisEligible age hasLicense MEANS age >= 18 AND hasLicense"
}
EOF
```

Response will include the UUID:

```json
{
  "uuid": "abc-def-ghi-123",
  "url": "http://localhost:3000/session/abc-def-ghi-123"
}
```

#### Step 3: Verify Auto-Push to Decision Service

Check that the function is available:

```bash
# List all functions
curl http://localhost:8081/functions | jq

# Should see:
# {
#   "functions": [
#     {
#       "name": "abc-def-ghi-123:isEligible",
#       "description": "Check if person is eligible for a program",
#       ...
#     }
#   ]
# }
```

#### Step 4: Call via Decision Service API

```bash
# Get function details
curl http://localhost:8081/functions/abc-def-ghi-123:isEligible | jq

# Evaluate with trace
curl -X POST http://localhost:8081/functions/abc-def-ghi-123:isEligible/evaluation \
  -H "Content-Type: application/json" \
  -H "X-L4-Trace: full" \
  -d @- <<'EOF'
{
  "fnArguments": {
    "age": 25,
    "hasLicense": true
  }
}
EOF

# Expected response:
# {
#   "values": [["result", true]],
#   "reasoning": { ... full trace ... }
# }

# Evaluate without trace (faster, smaller response)
curl -X POST http://localhost:8081/functions/abc-def-ghi-123:isEligible/evaluation \
  -H "Content-Type: application/json" \
  -H "X-L4-Trace: none" \
  -d @- <<'EOF'
{
  "fnArguments": {
    "age": 16,
    "hasLicense": false
  }
}
EOF

# Expected response:
# {
#   "values": [["result", false]],
#   "reasoning": { "payload": { ... empty tree ... } }
# }
```

#### Step 5: Test with Query Parameters

You can also use `?trace=` query parameter:

```bash
# No trace via query param
curl -X POST "http://localhost:8081/functions/abc-def-ghi-123:isEligible/evaluation?trace=none" \
  -H "Content-Type: application/json" \
  -d '{"fnArguments": {"age": 20, "hasLicense": true}}'

# Full trace via query param
curl -X POST "http://localhost:8081/functions/abc-def-ghi-123:isEligible/evaluation?trace=full" \
  -H "Content-Type: application/json" \
  -d '{"fnArguments": {"age": 20, "hasLicense": true}}'
```

**Note:** X-L4-Trace header takes precedence over query parameter if both are present.

### Advanced: Multiple Exports

Test with a file that exports multiple functions:

```l4
@export default Primary eligibility check
GIVEN age IS A NUMBER
GIVETH A BOOLEAN
isAdult age MEANS age >= 18

@export Check if senior citizen
GIVEN age IS A NUMBER
GIVETH A BOOLEAN
isSenior age MEANS age >= 65

@export Check if child
GIVEN age IS A NUMBER
GIVETH A BOOLEAN
isChild age MEANS age < 18
```

All three functions will be available:

- `/functions/{uuid}:isAdult` (default)
- `/functions/{uuid}:isSenior`
- `/functions/{uuid}:isChild`

### Security: Directive Stripping

The web-sessions server automatically strips `#EVAL`, `#ASSERT`, `#CHECK` directives before pushing to the decision service (security feature from PR #649). This prevents code injection.

**Before pushing:**

```l4
GIVEN x IS A NUMBER
myFunc x MEANS x * 2

#EVAL myFunc 5  -- This directive is stripped
```

**After pushing to decision service:**

```l4
GIVEN x IS A NUMBER
myFunc x MEANS x * 2
```

### Troubleshooting

1. **Function not appearing in decision service:**

   - Check web-sessions was started with decision service URL
   - Check decision service logs for errors
   - Verify the L4 file has `@export` annotations

2. **Evaluation returns error:**

   - Check parameter names match exactly
   - Verify types (NUMBER not String, BOOLEAN not Bool)
   - Use `?trace=full` to see detailed error

3. **"More than ONE #EVAL found" error:**
   - This should not happen anymore (fixed in PR #651)
   - If it does, directives weren't properly stripped

---

## Part 2: Mixfix Notation Walkthrough

### Overview

Mixfix notation (PR #647) allows you to define functions with infix, postfix, or complex multi-keyword patterns that read like natural language.

### Key Concept

Instead of:

```l4
isEligibleFor alice healthcare
```

You can write:

```l4
alice `is eligible for` healthcare
```

### How It Works

At **definition site**, parameter names appear in the pattern:

```l4
GIVEN person IS A Person, program IS A Program
GIVETH A BOOLEAN
person `is eligible for` program MEANS
  person's age >= program's minAge
```

The parser recognizes:

- `person` and `program` are GIVEN parameters → argument positions
- `is eligible for` (backticked, not in GIVEN) → function name

At **call site**, use the same pattern:

```l4
alice `is eligible for` healthcare
```

### Pattern Types

#### 1. Infix (Binary)

```l4
GIVEN a IS A NUMBER, b IS A NUMBER
GIVETH A NUMBER
a `plus` b MEANS a + b

-- Call sites:
#EVAL 3 `plus` 5           -- infix syntax
#EVAL `plus` 3 5           -- prefix syntax (still works!)
```

#### 2. Postfix (Unary)

```l4
GIVEN amount IS A NUMBER
GIVETH NUMBER
amount `percent` MEANS amount / 100

-- Call sites:
#EVAL 50 `percent`         -- postfix syntax
#EVAL `percent` 50         -- prefix syntax
```

#### 3. Ternary Mixfix

```l4
GIVEN cond IS A BOOLEAN, thenVal IS A NUMBER, elseVal IS A NUMBER
GIVETH NUMBER
`myif` cond `mythen` thenVal `myelse` elseVal MEANS
  IF cond THEN thenVal ELSE elseVal

-- Call sites:
#EVAL `myif` TRUE `mythen` 42 `myelse` 0           -- full mixfix
#EVAL `myif` TRUE 42 0                             -- prefix (fallback)
#EVAL `myif` (1 = 1) `mythen` (10 + 5) `myelse` 0 -- with sub-expressions
```

#### 4. Complex Multi-Keyword

```l4
GIVEN mummy IS A STRING, daddy IS A STRING, baby IS A STRING, nickname IS A STRING
GIVETH STRING
mummy `and` daddy `had` baby `called` nickname MEANS baby

-- Call site:
#EVAL "Alice" `and` "Bob" `had` "Charlie" `called` "Chuck"
```

### Practical Examples

#### Example 1: Range Checking

```l4
GIVEN lower IS A NUMBER, value IS A NUMBER, upper IS A NUMBER
GIVETH BOOLEAN
lower `<=` value `<=` upper MEANS
  lower <= value AND value <= upper

#EVAL 0 `<=` 5 `<=` 10      -- TRUE
#EVAL 0 `<=` 15 `<=` 10     -- FALSE
```

#### Example 2: Haskell-Style `over` (like `<$>`)

```l4
-- Define infix map operator
GIVEN a IS A TYPE, b IS A TYPE, f IS A FUNCTION FROM a TO b, list IS A LIST OF a
GIVETH A LIST OF b
f `over` list MEANS map f list

-- Helper
GIVEN x IS A NUMBER
GIVETH NUMBER
double x MEANS x * 2

-- Test data
testList MEANS LIST 1, 2, 3, 4, 5

-- Use it:
#EVAL (double) `over` testList           -- LIST 2, 4, 6, 8, 10

-- Chain them:
#EVAL (addOne) `over` ((double) `over` testList)  -- LIST 3, 5, 7, 9, 11
```

### Testing Mixfix

#### Run the Test Suite

```bash
# Test basic mixfix patterns
cabal test --test-show-details=streaming --test-option="--match=/mixfix-basic/"

# Test the 'over' operator
cabal test --test-show-details=streaming --test-option="--match=/mixfix-over/"
```

#### Try it in the CLI

```bash
# Build and run the CLI
cabal run jl4-cli

# Or with a file:
./dist-newstyle/build/.../jl4-cli/jl4-cli eval jl4/examples/ok/mixfix-basic.l4
```

Example session:

```
$ cabal run jl4-cli
> GIVEN a IS A NUMBER, b IS A NUMBER
> GIVETH A NUMBER
> a `plus` b MEANS a + b
>
> #EVAL 3 `plus` 5

Result: 8
```

#### Create Your Own

Create `my-mixfix-test.l4`:

```l4
§ `My Mixfix Test`

-- Natural language eligibility check
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER

DECLARE Program HAS
  programName IS A STRING
  minAge IS A NUMBER

GIVEN person IS A Person, program IS A Program
GIVETH A BOOLEAN
person `is eligible for` program MEANS
  person's age >= program's minAge

-- Test data
alice MEANS Person WITH name IS "Alice", age IS 25
medicare MEANS Program WITH programName IS "Medicare", minAge IS 65
medicaid MEANS Program WITH programName IS "Medicaid", minAge IS 18

-- Test evaluations
#EVAL alice `is eligible for` medicaid   -- TRUE
#EVAL alice `is eligible for` medicare   -- FALSE

-- Also works with prefix!
#EVAL `is eligible for` alice medicaid   -- TRUE
```

Run it:

```bash
./dist-newstyle/build/.../jl4-cli/jl4-cli eval my-mixfix-test.l4
```

### Important Limitations

1. **No Automatic Precedence**

   ```l4
   -- Ambiguous - requires parentheses:
   a `plus` b `times` c

   -- Clear:
   a `plus` (b `times` c)
   (a `plus` b) `times` c
   ```

2. **Presumptive Wrappers Are Always Prefix**

   When TYPICALLY defaults are involved, generated wrappers use prefix names:

   ```l4
   GIVEN age IS A NUMBER TYPICALLY 18
   GIVETH A BOOLEAN
   person `may purchase alcohol` age MEANS age >= 21

   -- In regular code, use mixfix:
   #EVAL alice `may purchase alcohol` 25

   -- But presumptive wrappers use prefix:
   #PEVAL 'presumptive may purchase alcohol' (JUST 25)
   ```

   This ensures the default-handling logic works correctly.

3. **Postfix at End of File**

   Known parser issue: postfix `#EVAL` immediately followed by definitions can cause errors. Put postfix tests at the end of the file:

   ```l4
   -- Definitions first
   GIVEN x IS A NUMBER
   x `squared` MEANS x * x

   -- Postfix tests at end
   #EVAL 5 `squared`
   #EVAL 10 `squared`
   ```

### Integration with Decision Service

Mixfix is for **authoring-time readability**. When functions are exposed via the decision service:

1. **Function names are canonicalized** to prefix form
2. **API calls use prefix names**:

   ```json
   POST /functions/my-module:is_eligible_for/evaluation
   {
     "fnArguments": {
       "person": {"name": "Alice", "age": 25},
       "program": {"programName": "Medicaid", "minAge": 18}
     }
   }
   ```

3. **Mixfix stays in the IDE** for human-friendly authoring

### Debugging Mixfix

If mixfix isn't working as expected:

1. **Check the pattern structure:**

   ```l4
   -- ✓ Correct: parameters from GIVEN in pattern
   GIVEN a IS A NUMBER, b IS A NUMBER
   a `plus` b MEANS a + b

   -- ✗ Wrong: using undefined names
   GIVEN x IS A NUMBER, y IS A NUMBER
   a `plus` b MEANS x + y  -- a and b not in GIVEN!
   ```

2. **Use prefix as fallback:**

   ```l4
   -- If mixfix call isn't parsing:
   alice `is eligible for` healthcare

   -- Try prefix:
   `is eligible for` alice healthcare
   ```

3. **Check type errors:**
   ```bash
   # Run with verbose type-checking
   ./dist-newstyle/build/.../jl4-cli/jl4-cli typecheck my-file.l4
   ```

### Examples to Try

1. **Eligibility checking** (like healthcare/insurance)
2. **Mathematical relations** (ranges, comparisons)
3. **Temporal logic** (`from X to Y`, `within N days`)
4. **Functional operators** (`over`, `then`, `compose`)
5. **Business rules** (`qualifies for`, `is subject to`)

---

## Quick Reference: Testing Checklist

### Web-Sessions Integration

- [ ] Decision service running on :8081
- [ ] Web sessions running on :3000 with DS URL
- [ ] Create L4 file with `@export`
- [ ] Save in web editor, note UUID
- [ ] Verify appears in `/functions`
- [ ] Call via REST API with JSON
- [ ] Test with `X-L4-Trace: none` and `full`
- [ ] Test with nested objects
- [ ] Test with multiple exports

### Mixfix Notation

- [ ] Read `doc/mixfix-operators.md`
- [ ] Study `jl4/examples/ok/mixfix-basic.l4`
- [ ] Try infix pattern: `a 'op' b`
- [ ] Try postfix pattern: `x 'op'`
- [ ] Try ternary pattern: `'if' a 'then' b 'else' c`
- [ ] Test prefix fallback still works
- [ ] Check with nested expressions
- [ ] Verify type-directed matching
- [ ] Run test suite
- [ ] Create custom mixfix function

---

## Next Steps

1. **Test web-sessions → decision service flow** with your own L4 programs
2. **Experiment with mixfix** to make your code more readable
3. **Combine both**: Create mixfix functions, save to web editor, call via API
4. **Report issues** you find on GitHub

For questions or issues, refer to:

- [`PROJECT-MASTER.md`](doc/todo/PROJECT-MASTER.md) - Overall project status
- [`ISSUE-635-PLANNING-STATUS.md`](doc/todo/ISSUE-635-PLANNING-STATUS.md) - Decision service work
- [`mixfix-operators.md`](doc/mixfix-operators.md) - Mixfix specification
- Swagger docs at `http://localhost:8081/swagger/`
