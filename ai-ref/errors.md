# Common L4 Errors and Fixes

Quick diagnostic patterns for frequent errors.

---

## Parse Error: Unexpected THEN

**Symptom:** `Parse error: unexpected THEN at line N`

**Cause:** Fragile indentation in nested IF/THEN/ELSE

**Example (wrong):**
```l4
result MEANS
    IF a THEN 1
    ELSE IF b THEN 2
         ELSE IF c THEN 3
              ELSE 4
```

**Fix:** Use BRANCH
```l4
result MEANS
    BRANCH
        IF a THEN 1
        IF b THEN 2
        IF c THEN 3
        OTHERWISE 4
```

---

## Missing ELSE Branch

**Symptom:** `Parse error: expected ELSE after THEN`

**Cause:** IF without ELSE (not a statement, must be expression)

**Wrong:** `result MEANS IF condition THEN value`

**Fix:** `result MEANS IF condition THEN value ELSE default`

**Alternative:** Use guard in parent context or provide default

---

## Type Mismatch in Branches

**Symptom:** `Type error: branch returns T1 but expected T2`

**Cause:** BRANCH/IF/CONSIDER branches have different types

**Wrong:**
```l4
result MEANS
    BRANCH
        IF x < 0 THEN "negative"
        IF x = 0 THEN 0  -- NUMBER, not STRING
        OTHERWISE "positive"
```

**Fix:** Make all branches same type
```l4
result MEANS
    BRANCH
        IF x < 0 THEN "negative"
        IF x = 0 THEN "zero"
        OTHERWISE "positive"
```

---

## Undefined Field Access

**Symptom:** `Error: record type T has no field 'fieldname'`

**Cause:** Accessing non-existent field or typo

**Fix:** Check field name spelling, check DECLARE definition

---

## Module Not Found

**Symptom:** `Error: cannot resolve import 'modulename'`

**Cause:** Library not in search path

**Search order:**
1. VFS (virtual filesystem)
2. Relative to current file
3. XDG directory (~/.local/share/jl4/libraries/)
4. Bundled libraries

**Fix:**
- Check spelling
- Install to ~/.local/share/jl4/libraries/
- Use relative path: `IMPORT ./localfile`

---

## Function Arity Mismatch

**Symptom:** `Error: function expects N arguments, got M`

**Cause:** Wrong number of arguments in application

**Wrong:** `f x y` when `f` expects 3 arguments

**Fix:** Provide all arguments or use partial application intentionally

---

## Pattern Match Not Exhaustive

**Symptom:** `Warning: pattern match may not be exhaustive`

**Cause:** Missing cases in CONSIDER

**Fix:** Add OTHERWISE branch or cover all constructors

```l4
CONSIDER status
WHEN Active THEN "running"
WHEN Closed THEN "stopped"
OTHERWISE "unknown"  -- Catches Suspended
```

---

## Parentheses Required for Field Access

**Symptom:** Parse error when using field access in function call

**Wrong:**
```l4
all (GIVEN g YIELD g's age >= 18) charity's governors
```

**Fix:** Wrap field access in parens
```l4
all (GIVEN g YIELD g's age >= 18) (charity's governors)
```

---

## Boolean Literal Case

**Symptom:** `Parse error: unknown identifier 'True'`

**Cause:** Wrong case for boolean literals

**L4 uses:** `TRUE` and `FALSE` (uppercase)

**Wrong:** `True`, `true`, `False`, `false`

**Fix:** `TRUE`, `FALSE`

---

## Equality Operator

**L4 uses:** `=` for equality (not `==`)

**Wrong:** `x == 5`

**Fix:** `x = 5`

---

## Circular Definition

**Symptom:** Evaluation hangs or stack overflow

**Cause:** Definition references itself without base case

**Wrong:**
```l4
x MEANS x + 1
```

**Fix:** Ensure recursion has base case
```l4
factorial MEANS
    IF n <= 1 THEN 1  -- Base case
    ELSE n * (factorial (n - 1))
```

---

## Missing IMPORT prelude

**Symptom:** `Error: undefined function 'map'` (or filter, all, any, etc.)

**Cause:** Prelude functions not imported

**Fix:** Add `IMPORT prelude` at top of file

---

## WHERE Scope Issue

**Symptom:** `Error: undefined variable in WHERE clause`

**Cause:** WHERE bindings can't see outer scope or wrong order

**Note:** WHERE bindings CAN reference outer scope and each other

**Check:** Ensure referenced variables exist in scope

---

## DEONTIC Outside Trace

**Symptom:** DEONTIC rule doesn't execute

**Cause:** DEONTIC is effect type, needs #TRACE to evaluate

**Fix:** Use `#TRACE ruleFunction context` to execute and see obligations

---

## Indentation Sensitivity

**General:** L4 is layout-sensitive (like Python/Haskell)

**Rules:**
- Nested blocks must be consistently indented
- Use spaces, not tabs
- WHERE bindings align vertically

**Tip:** Use editor with layout mode support (VS Code extension)

---

## Type Annotation Clarity

**Best practice:** Include GIVEN...GIVETH (or GIVES) for top-level functions

**Optional:** Type inference works without annotations

**Benefits of explicit types:**
- Self-documentation
- Better error messages
- Type checking clarity
- Prevents inference ambiguity

**Linter:** May flag missing type annotations as yellow warning (not error)

---

## Search Terms

Parse error, type mismatch, pattern match, indentation error, scope error, recursion, stack overflow, import error, module not found, field access, arity mismatch, boolean literal, equality operator
