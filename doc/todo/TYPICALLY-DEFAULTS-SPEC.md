# Specification: TYPICALLY Keyword for Default Values

**Status:** 📋 Draft
**Related:** `doc/default-values.md` (conceptual background), `BOOLEAN-MINIMIZATION-SPEC.md`

## Executive Summary

Add a `TYPICALLY` keyword to L4 that allows specifying default values for:
1. **DECLARE fields** - default values for record type fields
2. **GIVEN parameters** - default values for function parameters
3. **ASSUME declarations** - default values for assumed external values

These defaults represent **rebuttable presumptions** - they hold unless explicitly overridden by user input.

## Motivation

### Legal Domain: Rebuttable Presumptions

In legal reasoning, many terms carry implicit assumptions:

- A "person" is typically a natural person with mental capacity
- A contract party is typically not under duress
- A transaction is typically at arm's length
- A taxpayer is typically a resident

These are **rebuttable presumptions** - defaults that apply unless proven otherwise. L4 should support this pattern natively.

### UX: Reducing Question Burden

In interactive applications (chatbots, web forms), asking every possible question is tedious. With TYPICALLY:

```
Without TYPICALLY:
  Chatbot: Are you married?
  Chatbot: Do you have spousal approval?
  Chatbot: Are you buying beer only?
  Chatbot: Do you have parental approval?
  Chatbot: Are you legally emancipated?

With TYPICALLY (married TYPICALLY FALSE, etc.):
  Chatbot: How old are you?
  User: 30
  Chatbot: You can purchase alcohol! (assumed unmarried based on typical case)
```

### Formal Verification: Explicit Assumptions

When verifying contracts, TYPICALLY values make assumptions explicit and auditable:

```
Result: TRUE
Assumptions used:
  - has_capacity = TRUE (TYPICALLY)
  - is_under_duress = FALSE (TYPICALLY)
  - is_natural_person = TRUE (TYPICALLY)
```

## Syntax Design

### In DECLARE (Record Type Fields)

```l4
DECLARE Person HAS
  name IS A STRING
  age IS A NUMBER
  has_capacity IS A BOOLEAN TYPICALLY TRUE
  is_natural_person IS A BOOLEAN TYPICALLY TRUE
  is_under_duress IS A BOOLEAN TYPICALLY FALSE
  jurisdiction IS A STRING TYPICALLY "default"
```

### In GIVEN (Function Parameters)

```l4
GIVEN
  buyer IS A Person
  age IS A NUMBER
  married IS A BOOLEAN TYPICALLY FALSE
  has_spousal_approval IS A BOOLEAN TYPICALLY FALSE
  beer_only IS A BOOLEAN TYPICALLY FALSE
  has_parental_approval IS A BOOLEAN TYPICALLY FALSE
  is_emancipated IS A BOOLEAN TYPICALLY FALSE
GIVETH A BOOLEAN
DECIDE `may purchase alcohol` IF ...
```

### In ASSUME (External Declarations)

```l4
ASSUME `person has capacity` IS A BOOLEAN TYPICALLY TRUE
ASSUME `transaction is at arms length` IS A BOOLEAN TYPICALLY TRUE
ASSUME `applicable law` IS A STRING TYPICALLY "Singapore"
```

### Grammar Extension

```ebnf
TypeAnnotation ::= "IS" Article? Type TypicallyClause?

TypicallyClause ::= "TYPICALLY" Literal

Literal ::= BoolLiteral | NumberLiteral | StringLiteral

BoolLiteral ::= "TRUE" | "FALSE"
```

## Semantics

### Three-State Value Model

Each value exists in one of three states:

```haskell
data ValueState a
  = Explicit a        -- User provided this value
  | Default a         -- Using TYPICALLY value (no user input)
  | Unknown           -- No TYPICALLY and no user input
```

### Evaluation Priority

When evaluating a parameter:

1. **Explicit value** (user provided) → use it
2. **No explicit value, has TYPICALLY** → use TYPICALLY value
3. **No explicit value, no TYPICALLY** → Unknown (three-valued logic)

```haskell
resolve :: Maybe a -> Maybe a -> ValueState a
resolve (Just explicit) _           = Explicit explicit
resolve Nothing        (Just deflt) = Default deflt
resolve Nothing        Nothing      = Unknown
```

### In Three-Valued Logic

TYPICALLY values integrate with Kleene three-valued logic:

| Input State | TYPICALLY | Resolved Value |
|-------------|-----------|----------------|
| Explicit TRUE | any | TTrue (Explicit) |
| Explicit FALSE | any | TFalse (Explicit) |
| null / omitted | TRUE | TTrue (Default) |
| null / omitted | FALSE | TFalse (Default) |
| null / omitted | none | TUnknown |

### Audit Trail

The evaluation trace must distinguish default values from explicit ones:

```json
{
  "result": true,
  "inputs": {
    "age": { "value": 30, "source": "explicit" },
    "married": { "value": false, "source": "default", "typically": "FALSE" },
    "has_capacity": { "value": true, "source": "default", "typically": "TRUE" }
  },
  "assumptions_used": [
    { "param": "married", "assumed": false, "reason": "TYPICALLY FALSE" },
    { "param": "has_capacity", "assumed": true, "reason": "TYPICALLY TRUE" }
  ]
}
```

## Implementation Plan

### Phase 1: Lexer & Parser

**File:** `jl4-core/src/L4/Lexer.hs`

Add TYPICALLY keyword:

```haskell
keywords :: Map Text TKeywords
keywords = Map.fromList
  [ ...
  , ("TYPICALLY", TKTypically)
  ...
  ]
```

**File:** `jl4-core/src/L4/Parser.hs`

Extend type annotation parsing:

```haskell
parseTypicallyClause :: Parser (Maybe (Expr Name))
parseTypicallyClause = optional $ do
  keyword TKTypically
  parseLiteral

parseTypeAnnotation :: Parser (Type' Name, Maybe (Expr Name))
parseTypeAnnotation = do
  keyword TKIs
  optional (keyword TKA <|> keyword TKAn)
  ty <- parseType
  typically <- parseTypicallyClause
  pure (ty, typically)
```

### Phase 2: AST Extension

**File:** `jl4-core/src/L4/Syntax.hs`

Extend relevant AST nodes:

```haskell
-- For DECLARE fields
data TypedName n = MkTypedName
  { tnAnno :: Anno
  , tnName :: n
  , tnType :: Type' n
  , tnTypically :: Maybe (Expr n)  -- NEW: default value
  }

-- For GIVEN parameters (in TypeSig)
data TypeSigItem n = MkTypeSigItem
  { tsiAnno :: Anno
  , tsiName :: n
  , tsiType :: Type' n
  , tsiTypically :: Maybe (Expr n)  -- NEW: default value
  }

-- For ASSUME declarations
data Assume n = MkAssume
  { assumeAnno :: Anno
  , assumeTypeSig :: TypeSig n
  , assumeAppForm :: AppForm n
  , assumeType :: Maybe (Type' n)
  , assumeTypically :: Maybe (Expr n)  -- NEW: default value
  }
```

### Phase 3: Type Checking

**File:** `jl4-core/src/L4/TypeCheck.hs`

Validate TYPICALLY expressions:

```haskell
checkTypically :: Type' Resolved -> Maybe (Expr Name) -> TC (Maybe (Expr Resolved))
checkTypically _ Nothing = pure Nothing
checkTypically expectedTy (Just expr) = do
  (expr', actualTy) <- inferExpr expr
  unify expectedTy actualTy
  -- Ensure it's a literal (compile-time constant)
  unless (isLiteral expr') $
    throwError "TYPICALLY value must be a literal"
  pure (Just expr')
```

### Phase 4: Evaluation Integration

**File:** `jl4-core/src/L4/EvaluateLazy.hs`

Modify parameter binding to use defaults:

```haskell
bindParameter :: TypeSigItem Resolved -> Maybe Value -> Machine Value
bindParameter tsi maybeVal = case maybeVal of
  Just v  -> pure v  -- Explicit value provided
  Nothing -> case tsiTypically tsi of
    Just defaultExpr -> do
      v <- eval defaultExpr
      markAsDefault (tsiName tsi) v  -- Track for audit
      pure v
    Nothing -> pure VUnknown  -- No default, truly unknown
```

**File:** `jl4-core/src/L4/EvaluateLazy/Trace.hs`

Track default value usage in trace:

```haskell
data ValueSource = Explicit | Default | Unknown
  deriving (Eq, Show)

data TracedValue = TracedValue
  { tvValue :: Value
  , tvSource :: ValueSource
  , tvTypically :: Maybe Value  -- The TYPICALLY value if used
  }
```

### Phase 5: Decision Service Integration

**File:** `jl4-decision-service/src/Backend/Api.hs`

Extend API response to include default information:

```haskell
data EvalResponse = EvalResponse
  { result :: FnLiteral
  , reasoning :: Maybe ReasoningTree
  , defaultsUsed :: [DefaultUsage]  -- NEW
  }

data DefaultUsage = DefaultUsage
  { paramName :: Text
  , defaultValue :: FnLiteral
  , reason :: Text  -- e.g., "TYPICALLY FALSE"
  }
```

### Phase 6: IDE Support

**File:** `jl4-lsp/src/...`

- Syntax highlighting for TYPICALLY keyword
- Hover information showing default values
- Autocomplete suggesting TYPICALLY after type annotations
- Diagnostics for type mismatches in TYPICALLY values

## Test Cases

### Unit Tests: Parser

```haskell
describe "TYPICALLY parsing" $ do
  it "parses TYPICALLY TRUE in DECLARE" $ do
    parse "DECLARE Foo HAS x IS A BOOLEAN TYPICALLY TRUE"
      `shouldParseTo` Declare ... (Just (Lit True))

  it "parses TYPICALLY FALSE in GIVEN" $ do
    parse "GIVEN x IS A BOOLEAN TYPICALLY FALSE GIVETH ..."
      `shouldParseTo` ...

  it "parses TYPICALLY with numbers" $ do
    parse "DECLARE Foo HAS age IS A NUMBER TYPICALLY 18"
      `shouldParseTo` ...

  it "parses TYPICALLY with strings" $ do
    parse "ASSUME jurisdiction IS A STRING TYPICALLY \"Singapore\""
      `shouldParseTo` ...

  it "rejects TYPICALLY with non-literals" $ do
    parse "DECLARE Foo HAS x IS A BOOLEAN TYPICALLY (a AND b)"
      `shouldFailWith` "TYPICALLY value must be a literal"
```

### Unit Tests: Type Checking

```haskell
describe "TYPICALLY type checking" $ do
  it "accepts matching types" $ do
    typeCheck "DECLARE Foo HAS x IS A BOOLEAN TYPICALLY TRUE"
      `shouldSucceed`

  it "rejects mismatched types" $ do
    typeCheck "DECLARE Foo HAS x IS A BOOLEAN TYPICALLY 42"
      `shouldFailWith` "Expected BOOLEAN, got NUMBER"

  it "rejects non-literal expressions" $ do
    typeCheck "GIVEN x IS A BOOLEAN TYPICALLY (y AND z) ..."
      `shouldFailWith` "TYPICALLY value must be a literal"
```

### Unit Tests: Evaluation

```haskell
describe "TYPICALLY evaluation" $ do
  let func = parseAndCheck [l4|
    GIVEN
      x IS A BOOLEAN TYPICALLY TRUE
      y IS A BOOLEAN TYPICALLY FALSE
    GIVETH A BOOLEAN
    DECIDE foo IF x AND NOT y
    |]

  it "uses defaults when no input provided" $ do
    eval func Map.empty `shouldBe` Right True

  it "explicit value overrides default" $ do
    eval func (Map.singleton "x" False) `shouldBe` Right False

  it "tracks which defaults were used" $ do
    (_, trace) <- evalWithTrace func Map.empty
    defaultsUsed trace `shouldContain`
      [("x", True, "TYPICALLY TRUE"), ("y", False, "TYPICALLY FALSE")]
```

### Integration Tests: Decision Service

```haskell
describe "TYPICALLY in decision service" $ do
  it "returns defaults used in response" $ do
    resp <- postEval "/functions/foo/evaluation" "{}"
    resp.defaultsUsed `shouldContain`
      [DefaultUsage "x" (FnLitBool True) "TYPICALLY TRUE"]

  it "explicit input not listed as default" $ do
    resp <- postEval "/functions/foo/evaluation" "{\"x\": false}"
    resp.defaultsUsed `shouldNotContain` "x"
```

### Golden Tests

```haskell
describe "TYPICALLY golden tests" $ do
  it "person_with_defaults" $
    goldenEval "person_with_defaults" [l4|
      DECLARE Person HAS
        has_capacity IS A BOOLEAN TYPICALLY TRUE
        is_under_duress IS A BOOLEAN TYPICALLY FALSE

      GIVEN p IS A Person
      GIVETH A BOOLEAN
      DECIDE `can contract` p IF
        p's has_capacity AND NOT p's is_under_duress
      |]
      (Map.singleton "p" (FnObject []))  -- empty person, uses defaults
```

## Edge Cases

### 1. TYPICALLY on Optional Fields

What if a field is `MAYBE T` with TYPICALLY?

```l4
DECLARE Contract HAS
  arbitration_clause IS A MAYBE STRING TYPICALLY NOTHING
```

This means "by default, no arbitration clause" - the TYPICALLY value is `NOTHING`, not a default string.

### 2. TYPICALLY with Complex Types

TYPICALLY should only accept literals, not complex expressions:

```l4
-- OK: literal
DECLARE Foo HAS x IS A NUMBER TYPICALLY 42

-- ERROR: not a literal
DECLARE Foo HAS x IS A NUMBER TYPICALLY (40 + 2)

-- ERROR: not a literal
DECLARE Foo HAS items IS A LIST OF NUMBER TYPICALLY [1, 2, 3]
```

For complex defaults, use a separate definition:

```l4
defaultItems MEANS [1, 2, 3]

GIVEN items IS A LIST OF NUMBER  -- no TYPICALLY, handled in logic
DECIDE foo IF ...
  WHERE items' = IF items == [] THEN defaultItems ELSE items
```

### 3. Nested Record Defaults

When a record field has a record type with its own defaults:

```l4
DECLARE Address HAS
  country IS A STRING TYPICALLY "Singapore"

DECLARE Person HAS
  name IS A STRING
  address IS AN Address  -- No TYPICALLY on the whole address
```

If `Person.address` is provided but `Person.address.country` is omitted, should country default to "Singapore"?

**Recommendation:** Yes, defaults apply at each level independently.

### 4. TYPICALLY and Partial Evaluation

TYPICALLY integrates with Boolean Minimization (see `BOOLEAN-MINIMIZATION-SPEC.md`):

- With TYPICALLY, omitted fields resolve to default values (not Unknown)
- This reduces the number of questions the chatbot needs to ask
- User can explicitly provide `null` to indicate "I don't know" (overrides TYPICALLY)

### 5. Conflicting Defaults

What if DECLARE and GIVEN both specify TYPICALLY for the same concept?

```l4
DECLARE Person HAS
  age IS A NUMBER TYPICALLY 30

GIVEN p IS A Person
      age IS A NUMBER TYPICALLY 21  -- Different default!
DECIDE ...
```

**Rule:** The innermost (most specific) TYPICALLY wins. Here, the GIVEN's TYPICALLY 21 overrides the DECLARE's TYPICALLY 30 for this function.

## Migration Path

### Backward Compatibility

TYPICALLY is purely additive - existing L4 code without TYPICALLY continues to work exactly as before.

### Incremental Adoption

Teams can add TYPICALLY to their codebase incrementally:

1. Start with most commonly assumed values
2. Add TYPICALLY to ASSUME declarations for external facts
3. Add TYPICALLY to GIVEN for function parameters
4. Add TYPICALLY to DECLARE for record types

## Related Work

- **Prolog's Closed World Assumption:** Unknown facts are assumed false
- **SQL DEFAULT:** Column defaults in database schemas
- **TypeScript's Optional with Default:** `function foo(x: number = 42)`
- **Haskell's Default class:** Type class for default values
- **Legal Presumptions:** Rebuttable presumptions in evidence law

## References

- `doc/default-values.md`: Original conceptual design
- `BOOLEAN-MINIMIZATION-SPEC.md`: Integration with partial evaluation
- [Negation as Failure](https://en.wikipedia.org/wiki/Negation_as_failure)
- [Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption)
- [Default Logic](https://en.wikipedia.org/wiki/Default_logic)
