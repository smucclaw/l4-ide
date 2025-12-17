# LET...IN Expression Specification

**Status:** Proposed (Research Complete)
**Author:** Meng Wong, with Claude (Opus 4.5)
**Date:** 2025-12-17

## Overview

Add Haskell-style `LET...IN` expressions to L4. Unlike `WHERE` (which attaches to declarations), `LET...IN` is a standalone expression that can appear anywhere in expression context.

## Syntax

```
LET
    <binding>
    <binding>
    ...
IN <expr>
```

### Binding Keywords

Four synonymous keywords are accepted for bindings within LET blocks:

- `IS`
- `BE`
- `MEAN`
- `MEANS`

These synonyms are scoped to LET...IN only; they do not affect WHERE or DECIDE syntax.

### Examples

```
LET
    x IS 5
    y BE x PLUS 1
    z MEAN y TIMES 2
    w MEANS z MINUS 3
IN x TIMES y TIMES z TIMES w
```

**With @desc annotations for inline documentation:**

```
LET
    x IS 5 @desc x is the loneliest number
    y BE x PLUS 1 @desc y follows x
    z MEAN y TIMES 2 @desc z doubles down
IN x TIMES y TIMES z
```

Nested in expression context:

```
DECIDE foo IS
    (LET temp BE expensive_computation IN temp PLUS temp)
    TIMES
    (LET factor MEAN 10 IN factor)
```

## Semantics

### Recursive Bindings (Haskell-style)

LET...IN uses the same recursive binding semantics as WHERE:

- **Forward references allowed:** A binding can reference bindings defined later in the same LET block
- **Mutual recursion supported:** Bindings can reference each other

```
LET
    even n IS IF n EQUALS 0 THEN True  ELSE odd  (n MINUS 1)
    odd  n IS IF n EQUALS 0 THEN False ELSE even (n MINUS 1)
IN even 10
```

### Lazy Evaluation with Sharing

Bindings are evaluated lazily with memoization:

- Each binding starts as an unevaluated thunk
- On first access, the thunk evaluates and caches the result (WHNF)
- Subsequent accesses return the cached result
- Multiple references to the same binding do NOT cause re-evaluation

This is achieved via the same `evalRecLocalDecls` mechanism used by WHERE.

---

## Design Decision: Why Not Desugar to WHERE?

We considered a simpler implementation: parse `LET...IN` and immediately desugar it to `WHERE` in the parser, avoiding changes to downstream phases. This approach was rejected for two reasons:

### 1. Syntactic Position Incompatibility

WHERE is a **postfix** operator attached to declarations:

```
DECIDE foo IS expr WHERE bindings
```

LET...IN is a **prefix** standalone expression:

```
(LET bindings IN expr) TIMES other_expr
```

The parser only accepts WHERE in specific positions (after DECIDE/function definitions), not as a general subexpression. Even though `Where` exists as an `Expr` constructor internally, it cannot appear in arbitrary expression contexts syntactically.

### 2. Scope Flattening Breaks Shadowing

We also considered "hoisting" LET bindings to the enclosing function's WHERE clause. This breaks when multiple LET expressions use the same variable name:

```
DECIDE foo x IS
    (LET temp BE expensive IN temp PLUS temp)
    TIMES
    (LET temp BE cheap IN temp MINUS 1)   -- same name, different scope!
```

Hoisting would merge into one flat scope:

```
DECIDE foo x IS (temp PLUS temp) TIMES (temp MINUS 1)
WHERE
    temp IS expensive   -- collision!
    temp IS cheap       -- which temp wins?
```

Each `LET...IN` must create its **own lexical scope** - that's the fundamental feature. Alpha-renaming would require a mini-compiler pass more complex than just implementing `LetIn` properly.

**Conclusion:** Full implementation with a new `LetIn` AST constructor is required.

---

## Codebase Research Findings

The following was verified by examining the actual codebase:

### Lexer (`jl4-core/src/L4/Lexer.hs`)

**Existing tokens (lines 130-202):**

- `TKWhere` exists (line 157)
- `TKIs` exists (line 147)
- `TKMeans` exists (line 145)

**Keywords map (lines 204-276):**

```haskell
keywords :: Map Text TKeywords
keywords = Map.fromList
  [ ("WHERE"      , TKWhere      )
  , ("IS"         , TKIs         )
  , ("MEANS"      , TKMeans      )
  ...
  ]
```

**Need to add:** `TKLet`, `TKIn`, `TKBe`, `TKMean`

### AST (`jl4-core/src/L4/Syntax.hs`)

**Expr data type (lines 194-236):**

```haskell
data Expr n =
    And        Anno (Expr n) (Expr n)
    ...
  | Where      Anno (Expr n) [LocalDecl n]    -- line 228
    ...
```

**LocalDecl type (lines 325-329):**

```haskell
data LocalDecl n =
    LocalDecide Anno (Decide n)
  | LocalAssume Anno (Assume n)
```

LET bindings will wrap `Decide` values in `LocalDecide`.

### Parser (`jl4-core/src/L4/Parser.hs`)

**localdecl (lines 448-453):**

```haskell
localdecl :: Parser (LocalDecl Name)
localdecl =
  withTypeSig (\ sig -> attachAnno $
        LocalDecide    emptyAnno <$> annoHole (decide sig)
    <|> LocalAssume    emptyAnno <$> annoHole (assume sig)
  )
```

**whereExpr (lines 783-788):** - This is a **postfix** operator returning `Expr -> Expr`:

```haskell
whereExpr :: Pos -> Parser (Expr Name -> Expr Name)
whereExpr p =
  withIndent GT p $ \ _ -> do
    ann <- opToken $ TKeywords TKWhere
    ds <- many (indented localdecl p)
    pure (\ e -> Where (mkHoleAnnoFor e <> ann <> mkHoleAnnoFor ds) e ds)
```

**Key insight:** `letInExpr` must be a **prefix** expression (added to `baseExpr'`), not a postfix continuation like `whereExpr`.

**Important:** `localdecl` uses the `decide` parser internally, which expects DECIDE/MEANS syntax. We cannot reuse it for LET bindings with IS/BE/MEAN/MEANS keywords. A separate `letLocalDecl` parser is required.

### TypeChecker (`jl4-core/src/L4/TypeCheck.hs`)

**checkExpr for WHERE (lines 1001-1014):**

```haskell
checkExpr ec (Where ann e ds) t = softprune $ do
  let
    preScanDecl = mapMaybeM scanTyDeclLocalDecl
    scanDecl = mapMaybeM inferTyDeclLocalDecl
    scanFuns = mapMaybeM scanFunSigLocalDecl

  (rds, extends) <- withScanTypeAndSigEnvironment preScanDecl scanDecl scanFuns ds do
     unzip <$> traverse (firstM nlgLocalDecl <=< inferLocalDecl) ds
  re <- extendKnownMany (concat extends) do
    re <- checkExpr ec e t
    nlgExpr re
  setAnnResolvedType t Nothing (Where ann re rds)
```

**inferExpr for WHERE (lines 1275-1284):** follows same pattern.

### Evaluator (`jl4-core/src/L4/EvaluateLazy/Machine.hs`)

**forwardExpr for WHERE (lines 369-372):**

```haskell
Where _ann e ds -> do
  env' <- evalRecLocalDecls env ds
  let combinedEnv = Map.union env' env
  ForwardExpr combinedEnv e
```

**evalRecLocalDecls (lines 2034-2040):** handles recursive binding allocation and evaluation.

### Pretty Printer (`jl4-core/src/L4/Print.hs`)

**printWithLayout for WHERE (lines 314-319):**

```haskell
Where      _ e1 decls ->
  vcat
    [ indent 2 (printWithLayout e1)
    , "WHERE"
    , indent 2 (vsep $ fmap printWithLayout decls)
    ]
```

### Desugarer (`jl4-core/src/L4/Desugar.hs`)

**carameliseExpr for WHERE (line 53):**

```haskell
Where      ann e ds -> Where ann (carameliseExpr e) (fmap carameliseLocalDecl ds)
```

---

## Implementation Plan

### Phase 1: Lexer (`jl4-core/src/L4/Lexer.hs`)

Add to `TKeywords` data type (around line 157):

```haskell
  | TKLet
  | TKIn
  | TKBe
  | TKMean
```

Add to `keywords` map (around line 240):

```haskell
  , ("LET"        , TKLet        )
  , ("IN"         , TKIn         )
  , ("BE"         , TKBe         )
  , ("MEAN"       , TKMean       )
```

Note: `TKIs` and `TKMeans` already exist.

### Phase 2: AST (`jl4-core/src/L4/Syntax.hs`)

Add new constructor to `Expr` (after `Where` at line 228):

```haskell
  | LetIn      Anno [LocalDecl n] (Expr n)
```

Note: Order is bindings-first (matching source text), unlike `Where` which is body-first.

### Phase 3: Parser (`jl4-core/src/L4/Parser.hs`)

**3a. Add binding keyword helper:**

```haskell
-- | Keywords accepted for bindings in LET...IN blocks
letBindingKeyword :: Parser ()
letBindingKeyword = spacedKeyword_ TKIs
                <|> spacedKeyword_ TKBe
                <|> spacedKeyword_ TKMean
                <|> spacedKeyword_ TKMeans
```

**3b. Add letLocalDecl parser** (cannot reuse `localdecl` - see research findings):

```haskell
-- | Parse a local declaration in a LET block: "name IS/BE/MEAN/MEANS expr"
letLocalDecl :: Parser (LocalDecl Name)
letLocalDecl = attachAnno $ do
  name <- annoHole nameP
  letBindingKeyword
  body <- annoHole mixfixChainExpr
  pure $ LocalDecide emptyAnno (Decide emptyAnno name [] body)
```

Note: This creates a `Decide` with no parameters (simple binding). Function bindings with parameters (`f x IS ...`) need additional handling if desired.

**3c. Add LET...IN expression parser:**

```haskell
letInExpr :: Parser (Expr Name)
letInExpr = do
  current <- Lexer.indentLevel
  attachAnno $
    LetIn emptyAnno
      <$  annoLexeme (spacedKeyword_ TKLet)
      <*> annoHole (many (indented letLocalDecl current))
      <*  annoLexeme (spacedKeyword_ TKIn)
      <*> annoHole (indentedExpr current)
```

**3d. Add to baseExpr' alternatives** (around line 680):

```haskell
baseExpr' = ...
        <|> letInExpr
        <|> ...
```

### Phase 4: Type Checker (`jl4-core/src/L4/TypeCheck.hs`)

**4a. Add case for `LetIn` in `checkExpr`** (after WHERE case, ~line 1014):

```haskell
checkExpr ec (LetIn ann ds e) t = softprune $ do
  let
    preScanDecl = mapMaybeM scanTyDeclLocalDecl
    scanDecl = mapMaybeM inferTyDeclLocalDecl
    scanFuns = mapMaybeM scanFunSigLocalDecl
  (rds, extends) <- withScanTypeAndSigEnvironment preScanDecl scanDecl scanFuns ds do
     unzip <$> traverse (firstM nlgLocalDecl <=< inferLocalDecl) ds
  re <- extendKnownMany (concat extends) do
    re <- checkExpr ec e t
    nlgExpr re
  setAnnResolvedType t Nothing (LetIn ann rds re)
```

**4b. Add case for `LetIn` in `inferExpr`** (after WHERE case, ~line 1284):

```haskell
LetIn ann ds e -> do
  let
    preScanDecl = mapMaybeM scanTyDeclLocalDecl
    scanDecl = mapMaybeM inferTyDeclLocalDecl
    scanFuns = mapMaybeM scanFunSigLocalDecl
  (rds, extends) <- withScanTypeAndSigEnvironment preScanDecl scanDecl scanFuns ds do
    unzip <$> traverse inferLocalDecl ds
  (re, t) <- extendKnownMany (concat extends) $ inferExpr e
  pure (LetIn ann rds re, t)
```

### Phase 5: Evaluator (`jl4-core/src/L4/EvaluateLazy/Machine.hs`)

Add case for `LetIn` in `forwardExpr` (after WHERE case, ~line 372):

```haskell
LetIn _ann ds e -> do
    env' <- evalRecLocalDecls env ds
    let combinedEnv = Map.union env' env
    ForwardExpr combinedEnv e
```

This is identical to the WHERE case, reusing the same recursive binding mechanism.

### Phase 6: Pretty Printer (`jl4-core/src/L4/Print.hs`)

Add case for `LetIn` in `printWithLayout` (after WHERE case, ~line 319):

```haskell
LetIn _ decls e1 ->
  vcat
    [ "LET"
    , indent 2 (vsep $ fmap printWithLayout decls)
    , "IN"
    , indent 2 (printWithLayout e1)
    ]
```

### Phase 7: Desugarer (`jl4-core/src/L4/Desugar.hs`)

Add case for `LetIn` in `carameliseExpr` (after WHERE case, ~line 53):

```haskell
LetIn ann ds e -> LetIn ann (fmap carameliseLocalDecl ds) (carameliseExpr e)
```

### Phase 8: Other Passes

Search for all pattern matches on `Expr` constructors and add `LetIn` cases:

```bash
grep -r "Where.*ann.*e.*ds" jl4-core/src/L4/
```

Files to check:

- `L4/ExactPrint.hs`
- `L4/Annotation.hs`
- Any other expression traversals

---

## Files to Modify

| File                                      | Changes                                                                 |
| ----------------------------------------- | ----------------------------------------------------------------------- |
| `jl4-core/src/L4/Lexer.hs`                | Add `TKLet`, `TKIn`, `TKBe`, `TKMean` to `TKeywords` and `keywords` map |
| `jl4-core/src/L4/Syntax.hs`               | Add `LetIn` constructor to `Expr`                                       |
| `jl4-core/src/L4/Parser.hs`               | Add `letInExpr`, `letLocalDecl`, `letBindingKeyword`                    |
| `jl4-core/src/L4/TypeCheck.hs`            | Add `checkExpr` and `inferExpr` cases                                   |
| `jl4-core/src/L4/EvaluateLazy/Machine.hs` | Add `forwardExpr` case                                                  |
| `jl4-core/src/L4/Print.hs`                | Add pretty-printer case                                                 |
| `jl4-core/src/L4/Desugar.hs`              | Add desugarer case                                                      |

---

## Comparison: LET...IN vs WHERE

| Aspect               | WHERE                               | LET...IN                            |
| -------------------- | ----------------------------------- | ----------------------------------- |
| Syntactic role       | Suffix to declarations              | Standalone expression               |
| Parser type          | Postfix (`Expr -> Expr`)            | Prefix (standalone `Expr`)          |
| Position in code     | End of definition only              | Anywhere in expression context      |
| AST structure        | `Where Anno (Expr n) [LocalDecl n]` | `LetIn Anno [LocalDecl n] (Expr n)` |
| Binding keywords     | Standard DECIDE/MEANS syntax        | IS/BE/MEAN/MEANS                    |
| Recursive bindings   | Yes                                 | Yes                                 |
| Sharing/memoization  | Yes                                 | Yes                                 |
| Evaluation mechanism | `evalRecLocalDecls`                 | `evalRecLocalDecls` (same)          |

---

## Test Cases

1. **Simple non-recursive bindings**
2. **Forward references within LET block**
3. **Mutually recursive definitions**
4. **Nested LET...IN expressions**
5. **LET...IN inside other expressions** (IF, CONSIDER, function arguments)
6. **Type inference with polymorphic bindings**
7. **Lazy evaluation verification** (expensive computation referenced multiple times should only evaluate once)
8. **All binding keyword variants** (IS, BE, MEAN, MEANS)
9. **Variable shadowing** (same name in nested LETs)

---

## Design Rationale

### Why add LET...IN when WHERE exists?

In Haskell, `let...in` is strictly more expressive than `where` in terms of syntactic positions — it can appear anywhere an expression is valid. This enables:

- Local bindings inside lambdas
- Inline computation sharing in complex expressions
- Clearer scoping in nested contexts

### Why allow multiple binding keywords?

L4 targets legal professionals who may not be programmers. Natural language flexibility (IS/BE/MEAN/MEANS) reduces friction and allows drafters to choose phrasing that reads more naturally in context. This follows L4's existing pattern of accepting synonyms (e.g., `IS`/`IF` in DECIDE).

### Why Haskell-style recursive bindings?

Simplicity and power. Non-recursive `let` (as in ML) would require users to think about binding order. Recursive bindings "just work" for both simple sequential definitions and complex mutual recursion, matching the intuition that all definitions in a block are "simultaneous."

### Why a separate letLocalDecl parser?

The existing `localdecl` parser uses the `decide` parser internally, which expects DECIDE/MEANS syntax. LET bindings use IS/BE/MEAN/MEANS keywords which are intentionally scoped to LET...IN only. Reusing `localdecl` would require `("is" <|> "be" <|> "mean") <|> "means"` logic that doesn't make semantic sense - these are distinct syntactic contexts.
