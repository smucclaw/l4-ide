# Computed Fields: Comma Parsing Design Report

Date: 2026-03-15
Branch: `mengwong/methods`

## 1. What Was Built

Computed fields allow DECLARE record fields to specify a `MEANS` clause whose
body is an expression computed lazily from other fields:

```l4
DECLARE Person HAS
    `birth year`  IS A NUMBER
    `as at year`  IS A NUMBER
    `age`         IS A NUMBER
        MEANS `as at year` - `birth year`
    `adult`       IS A BOOLEAN
        MEANS `age` >= 18
```

Implementation uses a **desugar-early strategy**: before type checking,
`desugarComputedFields` in `Desugar.hs` transforms each computed field into a
synthetic top-level DECIDE declaration with a `_self` parameter, and wraps the
MEANS body in LET bindings that project sibling fields from `_self`.  The record
type retains all fields as stored; accessor calls are generated automatically.

The parser change is small: `reqParam` (which parses each field in a
DECLARE HAS block) gains an optional `MEANS` clause parsed by
`indentedExpr current`.

### Files changed

- **Parser.hs** – `reqParam` extended with optional MEANS; `parseAppArgs`
  simplified to never consume commas (juxtaposition only).
- **Desugar.hs** – `desugarComputedFields`, `makeComputedDecide`.
- **Syntax.hs** – `TypedName` extended with `Maybe (Expr n)`.
- **TypeCheck.hs** – calls `desugarComputedFields` before `checkProgram`.
- **Print.hs** – prints MEANS clause for computed fields.
- Various downstream modules updated for the new `TypedName` field.

### Test files

All in `jl4/experiments/`:

| File | What it tests |
|------|--------------|
| `computed-fields-basic.l4` | Inline arithmetic, boolean, IF/THEN/ELSE |
| `computed-fields-chain.l4` | Field-depends-on-field chains |
| `computed-fields-nested.l4` | Nested records with computed fields |
| `computed-fields-external.l4` | MEANS calls top-level function |
| `computed-fields-external-of.l4` | Same but using OF syntax |
| `computed-fields-age.l4` | Multiple snapshots of same entity |
| `computed-fields-legal.l4` | Legislative definitions (Employment Act) |
| `computed-fields-cycle.l4` | Mutual recursion (expected: diverges) |
| `computed-fields-override.l4` | Constructor overrides computed field |
| `computed-fields-where-let.l4` | WHERE and LET/IN inside MEANS, with commas |

---

## 2. The Comma Question

### The potential problem

When computed fields were first designed, a concern arose: could commas inside
a MEANS expression conflict with the field separator in `recordDecl'`?

```l4
DECLARE Employee HAS
    `age` IS A NUMBER
        MEANS `years between` `date of birth`, `current year`
    `covered` IS A BOOLEAN
```

If `parseAppArgs` consumed commas between juxtaposed arguments (`f a, b`
meaning `f(a, b)`), this comma would be ambiguous: is it an argument separator
within the MEANS expression, or a field separator for `recordDecl'`?

### The key insight: juxtaposition doesn't need commas

L4 follows the Haskell convention for function application: arguments are
juxtaposed with spaces, not separated by commas.  `f a b` is the standard
form; `f OF a, b` is the explicit comma-separated alternative using the `OF`
keyword.

Once we recognised this, the solution was clear: **`parseAppArgs` should never
consume commas**.  Commas are only meaningful in explicit comma-separated
contexts (OF, LIST, WITH, CONSIDER, CONCAT), each of which has its own
dedicated parser using `lsepBy`/`lsepBy1`.

### The fix

Remove comma consumption from `parseAppArgs` entirely.  The function now
collects indented atomic expressions by juxtaposition only:

```haskell
parseAppArgs :: Pos -> Name -> Parser [Expr Name]
parseAppArgs current fname = go True
  where
    funcLine = nameEndLine fname
    go allowBreak = do
      mArg <- optional $ try $ parseOne allowBreak
      case mArg of
        Nothing -> pure []
        Just arg -> (arg :) <$> go False
    parseOne allowBreak = do
      when allowBreak $ guardMixfixKeyword funcLine
      indented atomicExpr' current
```

No context flags, no special cases.  This means:

- `recordDecl'` keeps its `lsepBy ... TComma` — the colloquial form
  `DECLARE Foo HAS x IS A NUMBER, y IS A NUMBER` continues to work.
- MEANS expressions inside fields use juxtaposition (`f a b`) or OF syntax
  (`f OF a, b`) for multi-argument calls.  Commas within OF/LIST/etc. are
  correctly scoped by their own parsers.
- No parser state or context-sensitivity is needed.

### Why there is no conflict

After `reqParam` parses a field including its MEANS expression, any commas
that weren't consumed by a sub-parser (OF, LIST, etc.) remain in the token
stream.  `lsepBy` in `recordDecl'` then claims these as field separators.

This works because:

1. `parseAppArgs` never touches commas — they pass through cleanly.
2. OF/LIST/WITH/CONSIDER each use `lsepBy1` internally, so their commas
   are consumed within the expression, invisible to `recordDecl'`.
3. The only commas visible to `recordDecl'` are genuine field separators.

### Contrast with WHERE blocks

WHERE blocks use `many (indented localdecl p)` — layout only, no commas.
This is equally clean for a different reason: local declarations are
separated by indentation, and there's no comma to conflict with.

Both approaches work because commas have a single, unambiguous meaning in
each context:

| Context | Separator | Comma role |
|---------|-----------|-----------|
| DECLARE HAS fields | `lsepBy ... TComma` | Field separator |
| WHERE bindings | `many (indented ...)` | Not used |
| OF arguments | `lsepBy1 ... TComma` | Argument separator |
| LIST items | `lsepBy ... TComma` | Item separator |
| Top-level decls | `lsepBy ... TSemicolon` | Not used |

---

## 3. Design Principle

In L4's grammar, commas serve as **syntactic separators in explicit list
contexts** (OF, LIST, WITH, CONSIDER, CONCAT, HAS fields, GIVEN parameters).
Function application by juxtaposition does not use commas.

This is consistent with L4's design principle that commas are
"sugar-equivalent to newlines followed by indentation to the same level."
In each list context, a comma means "and another item follows at this level."
Between juxtaposed function arguments, no separator is needed — indentation
alone governs the scope.

The result is a clean, context-free grammar with no parser state flags.
