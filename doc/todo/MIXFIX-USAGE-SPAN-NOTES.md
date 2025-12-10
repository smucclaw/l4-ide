# Mixfix Usage Span Issue

## Context

- File under investigation: `jl4/experiments/mixfix.l4`.
- `#EVAL` on line 15 parses/executes successfully only when operands are wrapped in parentheses.
- Line 17 (same expression without parentheses) fails with two diagnostics:
  1. "`alice` … is not a function" (parser treated `alice` as a prefix function applied to four arguments).
  2. `` `to make` `` identifier not found.

## Root Cause Analysis (Session 2024-12)

### The Real Problem

The issue is **NOT** in `mixfixChainExpr` or source range extraction. The **actual root cause** is in the `app` parser being too greedy:

1. **`app` parser is greedy**: The `app` parser (line ~1319) parses a function name followed by arguments using `atomicExpr'`
2. **`atomicExpr'` accepts quoted names**: Line ~1263-1267 shows `atomicExpr'` uses `nameAsApp App` which calls `name`
3. **`name` includes `quotedName`**: Line ~336 shows `name = attachEpa (quotedName <|> try qualifiedName <|> simpleName)`
4. **Backticked identifiers are consumed as arguments**: When parsing `alice \`copulated with\` bob`, the `app` parser sees:
   - `alice` as the function name
   - `` `copulated with` `` as argument 1 (a quoted name!)
   - `bob` as argument 2
   - `` `to make` `` as argument 3
   - `charlie` as argument 4
5. **`mixfixChainExpr` never gets a chance**: By the time `mixfixChainExpr` receives `firstExpr`, it's already `App alice [copulated with, bob, to make, charlie]` - nothing left to parse as mixfix!

### Why Parentheses Work

`(alice)` parses through `paren expr` which creates a complete expression. When `app` looks for arguments after `(alice)`, the first thing it sees is `` `copulated with` ``, which triggers the backtick-based mixfix parsing correctly because `(alice)` itself doesn't try to consume arguments.

## Attempted Fix (Reverted)

### Approach

Prevent `app` from consuming backticked names as arguments by:

1. Adding `nameNoQuoted` parser (excludes backticked names)
2. Adding `atomicExprNoQuoted'` variant
3. Modifying `app` to use `atomicExprNoQuoted'` for argument parsing

### Result

This fix made `alice \`copulated with\` bob` work correctly, but **broke many other tests** where backticked names ARE meant to be function arguments, e.g.:

- `Day \`Excel 1900 epoch\`` - function application (Day is a function)
- `` `Total Repayment Amount` `` in promissory-note.l4

### Why Simple Fix Won't Work

The fundamental issue is **ambiguity that can't be resolved at parse time**:

- `Day \`Excel 1900 epoch\`` - backticked name is a function argument
- `alice \`copulated with\` bob` - backticked name is a mixfix operator
- `alice \`is cool\`` - postfix mixfix operator (no following expression)

Without semantic information (knowing if `alice` vs `Day` is a function), the parser cannot distinguish these cases.

## Possible Solutions

### Option 1: Require Parentheses (Status Quo)

- For mixfix: `(alice) \`copulated with\` (bob)`
- Document this behavior as expected
- Pros: No code changes, backward compatible
- Cons: Less intuitive syntax

### Option 2: Require Parentheses for Backticked Arguments

- For function args: `Day (\`Excel 1900 epoch\`)`
- For mixfix: `alice \`copulated with\` bob` (no parens needed)
- Pros: More intuitive for mixfix
- Cons: Breaks existing code, requires migration

### Option 3: Post-Parsing Resolution (Recommended)

- Parse greedily as function application
- During typechecking, detect when a non-function is "applied" to arguments that include backticked names
- Rewrite `App alice [\`copulated with\`, bob, ...]`to mixfix when`alice` isn't a function
- Pros: Best of both worlds, no syntax changes
- Cons: More complex implementation, requires typechecker changes

### Option 4: Explicit Mixfix Syntax

- Use different syntax to invoke mixfix operators
- E.g., `@mixfix alice \`copulated with\` bob`
- Pros: Unambiguous
- Cons: Verbose, changes language syntax

## Files & Locations

### Key Parsers (jl4-core/src/L4/Parser.hs)

- `name` (line ~336): `quotedName <|> try qualifiedName <|> simpleName`
- `atomicExpr'` (line ~1263): `lit <|> nameAsApp App <|> paren expr`
- `app` (line ~1319): Uses `lmany (const (indented atomicExpr' current))` for argument parsing
- `mixfixChainExpr` (line ~1138): Handles mixfix operator chains

### Parser Hierarchy

```
expr
  └── mixfixChainExpr
        └── baseExpr
              └── baseExpr'
                    └── app  <-- THE CULPRIT
                          ├── name (function name)
                          └── lmany atomicExpr' (arguments)
                                    └── nameAsApp
                                          └── name  <-- includes quotedName!
```

### Test File

- `jl4/experiments/mixfix.l4`:
  - Line 15 (works): `#EVAL (alice) \`copulated with\` (bob) \`to make\` charlie`
  - Line 17 (fails): `#EVAL alice \`copulated with\` bob \`to make\` charlie`

## Handoff Notes

- Code changes reverted - no modifications to Parser.hs
- Option 3 (post-parsing resolution) is the recommended approach but requires architectural changes
- For now, use parenthesized syntax as workaround
- Consider creating a GitHub issue to track this as a language design decision
