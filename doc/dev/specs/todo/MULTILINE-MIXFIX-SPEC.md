# Multiline Mixfix Spec

## Context

- Mixfix and postfix operators are declared via `MEANS`/`IS-DEFINED-AS` clauses and recorded in the mixfix hint registry during the first parse.
- `mixfixChainExpr` currently requires each keyword token to live on the **same source line** as the preceding operand. This mirrors the historical behavior of postfix functions needing to be on a single line (e.g. `radius squared`).
- Built-in keywords such as `IF`, `THEN`, and `ELSE` are indentation-aware: they may be vertically stacked at the same column while remaining part of the same expression.
- Users want equivalent ergonomics for mixfix keywords so `party `is eligible for`
program` (line break between operands and keywords) behaves the same as the single-line form.

## Goals

1. Allow mixfix keywords to begin on a new line as long as they line up with the start column of the preceding operand (same indentation block) just like `IF/THEN/ELSE`.
2. Keep the existing same-line fast path for backwards compatibility and avoid regressing current programs.
3. Maintain hint-registry gating so that only declared keywords are consumed.
4. Preserve precise EPA/Anno spans so IDE highlight ranges do not jump when keywords move to new lines.

## Non-Goals

- No change to how keywords are declared or to the mixfix registry format.
- Do not alter the type-checker reinterpretation fallback yet; parser enhancements must preserve today’s AST so the fallback path still works.

## Proposed Parser Changes

1. **Indent-aware lookahead helper**

   - Extend `mixfixKeywordOnLine` (Parser.hs ~1211) into a generalized `mixfixKeywordAligned` helper that accepts `exprEndLine` and `exprEndColumn`.
   - Permit keyword tokens whose start line is either the same line **or** the next line provided the start column matches the col of the preceding operand.
   - Guard with registry membership (already done) to avoid consuming random identifiers.

2. **Chain parser update**

   - Update `mixfixKeywordClause` inside `mixfixChainExpr` to use the new helper when deciding whether another `(keyword, expr)` pair can follow.
   - When a keyword is accepted on the next line, ensure the subsequently parsed argument inherits the same indentation guard so `a
`foo`
b` is legal while misaligned/ambiguous layouts still fail.

3. **Postfix alignment**

   - `mixfixPostfixOp` should share the same helper so postfix-only keywords (`radius
squared`) also respect indentation rather than being forced onto one line.

4. **EPA annotations**
   - When the keyword begins on a new line, update the `Range` stored in `keywordAnno` so the newline is captured. This maintains IDE features (hover/highlight) for multi-line mixfix expressions.

## Testing Strategy

1. **New parser spec**

   - Extend `MixfixParserSpec` with golden snippets covering:
     - Multi-line postfix: `radius` on one line, `` `squared` `` indented to the same column on the following line.
     - Multi-line multi-operand chain: one keyword per line but aligned columns, verifying the parser still produces a `MixfixChain` with the same keywords as the single-line equivalent.

2. **Regression programs**

   - Add `jl4/examples/ok/mixfix-multiline.l4` demonstrating IF/THEN style alignment for a user-defined mixfix declaration and ensure `cabal test` covers it (via existing example-driven tests or a new `golden` file).

3. **Manual scenario**
   - Reproduce the prior postfix-with-variables failure in multi-line form to ensure the parser + reinterpretation fallback continue to agree.

## Acceptance Criteria

- Parser accepts vertically aligned mixfix keywords/postfix without needing backticks on operands or extra parentheses.
- Hint-aware parsing still prevents unrelated identifiers from being captured as keywords.
- All existing tests pass (`cabal test all`, `npm run format`).
- Documentation in `doc/mixfix-implementation-plan.md` and/or related mixfix notes explains the new indentation flexibility.

## Risks & Mitigations

- **Risk**: Allowing next-line keywords could reintroduce greediness if columns are mis-read.
  - _Mitigation_: Reuse the same column data the layout-sensitive `let`/`where` blocks already rely on; include regression tests with mismatched indentation to ensure they still fail predictably.
- **Risk**: EPA ranges missing newline info.
  - _Mitigation_: Write tests/assertions in `MixfixParserSpec` comparing `range.start.line`/`start.col` for the new layouts.

## Open Questions

- Should we also allow keywords deeper than the operand (e.g., argument + 2 spaces indent)? For the first implementation we will require exact column matching and revisit later if authors demand hanging indent support.
- How should we surface diagnostics when indentation is wrong? Proposal: Keep existing "expected keyword" messages since the parser will simply stop the chain when the alignment test fails.
