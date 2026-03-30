# L4 Advanced Patterns Reference

Advanced and intermediate L4 patterns for structuring complex rules, calculations, and contracts. This page covers patterns beyond the basics found in the [Common Patterns tutorial](../../tutorials/getting-started/common-patterns.md).

For basic patterns (simple records, predicates, list operations, conditionals, simple obligations), see the [Common Patterns](../../tutorials/getting-started/common-patterns.md) page.

---

## Progressive Tax Brackets

**When to use:** Multi-bracket calculations where different portions of a value are subject to different rates (tax, fees, tiered pricing).

**Canonical form:** Use WHERE to decompose each bracket into its own named intermediate, then sum the results.

```l4
GIVEN income IS A NUMBER
GIVETH A NUMBER
`tax owed` MEANS
    `bracket1 tax` + `bracket2 tax` + `bracket3 tax`
    WHERE
        `bracket1 tax` MEANS `amount in bracket1` * rate1
        `bracket2 tax` MEANS `amount in bracket2` * rate2
        `bracket3 tax` MEANS `amount in bracket3` * rate3

        `amount in bracket1` MEANS
            BRANCH
                IF income <= threshold1 THEN income
                OTHERWISE threshold1

        `amount in bracket2` MEANS
            BRANCH
                IF income <= threshold1 THEN 0
                IF income <= threshold2 THEN income - threshold1
                OTHERWISE threshold2 - threshold1

        `amount in bracket3` MEANS
            IF income <= threshold2 THEN 0
            ELSE income - threshold2
```

**Notes:**

- Each bracket clips the income to its own range, avoiding double-counting.
- WHERE bindings let you name each intermediate for readability and traceability.
- Extend to N brackets by adding more `amount in bracketN` definitions.
- The evaluator's trace will show exactly how much falls into each bracket, which is valuable for audit and explanation.

---

## Eligibility with Record Decomposition

**When to use:** Complex multi-condition eligibility checks where each condition deserves its own named sub-expression.

**Canonical form:** A top-level DECIDE with named WHERE bindings for each requirement.

```l4
DECLARE Person HAS
    age IS A NUMBER
    citizenship IS A STRING
    income IS A NUMBER
    disqualified IS A BOOLEAN

GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `is eligible` IF
    `age requirement met`
    AND `citizenship requirement met`
    AND `income requirement met`
    AND NOT `is disqualified`
    WHERE
        `age requirement met` MEANS
            person's age >= minAge
            AND person's age < maxAge

        `citizenship requirement met` MEANS
            person's citizenship = "citizen"
            OR person's citizenship = "permanent resident"

        `income requirement met` MEANS
            person's income < incomeThreshold

        `is disqualified` MEANS
            person's disqualified
```

**Notes:**

- Each WHERE binding maps to a distinct legal requirement, making the structure traceable back to the source rule.
- The top-level expression reads like a checklist: age, citizenship, income, disqualification.
- Individual requirements can be independently tested and reused.

---

## Mixfix Function Definitions

**When to use:** When you want function parameters interspersed with identifier words, enabling infix or postfix calling conventions that read more naturally.

**Canonical form:** Parameters appear between or after identifier words in the definition head.

```l4
GIVEN mom IS A STRING
      dad IS A STRING
      kid IS A STRING
GIVES A STRING
mom and dad `have a baby named` kid MEANS
    CONCAT "mother: ", mom, ", father: ", dad, ", child: ", kid
```

**Calling convention:**

```l4
"alice" and "bob" `have a baby named` "carol"
```

**Notes:**

- Mixfix definitions let L4 expressions read closer to natural language.
- Parameters can appear before, between, or after the function's identifier words.
- Backtick-quoted identifiers can contain spaces, allowing multi-word function names.
- Useful for legal predicates that follow natural phrasing, e.g. `person `is entitled to` benefit`.

---

## Caret Operator (Ditto)

**When to use:** When repeating the same operator or keyword across multiple lines in a vertically aligned expression. The caret `^` acts as "ditto" -- it copies the token that appeared in the same column position on the previous line.

**Canonical form:**

```l4
`tautology` MEANS
        p EQUALS TRUE
    OR  p EQUALS FALSE
   AND  q ^      TRUE    -- ^ = EQUALS
    OR  q ^      FALSE
```

**Notes:**

- The `^` operator is pure syntactic sugar for vertical alignment. It resolves to whatever token occupied the same column on the nearest preceding line.
- Primarily useful in decision tables and multi-branch boolean expressions where the same operator repeats.
- Improves visual alignment and reduces repetitive boilerplate.
- See the [Syntax reference](../syntax/README.md) for full details on special symbols.

---

## Ellipsis Operators and Inert Elements (Statutory Isomorphism)

**When to use:** When formalizing legislation or regulatory text and you want the L4 code to mirror the exact structure of the source statute. The ellipsis operators provide implicit boolean connectives, and bare quoted strings act as inert scaffolding that preserves the statutory language without affecting evaluation.

**Operators:**

- `...` (triple dot) -- syntactic sugar for AND (asyndetic conjunction)
- `..` (double dot) -- syntactic sugar for OR (asyndetic disjunction)
- `"quoted string"` in boolean context -- inert element (evaluates to the identity value for the enclosing operator: TRUE for AND, FALSE for OR)

**Example: Singapore Penal Code S415 (Cheating)**

```l4
GIVEN `by deceiving any person`   IS A BOOLEAN
      `fraudulently`              IS A BOOLEAN
      `dishonestly`               IS A BOOLEAN
      `deliver`                   IS A BOOLEAN
      `cause the delivery`        IS A BOOLEAN
      `intentionally`             IS A BOOLEAN
      `causes`                    IS A BOOLEAN
      `is likely to cause`        IS A BOOLEAN
      `damage`                    IS A BOOLEAN
      `harm`                      IS A BOOLEAN
DECIDE `commits cheating` IF
        `by deceiving any person`
   ...      `fraudulently`
        OR  `dishonestly`
        ..  "induces the person so deceived"
   ...  "to" ...  `deliver`
             OR   `cause the delivery`
             ..   "of any property to any person"

    OR  `intentionally` .. "induces the person so deceived"
        ... "which act or omission"
                ..  `causes`
                OR  `is likely to cause`
            ...     `damage`
                OR  `harm`
                ..  "to any person"
```

**Notes:**

- The quoted strings (`"induces the person so deceived"`, `"to"`, `"of any property to any person"`, etc.) are preserved in the AST for visualization and natural-language explanation but do not affect the boolean evaluation.
- This pattern lets the formalization be read side-by-side with the original statute, making review by legal professionals straightforward.
- The `...` and `..` operators replace explicit AND/OR keywords where the statutory text uses asyndetic (connective-free) phrasing.
- See the [Operators reference](../operators/README.md) for operator precedence and the asyndetic operator details.

---

## UNLESS

**When to use:** When a legal rule has an exception clause. UNLESS is equivalent to AND NOT but reads more naturally in legal contexts.

**Canonical form:**

```l4
DECIDE `is eligible` IF
        `meets age requirement`
    AND `has valid ID`
 UNLESS  `is disqualified`
```

This is semantically identical to:

```l4
DECIDE `is eligible` IF
        `meets age requirement`
    AND `has valid ID`
    AND NOT `is disqualified`
```

**Notes:**

- UNLESS binds looser than OR, so it applies to the entire preceding expression.
- `p UNLESS q` is equivalent to `p AND NOT q`.
- Preferred in rules-as-code formalization where the source text says "unless" or "except where".
- See the [Operators reference](../operators/README.md) for precedence details.

---

## LET / IN (Prefix-Style Local Bindings)

**When to use:** When you want to define local bindings before the expression that uses them, as an alternative to WHERE (which places bindings after).

**Canonical form:**

```l4
result MEANS
    LET x MEANS a + b
        y MEANS c * d
    IN  x + y
```

**Equivalent WHERE form (postfix):**

```l4
result MEANS
    x + y
    WHERE
        x MEANS a + b
        y MEANS c * d
```

**Notes:**

- LET/IN and WHERE are interchangeable -- choose based on readability preference.
- LET/IN is "define then use" (top-down reading). WHERE is "use then define" (result-first reading).
- WHERE tends to be more common in L4 because it mirrors how legal rules state the conclusion first, then define terms.
- See the [Functions reference](../functions/README.md) for details on local bindings.

---

## Section Scoping

**When to use:** To organize L4 code into named sections and subsections, similar to numbered sections in legislation.

**Canonical form:**

```l4
§ Definitions
  DECLARE Person HAS
      name IS A STRING
      age IS A NUMBER

§ Eligibility
  GIVEN p IS A Person
  DECIDE `is eligible` IF p's age >= 18

  §§ Special Cases
    -- Nested subsection
    DECIDE `is exempt` IF ...
```

**Notes:**

- `§` introduces a top-level section. `§§` introduces a nested subsection.
- Sections provide namespace scoping -- definitions within a section are scoped to it.
- Section names mirror the structure of legislation (Part, Division, Section, Subsection).
- Useful for organizing large L4 files that formalize multi-part statutes or contracts.

---

## Nested WHERE Scopes

**When to use:** When a complex calculation has multiple levels of intermediate values, and you want to keep each level's definitions local to where they are used.

**Canonical form:**

```l4
result MEANS
    outer1 + outer2
    WHERE
        outer1 MEANS inner1 + inner2
            WHERE
                inner1 MEANS x * 2
                inner2 MEANS y * 3

        outer2 MEANS inner3
            WHERE
                inner3 MEANS outer1 * 4  -- Can reference outer1
```

**Notes:**

- Inner WHERE blocks can reference bindings from enclosing scopes.
- Each WHERE block creates its own scope -- names defined in one nested WHERE are not visible in sibling WHERE blocks.
- Use sparingly: deeply nested WHEREs can become hard to read. Consider flattening into a single WHERE when possible.

---

## Full Regulative Rule

**When to use:** When modeling a complete contractual workflow with sequential obligations, conditions precedent, and computed values.

**Example: Loan Contract**

```l4
GIVEN borrower IS A Person
      lender   IS A Person
      capital  IS A NUMBER
      interest IS A NUMBER
      closing  IS A DATE
      repayment IS A DATE
      `conditions precedent are met` IS A BOOLEAN
`Loan Contract` MEANS
  IF     NOT `conditions precedent are met`
  THEN   FULFILLED
  ELSE   PARTY lender
         MUST  EXACTLY send capital to borrower
         WITHIN closing
         HENCE    PARTY  borrower
                  MUST   EXACTLY send accrued to lender
                  WITHIN repayment
  WHERE
    send money to person MEANS
        CONCAT person's name, " receives ", TOSTRING(money)
    accrued MEANS capital PLUS capital * interest
```

**Notes:**

- Conditions precedent gate the entire obligation chain. If not met, the contract is FULFILLED (no obligations arise).
- HENCE chains sequential obligations: the lender must fund first, then the borrower must repay.
- WHERE defines helper functions (the mixfix `send money to person`) and computed values (`accrued`).
- `EXACTLY` constrains the action to the precise amount specified.
- This pattern combines conditionals, deontic rules, HENCE chaining, WHERE bindings, and mixfix definitions in a single coherent structure.
- See the [Regulative reference](../regulative/README.md) for full details on PARTY, MUST, HENCE, LEST, and other deontic keywords.

---

## Recursive Definitions

**When to use:** When a value is defined in terms of itself with a base case, such as mathematical recursion or recursive data traversal.

**Canonical form:**

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
factorial MEANS
    IF n <= 1
    THEN 1
    ELSE n * (factorial (n - 1))
```

**Notes:**

- Always include a base case to prevent infinite recursion.
- Recursive definitions work naturally with pattern matching on lists (see [Common Patterns: List Pattern Matching](../../tutorials/getting-started/common-patterns.md)).
- The evaluator supports lazy evaluation, but infinite recursion without a base case will still diverge.

---

## Lambda Functions

**When to use:** When you need an anonymous function inline, typically as an argument to higher-order functions like `map`, `filter`, `all`, or `any`.

**Canonical form:**

```l4
GIVEN x YIELD expression
```

**Examples in context:**

```l4
-- Filter positive numbers
filter (GIVEN n YIELD n > 0) numbers

-- Extract ages from a list of people
map (GIVEN p YIELD p's age) people

-- Check all governors are adults
all (GIVEN g YIELD g's age >= 18) (charity's governors)
```

**Notes:**

- `GIVEN x YIELD expr` is L4's lambda syntax, equivalent to `\x -> expr` in Haskell or `x => expr` in JavaScript.
- Wrap field access expressions in parentheses when used inside quantifiers: `(charity's governors)`.
- Lambdas are most commonly used with list operations from the prelude.
- See the [Functions reference](../functions/README.md) for GIVEN/YIELD details.

---

## See Also

- **[Common Patterns](../../tutorials/getting-started/common-patterns.md)** -- Basic patterns (records, predicates, lists, conditionals, simple obligations)
- **[Functions Reference](../functions/README.md)** -- GIVEN, GIVETH, DECIDE, MEANS, WHERE, LET
- **[Operators Reference](../operators/README.md)** -- AND, OR, NOT, UNLESS, IMPLIES, ellipsis operators
- **[Syntax Reference](../syntax/README.md)** -- Layout rules, backtick identifiers, ditto marks, annotations
- **[Regulative Reference](../regulative/README.md)** -- PARTY, MUST, MAY, SHANT, HENCE, LEST, WITHIN
- **[Types Reference](../types/README.md)** -- DECLARE, HAS, IS ONE OF, LIST, MAYBE
- **[GLOSSARY](../GLOSSARY.md)** -- Master index of all language features
