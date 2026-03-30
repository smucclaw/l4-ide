# L4 Keywords Reference

Dense technical specs. Each section: syntax, type, semantics, FP analog, example.

**Cross-language cheat sheet:** struct/class/interface → DECLARE HAS. enum/union/variant → IS ONE OF. def/fn/function → MEANS or DECIDE. switch/case/match → CONSIDER or BRANCH. ternary/if-else → IF THEN ELSE (for long chains, use BRANCH). lambda/arrow function/closure → GIVEN...YIELD. let/const/local variable → WHERE or LET...IN. null/nil/None/optional/nullable → MAYBE. array/list/sequence → LIST OF. dot notation/property access/member access/genitive → `'s` possessive. string concatenation/join → CONCAT. modulo/remainder/mod → MODULO. toString/str()/cast → TOSTRING. for-each/forEach/all/every → `all`/`any` (IMPORT prelude). namespace/scope → § sections. implicit AND/asyndetic conjunction → `...` (three dots). implicit OR/asyndetic disjunction → `..` (two dots). comment/annotation/inert/no-op → bare `"string"` in boolean context. comma-separated args/apply/call → OF (e.g. `f OF arg1, arg2`).

---

## 's (Genitive / Possessive / Field Access)

**Syntax:** `record's fieldname`
**Semantics:** Record field access (dot notation equivalent). The `'s` clitic is variously called the possessive, genitive, Saxon genitive, genitive clitic, or possessive clitic in linguistics.
**Haskell:** Record accessor function, or lens `^. field`
**Other languages:** `record.field` (JS/Python), `record->field` (C), `record.getField()` (Java)
**Chaining:** `company's ceo's name` (like `company.ceo.name`)

```l4
person's age
contract's buyer's name
```

**Note:** When used as argument to a function, wrap in parens: `f (record's field)`

---

## ^ (Caret / Ditto Operator)

**Syntax:** `^` in an expression
**Syntactic sugar** for the token in the same position from the previous line.
**Linguistics:** Performs ellipsis — specifically gapping (omitting repeated operators/verbs across parallel conjuncts). Related concepts: stripping, coordination reduction, right node raising, zeugma, parallel structure. The caret explicitly marks the gap site rather than leaving it implicit.
**Use:** Vertical alignment for human readability in boolean expressions and decision tables
**No equivalent in most languages** — unique to L4 (in another timeline, the Unicode ellipsis `…` U+2026 might have served, but humans know how to type `^` on the keyboard. L4 does have actual ellipsis operators used for other purposes.)

```l4
`tautology` MEANS
        p EQUALS TRUE
    OR  p EQUALS FALSE
   AND  q ^      TRUE    -- ^ means EQUALS (from previous line)
    OR  q ^      FALSE
```

---

## ... (Ellipsis AND / Asyndetic Conjunction)

**Syntax:** `... expr` within a boolean expression
**Syntactic sugar** for AND. Three dots join the following expression with AND to the preceding context.
**Linguistics:** Asyndetic conjunction — coordinating clauses without an explicit conjunction word. Cf. syndetic conjunction (with AND/OR keywords).
**Use:** Embedding natural-language scaffolding into boolean logic so the L4 reads like the source statute or contract. Used with inert elements (bare string literals in boolean context, which evaluate to TRUE in AND context and FALSE in OR context — the identity values for their respective operators).

```l4
-- From Singapore Penal Code §415
DECIDE `commits cheating` IF
        `by deceiving any person`
   ...  p1   ...
                  `fraudulently`
              OR  `dishonestly`
              ..  "induces the person so deceived"
         ...    "to" ...     `deliver`
                         OR  `cause the delivery`
```

**Note:** The bare string `"induces the person so deceived"` is an inert element — it appears in the AST for visualization/explanation but evaluates to the boolean identity for its context (TRUE for AND, FALSE for OR), so it doesn't affect the logical result.

---

## .. (Ellipsis OR / Asyndetic Disjunction)

**Syntax:** `.. expr` within a boolean expression
**Syntactic sugar** for OR. Two dots join the following expression with OR to the preceding context.
**Linguistics:** Asyndetic disjunction. See `...` above for full explanation.
**Use:** Same as `...` but for OR chains. Often used to attach inert string scaffolding to OR-linked alternatives.

```l4
              `causes`
          OR  `is likely to cause`
      ...     `damage`
          OR  `harm`
          ..  "to any person"    -- inert, evaluates to FALSE (OR identity)
      ...     "in"
          ..  `body`
          ..  `mind`
          ..  `reputation`
          OR  `property`
```

---

## Inert Elements (Bare Strings in Boolean Context)

**Syntax:** `"any string"` appearing as an operand in a boolean expression
**Semantics:** Grammatical scaffolding. String literals in boolean context are converted to `Inert` nodes during type checking. They evaluate to the identity value for their containing operator: TRUE in AND context, FALSE in OR context.
**Use:** Mirrors statutory language structure without affecting logical evaluation. Preserved in AST for visualization, NLG, and explanation traces.
**See also:** `...` (AND ellipsis), `..` (OR ellipsis)

---

## AND

**Syntax:** `expr AND expr`
**Type:** `BOOLEAN -> BOOLEAN -> BOOLEAN`
**Semantics:** Logical conjunction
**Aliases:** `&&`, `...`
**Precedence:** Higher than OR, lower than NOT

```l4
age >= 18 AND isCitizen
```

---

## BEFORE (NOT YET IMPLEMENTED)

**Syntax:** `... BEFORE deadline`
**Intended semantics:** Temporal deadline in deontic rule (alternative to WITHIN) for absolute dates/events
**Status:** Planned but not yet in parser. Use WITHIN for now.
**See also:** WITHIN (implemented, for relative durations)

---

## BRANCH

**Syntax:** `BRANCH IF cond1 THEN val1 ... OTHERWISE default`
**Type:** `GIVETH T` (all branches same type)
**Semantics:** Multi-way conditional, top-to-bottom, first match wins
**Other languages:** switch/case (C/Java), match (Rust/Scala), cond (Lisp), guards (Haskell)
**Use:** Avoid nested IF/THEN/ELSE indentation problems

```l4
grade MEANS
    BRANCH
        IF score >= 90 THEN "A"
        IF score >= 80 THEN "B"
        OTHERWISE "F"
```

**vs IF:** Binary choice → IF/THEN/ELSE. Multiple choices → BRANCH.
**vs CONSIDER:** Value-based → BRANCH. Pattern matching → CONSIDER.

To construct decision tables, use the caret operator to substitute for `IF` / `AND` / `THEN` keywords, as well as repeating terms, so that visually they look like column separators. This accords with the human visual system, and with a linguistic "sounding-out" of the non-caret terms, the repeating parts having been elided. In Tufte's terms, this convention improves the data-ink ratio.

```l4
grade MEANS
    BRANCH
        IF score >= 90 THEN "A"
        ^  ^     >= 80 ^    "B"
        ^  ^     >= 70 ^    "C"
        ^  ^     >= 60 ^    "D"
        ^  ^     >= 50 ^    "E"
        OTHERWISE           "F"
```

---

## CONCAT

**Syntax:** `CONCAT expr1, expr2, ...`
**Type:** `STRING -> STRING -> ... -> STRING`
**Semantics:** String concatenation of multiple arguments
**Other languages:** `+` (JS/Python), `++` (Haskell), `strcat` (C), `.` (PHP)
**See also:** `APPEND` for binary infix alternative

```l4
greeting MEANS CONCAT "Hello, ", person's name, "!"
```

---

## CONSIDER

**Syntax:** `CONSIDER expr WHEN pattern THEN result ... OTHERWISE default`
**Type:** Pattern match on expr type
**Semantics:** Pattern matching / destructuring elimination form
**Haskell:** `case ... of`
**Other languages:** switch/case (C/Java), match (Rust/Python 3.10+), when (Kotlin), destructuring (JS/TS/Rust)
**Use:** Sum types, Maybe, List destructuring

```l4
CONSIDER status
WHEN Active THEN "running"
WHEN Suspended reason THEN "paused: " APPEND reason
WHEN Closed THEN "stopped"
```

**Patterns:**

- Constructor: `WHEN Active THEN ...`
- Constructor + arg: `WHEN Suspended x THEN ...`
- List empty: `WHEN EMPTY THEN ...`
- List cons: `WHEN head FOLLOWED BY tail THEN ...`
- Maybe: `WHEN NOTHING THEN ...` / `WHEN JUST x THEN ...`

---

## DECIDE

**Syntax:** `DECIDE identifier IF|IS expression`
**Type:** `GIVEN inputs GIVETH T`
**Semantics:** Function definition for constitutive rules (eligibility, classification)
**Haskell:** `identifier :: a -> T`

```l4
-- Boolean predicate
GIVEN age IS A NUMBER
GIVETH A BOOLEAN
DECIDE `is adult` IF age >= 18

-- Multi-valued classification
GIVEN income IS A NUMBER
GIVETH A STRING
DECIDE bracket IS
    BRANCH
        IF income < 10000 THEN "low"
        IF income < 50000 THEN "mid"
        OTHERWISE "high"
```

**IF variant:** Returns BOOLEAN, reads like question
**IS variant:** Returns any type, reads like assignment
**vs MEANS:** Semantically identical, style preference

---

## DECLARE

**Syntax:** `DECLARE TypeName HAS field1 IS A Type1, ...` (record)
`DECLARE TypeName IS ONE OF Ctor1, Ctor2 HAS field IS A Type, ...` (sum)
**Semantics:** Algebraic data type definition
**Haskell:** `data TypeName = TypeName { field1 :: Type1, ... }` or `data TypeName = Ctor1 | Ctor2 Type`

```l4
-- Product type (record)
DECLARE Person HAS
    name IS A STRING
    age IS A NUMBER
    status IS A Status

-- Sum type (enum with fields)
DECLARE Status IS ONE OF
    Active
    Suspended HAS
        reason IS A STRING
    Closed

-- Nested
DECLARE Company HAS
    name IS A STRING
    ceo IS A Person
    employees IS A LIST OF Person
```

**Field access:** `person's name`, `company's ceo's age`

---

## DEONTIC

**Type:** `DEONTIC` (effect type for regulative rules)
**Semantics:** Modal logic wrapper for obligations/permissions/prohibitions
**Values:** `PARTY actor MUST/MAY/SHANT action ...`
**Trace:** `#TRACE` executes deontic rules, produces obligation trace

```l4
GIVEN contract IS A Contract, today IS A DATE
GIVETH A DEONTIC
`payment obligation` MEANS
    IF today >= contract's effectiveDate
    THEN
        PARTY contract's buyer
        MUST `pay` (contract's amount)
        WITHIN 30 days
    ELSE FULFILLED
```

**Combinators:** HENCE (fulfillment), LEST (breach)

---

## ELSE

**Syntax:** `IF cond THEN val1 ELSE val2`
**Semantics:** Alternative branch, required in IF expressions
**Note:** No dangling else, both branches mandatory

---

## EXACTLY

**Syntax:** `MUST EXACTLY expression`
**Semantics:** Changes how the action is matched against incoming events during contract execution. Without EXACTLY, the action is a **pattern** (matched structurally, like `WHEN` in `CONSIDER` — can bind variables). With EXACTLY, the action is an **expression** that is evaluated to a value and compared for **equality** against the event.
**Haskell analogy:** Pattern match (`WHEN Constructor args`) vs equality guard (`| val == expr`)
**See also:** PROVIDED (guard condition on action match)

```l4
-- Without EXACTLY: "pay" is a pattern, matches any pay-shaped event
PARTY buyer MUST pay

-- With EXACTLY: expression is evaluated, event must equal the result
PARTY lender MUST EXACTLY send capital to borrower
```

---

## PROVIDED

**Syntax:** `MUST action PROVIDED condition`
**Semantics:** Guard condition on a deontic action. After an event matches the action pattern, the PROVIDED expression is evaluated. If it returns FALSE, the match is rejected and the system tries the next event. Defaults to TRUE if omitted.
**Haskell analogy:** Guard in pattern matching (`| condition`)
**See also:** EXACTLY (controls pattern vs equality matching of the action itself)

```l4
-- Only matches if the transferred amount meets the threshold
PARTY borrower
MUST `Amount Transferred`
  PROVIDED `is money at least equal` `Amount Transferred` `Payment Due`
```

---

## FULFILLED

**Syntax:** `FULFILLED`
**Type:** `DEONTIC`
**Semantics:** Terminal deontic value — all obligations satisfied, no further action needed
**Use:** Base case in conditional deontic rules, or after successful completion

```l4
IF NOT `conditions precedent are met`
THEN FULFILLED
ELSE PARTY lender MUST ...
```

---

## GIVEN

**Syntax:** `GIVEN param1 IS A Type1, param2 IS A Type2 ...`
**Semantics:** Function parameter declarations with types
**Haskell:** Left side of `::`
**Other languages:** Function parameters (all languages), type annotations (TypeScript/Python)
**Note:** All parameters explicit (functional purity), no hidden dependencies
**Positional:** Parameters are positional from top to bottom in GIVEN block

```l4
GIVEN applicant IS A Person
      today IS A DATE
      jurisdiction IS A STRING
GIVETH A BOOLEAN
```

---

## GIVETH / GIVES

**Syntax:** `GIVETH Type` or `GIVES Type`
**Semantics:** Function return type declaration
**Haskell:** Right side of `::`
**Optional:** Type inference works without it, but recommended for clarity
**Note:** Linter may flag missing type annotations

---

## HENCE

**Syntax:** `... HENCE continuation`
**Semantics:** What happens on the "success" path of a deonton. The meaning of "success" depends on the deontic modal:

| Modal   | HENCE triggers when...                  | Default if omitted |
| ------- | --------------------------------------- | ------------------ |
| `DO`    | action is taken                         | _(required)_       |
| `MUST`  | action is taken                         | `FULFILLED`        |
| `MAY`   | action is taken                         | `FULFILLED`        |
| `SHANT` | deadline passes (prohibition respected) | `FULFILLED`        |

**See Also:** LEST (the "failure" path), DO, MUST, MAY, SHANT

```l4
PARTY buyer MUST pay seller
HENCE PARTY seller MUST deliver goods to buyer
```

---

## IF / THEN

**Syntax:** `IF condition THEN trueVal ELSE falseVal`
**Type:** `BOOLEAN -> a -> a -> a`
**Semantics:** Conditional expression (ternary)
**Note:** Expression not statement, both branches required

```l4
status MEANS IF age >= 18 THEN "adult" ELSE "minor"
```

**Nested:** Use BRANCH instead to avoid indentation errors

---

## IMPORT

**Syntax:** `IMPORT modulename`
**Semantics:** Load library from search path (VFS, relative, XDG ~/.local/share/jl4/libraries/, bundled)
**Common:** `IMPORT prelude` for list functions (map, filter, all, any, etc.)
**Libraries:** prelude, daydate, math, currency, legal-persons, datetime, time, timezone, actus

---

## IS ONE OF

**Syntax:** `DECLARE TypeName IS ONE OF Ctor1, Ctor2 HAS field IS A Type, ...`
**Semantics:** Sum type / discriminated union / algebraic data type. Constructors separated by commas or newlines (NOT pipes `|`).
**Haskell:** `data TypeName = Ctor1 | Ctor2 { field :: Type }`
**Use:** Pattern match with CONSIDER
**Note:** Constructor fields require HAS (not positional bare types)

---

## LET / IN

**Syntax:** `LET bindings IN expression`
**Semantics:** Local bindings scoped to expression (alternative to WHERE)
**Haskell:** `let ... in ...`
**Other languages:** `let` (JS/Rust), local variables (Python/Java)
**See also:** WHERE (postfix variant)

```l4
result MEANS
    LET x MEANS a + b
        y MEANS c * d
    IN  x + y
```

---

## LEST

**Syntax:** `... LEST continuation`
**Semantics:** What happens on the "failure" path of a deonton. The meaning of "failure" depends on the deontic modal:

| Modal   | LEST triggers when...                      | Default if omitted |
| ------- | ------------------------------------------ | ------------------ |
| `DO`    | deadline passes                            | _(required)_       |
| `MUST`  | deadline passes without action             | `BREACH`           |
| `MAY`   | deadline passes (permission not exercised) | `FULFILLED`        |
| `SHANT` | action is taken (prohibition violated)     | `BREACH`           |

**See Also:** HENCE (the "success" path), DO, MUST, MAY, SHANT

```l4
PARTY buyer MUST pay WITHIN 30
LEST PARTY buyer MUST `pay with penalty`
```

**Note:** SHANT flips the polarity — for prohibitions, the _action happening_ is the failure case (LEST), while the _deadline passing without action_ is success (HENCE).

---

## LIST

**Type:** `LIST OF Type`
**Syntax:** `LIST elem1, elem2, ...` or `EMPTY`
**Haskell:** `[Type]`
**Operations:** Prelude functions (map, filter, all, any, null, elem)
**Pattern match:** `WHEN EMPTY` / `WHEN first FOLLOWED BY rest`

```l4
numbers MEANS LIST 1, 2, 3
all (GIVEN n YIELD n > 0) numbers
```

---

## MAYBE

**Type:** `MAYBE Type`
**Semantics:** Optional value type
**Haskell:** `Maybe Type`
**Values:** `NOTHING` or `JUST value`
**Pattern match:** `WHEN NOTHING THEN ...` / `WHEN JUST x THEN ...`

```l4
DECLARE Person HAS
    name IS A STRING
    email IS A MAYBE STRING

CONSIDER person's email
WHEN NOTHING THEN "no email"
WHEN JUST addr THEN addr
```

---

## MAY

**Syntax:** `PARTY actor MAY action ...`
**Type:** `DEONTIC`
**Semantics:** Permission (deontic modality)
**Modal logic:** Diamond operator (◇)

---

## MEANS

**Syntax:** `identifier MEANS expression`
**Type:** `GIVEN inputs GIVETH T`
**Semantics:** Function definition / let binding
**Haskell:** `identifier = expression`
**Other languages:** `def` (Python), `function` (JS), `fn` (Rust), `=` (Haskell)
**vs DECIDE:** Semantically same, MEANS reads like "equals", DECIDE like "determines"
**Mixfix:** Parameters can appear in identifier: `mom and dad `have a baby named` kid MEANS ...`

```l4
GIVEN x IS A NUMBER, y IS A NUMBER
GIVETH A NUMBER
total MEANS x + y
```

---

## MODULO

**Syntax:** `expr MODULO expr`
**Type:** `NUMBER -> NUMBER -> NUMBER`
**Semantics:** Remainder after division
**Other languages:** `%` (C/JS/Python), `mod` (Haskell), `rem` (Clojure)

```l4
DECIDE `is even` IF n MODULO 2 EQUALS 0
```

---

## MUST

**Syntax:** `PARTY actor MUST action ...`
**Type:** `DEONTIC`
**Semantics:** Obligation (deontic modality)
**Legal:** "shall", "is required to", "is obligated to"
**Modal logic:** Box operator (□)

```l4
PARTY tenant
MUST `pay rent`
WITHIN 5 days
```

---

## NOT

**Syntax:** `NOT expr`
**Type:** `BOOLEAN -> BOOLEAN`
**Semantics:** Logical negation
**Precedence:** Highest
**See also:** `UNLESS`

---

## OF

**Syntax:** Varies by context (see below)
**Semantics:** Multi-purpose structural keyword that introduces comma-separated argument lists. Without OF, arguments must be space-separated on the same line or indented on subsequent lines.

**Contexts where OF appears:**

| Context                          | Syntax                      | Example                                    |
| -------------------------------- | --------------------------- | ------------------------------------------ |
| Sum type declaration             | `IS ONE OF`                 | `DECLARE Color IS ONE OF Red, Green, Blue` |
| Type constructor                 | `LIST OF Type`              | `field IS A LIST OF Person`                |
| Record construction (positional) | `Type OF val1, val2`        | `Pair OF 10, 20`                           |
| Function application             | `fname OF arg1, arg2`       | `add OF 3, 4`                              |
| Pattern matching                 | `Constructor OF pat1, pat2` | `WHEN Pair OF x, y THEN ...`               |
| Function type                    | `FUNCTION FROM T1 TO T2`    | (OF not used here, but related)            |

**Key insight:** OF enables comma-separated arguments on the same line. Without OF, you must use space-separated arguments or indented continuation lines.

```l4
-- WITH OF: comma-separated args on one line
result1 MEANS add OF 3, 4
result2 MEANS foldr OF add, 0, numbers

-- WITHOUT OF: space-separated on same line
result3 MEANS add 3 4

-- Record construction: OF (positional) vs WITH (named)
pair1 MEANS Pair OF 10, 20                         -- positional, fragile
pair2 MEANS Pair WITH first IS 10, second IS 20    -- named, resilient
```

**Note:** OF is optional in function application — `add OF 3, 4` and `add 3 4` are equivalent. But for multi-argument calls, OF + commas is often clearer than relying on whitespace parsing.

**Caution:** "NOT OF" is valid syntax (`NOT OF expr`) but OF is optional there — `NOT expr` works the same way. (As one linguist noted, "NOT OF" reads like the Americanism "off of".)

---

## OR

**Syntax:** `expr OR expr`
**Type:** `BOOLEAN -> BOOLEAN -> BOOLEAN`
**Semantics:** Logical disjunction
**Aliases:** `||`
**Precedence:** Lower than AND

---

## OTHERWISE

**Syntax:** Terminal case in BRANCH or CONSIDER
**Semantics:** Default branch when no patterns match
**Required:** Best practice, unless exhaustive

---

## PARTY

**Syntax:** `PARTY expression`
**Semantics:** Actor in regulative rule (deontic subject)
**Type:** Any type, typically Person/Entity, according to the type definition of the deonton

---

## RAND (Regulative AND / Parallel Conjunction)

**Syntax:** `deonton1 RAND deonton2`
**Type:** `DEONTIC -> DEONTIC -> DEONTIC`
**Semantics:** Parallel conjunction of deontic obligations — ALL must be fulfilled for the compound to be fulfilled. Short-circuit evaluation: if either side breaches, the whole breaches.
**Precedence:** 3 (higher than ROR, lower than comparisons)
**Associativity:** Right
**Theory:** Corresponds to Hvitved's subcontract conjunction in CSL (Contract Specification Language). In concurrency theory terms, this is parallel composition where all threads must complete successfully.
**See also:** ROR (disjunctive choice), HENCE, LEST

```l4
-- Both obligations must be fulfilled
(PARTY seller MUST deliver WITHIN 14 HENCE FULFILLED LEST BREACH)
RAND
(PARTY buyer MUST pay WITHIN 30 HENCE FULFILLED LEST BREACH)
```

---

## ROR (Regulative OR / Disjunctive Choice)

**Syntax:** `deonton1 ROR deonton2`
**Type:** `DEONTIC -> DEONTIC -> DEONTIC`
**Semantics:** Disjunctive choice between deontic obligations — EITHER fulfilling suffices for the compound to be fulfilled. Short-circuit evaluation: if either side fulfills, the whole fulfills.
**Precedence:** 2 (lower than RAND)
**Associativity:** Right
**Theory:** Corresponds to Hvitved's subcontract disjunction in CSL. In concurrency theory terms, this is a race where the first to complete determines the outcome.
**See also:** RAND (parallel conjunction), HENCE, LEST

```l4
-- Either obligation can fulfill the contract
(PARTY seller MUST ship WITHIN 14 HENCE FULFILLED LEST BREACH)
ROR
(PARTY seller MUST `arrange pickup` WITHIN 7 HENCE FULFILLED LEST BREACH)
```

**RAND vs ROR summary:**

- `A RAND B` = both A and B must succeed (like logical AND for obligations)
- `A ROR B` = either A or B succeeding suffices (like logical OR for obligations)
- RAND binds tighter than ROR, so `A ROR B RAND C` means `A ROR (B RAND C)`

---

## § (Section Scope)

**Syntax:** `§ SectionName` or `§§ SubsectionName`
**Semantics:** Nested scoping within a file, like sections in legislation. Same-named definitions in different sections do NOT shadow each other — the compiler creates fully qualified name bindings for disambiguation.
**Use:** Organize definitions into logical sections
**Other languages:** namespace (C++), module (Python), package (Java)
**Note:** Multiple § levels for nesting depth (§, §§, §§§)

```l4
§ Definitions
  DECLARE Person HAS name IS A STRING

§ Eligibility Rules
  GIVEN p IS A Person
  DECIDE `is eligible` IF ...
```

**Same name, different sections:** Definitions don't shadow — they coexist and consumers must qualify to disambiguate. This parallels how legislation scopes definitions: "for purposes of subsection 2, X means ...".

```l4
§ `Part VII`

§§ `Subsection 2`
`age of majority` MEANS 18

§§ `Subsection 3`
`age of majority` MEANS 21

§ Application

-- Consumer must qualify: a 19-year-old is an adult
-- under Subsection 2 (age 18) but not Subsection 3 (age 21)
GIVEN age IS A NUMBER
GIVETH A BOOLEAN
DECIDE `is adult for purposes of subsection 2` IF
    age >= `Part VII`'s `Subsection 2`'s `age of majority`

GIVEN age IS A NUMBER
GIVETH A BOOLEAN
DECIDE `is adult for purposes of subsection 3` IF
    age >= `Part VII`'s `Subsection 3`'s `age of majority`

#EVAL `is adult for purposes of subsection 2` 19  -- TRUE
#EVAL `is adult for purposes of subsection 3` 19  -- FALSE
```

**Note:** Sections provide namespacing, not lexical scoping. A function defined inside a section does NOT automatically prefer its own section's bindings — if the same name exists in multiple sections, you must always qualify. Dot notation also works: `` `Part VII`.`Subsection 2`.`age of majority` ``.

**Section aliases (AKA):** Provide shorter names for qualified references:

```l4
§ `Definitions for Part VII` AKA defs
  taxableIncome MEANS 50000

result MEANS defs.taxableIncome   -- via alias
```

---

## SHANT

**Syntax:** `PARTY actor SHANT action ...`
**Type:** `DEONTIC`
**Semantics:** Prohibition (deontic modality)
**Legal:** "shall not", "must not", "is prohibited from"
**Modal logic:** Negated box ¬□

---

## Arithmetic Operators

L4 provides both symbolic and keyword forms for arithmetic. All keyword forms are uppercase. Keyword and symbol forms are interchangeable.

| Keyword(s)   | Symbol | Meaning        | Precedence  |
| ------------ | ------ | -------------- | ----------- |
| `TIMES`      | `*`    | Multiplication | 7 (highest) |
| `DIVIDED BY` | `/`    | Division       | 7           |
| `MODULO`     | —      | Remainder      | 7           |
| `PLUS`       | `+`    | Addition       | 6           |
| `MINUS`      | `-`    | Subtraction    | 6           |

**Note:** `DIVIDED BY` is two keywords (`DIVIDED` + `BY`). `+` works for numbers only, not strings (use CONCAT or APPEND for strings).

```l4
accrued MEANS capital PLUS capital TIMES interest
share MEANS total DIVIDED BY count
remainder MEANS n MODULO 2
```

---

## Comparison Operators

| Keyword(s)     | Symbol | Meaning          | Precedence |
| -------------- | ------ | ---------------- | ---------- |
| `EQUALS`       | `=`    | Equality         | 4          |
| `GREATER THAN` | `>`    | Greater than     | 4          |
| `ABOVE`        | —      | Synonym for `>`  | 4          |
| `LESS THAN`    | `<`    | Less than        | 4          |
| `BELOW`        | —      | Synonym for `<`  | 4          |
| `AT LEAST`     | `>=`   | Greater or equal | 4          |
| `AT MOST`      | `<=`   | Less or equal    | 4          |

**Note:** L4 uses `=` for equality (NOT `==`). `ABOVE`/`BELOW` are semantic synonyms for `GREATER THAN`/`LESS THAN`, useful for natural-language readability in legal contexts (e.g. "income ABOVE threshold").

```l4
DECIDE `is adult` IF age AT LEAST 18
DECIDE `is minor` IF age BELOW 18
DECIDE `exceeds limit` IF amount ABOVE 10000
```

---

## TO

**Syntax:** `FUNCTION FROM Type1 TO Type2`
**Semantics:** Used in function type annotations (`FUNCTION FROM T1 AND T2 TO T3`).
**Note:** `TO` is a reserved keyword but is **not** a deontic beneficiary marker. In deontic rules, `to` appearing in the action (e.g. `deliver goods to buyer`) is part of the mixfix expression, not a keyword.

---

## WHEN

**Syntax:** `WHEN pattern THEN result`
**Semantics:** Pattern matching arm in CONSIDER
**Haskell:** `pattern -> result`

---

## WHERE

**Syntax:** `expression WHERE binding1, binding2, ...`
**Semantics:** Local let bindings scoped to expression
**Haskell:** `where` clause
**Use:** Break down complex calculations with named intermediates

```l4
totalCost MEANS
    baseCost + taxAmount + fees
    WHERE
        baseCost MEANS price * quantity
        taxAmount MEANS baseCost * taxRate
        fees MEANS IF express THEN 10 ELSE 0
```

**Scoping:** Bindings in WHERE can reference each other and outer scope

---

## TOSTRING

**Syntax:** `TOSTRING(expr)`
**Type:** `a -> STRING`
**Semantics:** Type coercion to string representation
**Other languages:** `str()` (Python), `toString()` (Java/JS), `show` (Haskell)
**See also:** TODATE, TOTIME, TODATETIME for reverse coercions

```l4
message MEANS CONCAT person's name, " owes ", TOSTRING(amount)
```

---

## UNLESS

**Syntax:** `condition UNLESS exception`
**Semantics:** Equivalent to `condition AND NOT exception`
**Use:** Natural language legal phrasing — "X unless Y"
**Legal:** Maps directly to common legislative drafting pattern

```l4
DECIDE `is eligible` IF
        person's age >= 18
    AND `has valid ID`
 UNLESS  `is disqualified`
-- equivalent to: age >= 18 AND hasValidID AND NOT isDisqualified
```

---

## WITHIN

**Syntax:** `... WITHIN duration [OF event]`
**Semantics:** Temporal deadline as relative duration in deontic rule
**See also:** BEFORE (planned, for absolute deadlines — not yet implemented)
**Examples:** `WITHIN 30 days`, `WITHIN 5 days OF notice`

---

## #EVAL (Directive)

**Syntax:** `#EVAL expression`
**Semantics:** Evaluate expression and display result (testing/debugging)
**Use:** At end of file to test functions

```l4
#EVAL `is adult` 21        -- evaluates to TRUE
#EVAL `tax owed` 75000     -- evaluates to the computed number
```

---

## #TRACE (Directive)

**Syntax:** `#TRACE expression`
**Semantics:** Evaluate deontic expression and display obligation trace
**Use:** Debug regulative rules, inspect obligation graph

```l4
#TRACE `Loan Contract` myLender myBorrower 10000 0.05 ...
-- Shows: PARTY lender MUST send 10000 to borrower WITHIN ...
--        HENCE PARTY borrower MUST send 10500 to lender WITHIN ...
```

---

## Mixfix Syntax

**Syntax:** Parameters interspersed with identifier words in MEANS/DECIDE line
**Semantics:** Infix/postfix function application without special delimiters
**Haskell:** Backtick infix: ``a `f` b``
**Use:** Natural language readability for legal rules

```l4
GIVEN mom IS A STRING
      dad IS A STRING
      kid IS A STRING
mom and dad `have a baby named` kid MEANS
    CONCAT "mother: ", mom, ", father: ", dad, ", child: ", kid

#EVAL "alice" and "bob" `have a baby named` "carol"
```

**Note:** Backtick-delimited parts are the function name; bare words are positional parameters from GIVEN.

---

## FAQ: Deontic Guards — IF vs PROVIDED

**Q: When should I use IF (outside) vs PROVIDED (inside) a deontic rule?**

They serve different purposes:

|                | IF (outside)                      | PROVIDED (inside)                                        |
| -------------- | --------------------------------- | -------------------------------------------------------- |
| **Controls**   | Whether the obligation **exists** | Which events **satisfy** the obligation                  |
| **When false** | No obligation arises              | Obligation exists but event doesn't count as fulfillment |
| **Analogy**    | Precondition / applicability test | Pattern match guard / filter                             |

**Pattern:** Deontic rules typically emerge from a MEANS or DECIDE definition, with IF as a precondition:

```l4
-- IF controls whether the obligation exists at all
GIVEN `amount owed` IS A NUMBER
`payment obligation` MEANS
    IF `amount owed` > 0
    THEN
        PARTY borrower
        MUST pay
            `Amount Transferred`
            PROVIDED `Amount Transferred` >= `amount owed`
        WITHIN 30
        HENCE `payment obligation` (`amount owed` - `Amount Transferred`)
    ELSE FULFILLED
```

Here, IF tests whether money is still owed (no obligation if fully paid). PROVIDED filters which payment events count (must be at least the amount owed). The obligation exists as long as `amount owed > 0`, but only a sufficiently large payment satisfies it.

---

## FAQ: HENCE/LEST Defaults by Deontic Modal

**Q: What happens if I omit HENCE or LEST?**

The defaults depend on which deontic modal you use. MUST/MAY/SHANT are syntactic sugar over the primitive `DO`, which requires both HENCE and LEST explicitly:

| Modal   | HENCE triggers  | Default HENCE | LEST triggers   | Default LEST |
| ------- | --------------- | ------------- | --------------- | ------------ |
| `DO`    | action taken    | _(required)_  | deadline passes | _(required)_ |
| `MUST`  | action taken    | `FULFILLED`   | deadline passes | `BREACH`     |
| `MAY`   | action taken    | `FULFILLED`   | deadline passes | `FULFILLED`  |
| `SHANT` | deadline passes | `FULFILLED`   | action taken    | `BREACH`     |

Key observations:

- **SHANT flips polarity:** For prohibitions, success = nothing happened; failure = action was taken.
- **MAY never breaches:** Both paths lead to FULFILLED — the party is merely permitted, not obligated.
- **DO is neutral:** No default moral weight; you must specify both outcomes explicitly.
- **MUST is the common case:** Do it or breach.

---

## Search Terms

**Functional programming:** algebraic data type, sum type, product type, pattern matching, pure function, type signature, lazy evaluation, currying, partial application, higher-order function, closure, lambda, anonymous function, immutable, referential transparency

**Formal methods:** modal logic, deontic logic, temporal logic, constitutive rule, regulative rule, obligation, permission, prohibition, formal verification, model checking, property assertion, Hohfeld, Searle, LegalRuleML, Hvitved, CSL, contract specification language, subcontract conjunction, subcontract disjunction, parallel composition, concurrency

**Haskell correspondence:** case expression → CONSIDER, where clause → WHERE, let-in → LET...IN, data declaration → DECLARE, Maybe monad → MAYBE (constructors: NOTHING/JUST, not Nothing/Just), guards → BRANCH, pattern matching → CONSIDER/WHEN, type class → (not yet), do-notation → (not yet), record syntax → DECLARE HAS, record access → 's genitive/possessive

**Other language mappings:** struct/class/interface → DECLARE HAS, enum/union/variant/tagged union → IS ONE OF, def/fn/function/method → MEANS or DECIDE, switch/case/match → CONSIDER or BRANCH, if-else/ternary/conditional → IF THEN ELSE, lambda/arrow function/(x) => → GIVEN...YIELD, let/const/var → WHERE or LET...IN, null/nil/None/undefined/optional → MAYBE, array/list/vector/sequence → LIST OF, dot notation/property access/.field → 's genitive/possessive, string concat/join/+ → CONCAT, modulo/remainder/% → MODULO, toString/str/show/cast → TOSTRING, import/require/include/use → IMPORT, namespace/module/package/scope → § sections

**L4-specific syntax:** mixfix (infix/postfix function application), caret ^ (ditto operator / vertical alignment / repeat token from previous line / ellipsis / gapping / stripping / coordination reduction), `...` three-dot ellipsis (implicit AND / asyndetic conjunction), `..` two-dot ellipsis (implicit OR / asyndetic disjunction), inert elements (bare strings as boolean scaffolding / no-op / identity element / annotation), backtick delimiters for multi-word identifiers, UNLESS (= AND NOT), 's genitive/possessive/Saxon genitive/genitive clitic/possessive clitic (= dot notation / record field access / member access / property access / projection / dereference), § section scoping (= namespace / module / qualified names), EXACTLY (strict deontic action), FULFILLED (terminal deontic value / base case), OF (comma-separated argument lists / function application / record construction / type constructors)

**Type theory:** arrow type, unit type, product, coproduct, elimination form, constructor, introduction form, bidirectional type checking, Hindley-Milner, parametric polymorphism

**Legal/compliance:** shall, must, may, may not, shall not, obligation, permission, prohibition, entitlement, deadline, penalty, breach, fulfillment, liability, indemnity, contract, legislation, regulation, statutory instrument, rules as code, computational law

**Arithmetic operators:** PLUS (+), MINUS (-), TIMES (\*), DIVIDED BY (/), MODULO (%), EQUALS (=), GREATER THAN (>), LESS THAN (<), AT LEAST (>=), AT MOST (<=), ABOVE (> synonym), BELOW (< synonym)

**Boolean operators:** AND (&&), OR (||), NOT (!), IMPLIES (=>), UNLESS (AND NOT)

**Deontic composition:** RAND (parallel conjunction / all must fulfill / subcontract AND), ROR (disjunctive choice / either suffices / subcontract OR), Hvitved CSL, contract composition, concurrent obligations
