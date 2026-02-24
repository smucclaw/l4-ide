# L4 Keywords Reference

Dense technical specs. Each section: syntax, type, semantics, FP analog, example.

**Cross-language cheat sheet:** struct/class/interface → DECLARE HAS. enum/union/variant → IS ONE OF. def/fn/function → MEANS or DECIDE. switch/case/match → CONSIDER or BRANCH. ternary/if-else → IF THEN ELSE. lambda/arrow function/closure → GIVEN...YIELD. let/const/local variable → WHERE or LET...IN. null/nil/None/optional/nullable → MAYBE. array/list/sequence → LIST OF. dot notation/property access/member access → `'s` possessive. string concatenation/join → CONCAT. modulo/remainder/mod → MODULO. toString/str()/cast → TOSTRING. for-each/forEach/all/every → `all`/`any` (IMPORT prelude). namespace/scope → § sections.

---

## 's (Possessive / Field Access)

**Syntax:** `record's fieldname`
**Semantics:** Record field access (dot notation equivalent)
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
**Semantics:** Refers to the token in the same position from the previous line
**Use:** Vertical alignment for human readability in boolean expressions
**No equivalent in most languages** - unique to L4

```l4
`tautology` MEANS
        p EQUALS TRUE
    OR  p EQUALS FALSE
   AND  q ^      TRUE    -- ^ means EQUALS (from previous line)
    OR  q ^      FALSE
```

---

## AND

**Syntax:** `expr AND expr`
**Type:** `BOOLEAN -> BOOLEAN -> BOOLEAN`
**Semantics:** Logical conjunction
**Aliases:** `&&`
**Precedence:** Higher than OR, lower than NOT

```l4
age >= 18 AND isCitizen
```

---

## BEFORE

**Syntax:** `... BEFORE deadline`
**Semantics:** Temporal deadline in deontic rule (alternative to WITHIN)
**Use:** When deadline is a specific date/event, not a duration
**See also:** WITHIN (for relative durations)

```l4
PARTY lender
MUST send capital to borrower
BEFORE closing
```

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

---

## CONCAT

**Syntax:** `CONCAT expr1, expr2, ...`
**Type:** `STRING -> STRING -> ... -> STRING`
**Semantics:** String concatenation of multiple arguments
**Other languages:** `+` (JS/Python), `++` (Haskell), `strcat` (C), `.` (PHP)

```l4
greeting MEANS CONCAT "Hello, ", person's name, "!"
```

---

## CONSIDER

**Syntax:** `CONSIDER expr WHEN pattern THEN result ... OTHERWISE default`
**Type:** Pattern match on expr type
**Semantics:** Pattern matching elimination form
**Haskell:** `case ... of`
**Other languages:** switch/case (C/Java), match (Rust/Python 3.10+), when (Kotlin)
**Use:** Sum types, Maybe, List destructuring

```l4
CONSIDER status
WHEN Active THEN "running"
WHEN Suspended reason THEN "paused: " + reason
WHEN Closed THEN "stopped"
```

**Patterns:**
- Constructor: `WHEN Active THEN ...`
- Constructor + arg: `WHEN Suspended x THEN ...`
- List empty: `WHEN EMPTY THEN ...`
- List cons: `WHEN head FOLLOWED BY tail THEN ...`
- Maybe: `WHEN Nothing THEN ...` / `WHEN Just x THEN ...`

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
         `DECLARE TypeName IS ONE OF Ctor1 | Ctor2 Type | ...` (sum)
**Semantics:** Algebraic data type definition
**Haskell:** `data TypeName = TypeName { field1 :: Type1, ... }` or `data TypeName = Ctor1 | Ctor2 Type`

```l4
-- Product type (record)
DECLARE Person HAS
    name IS A STRING
    age IS A NUMBER
    status IS A Status

-- Sum type (enum)
DECLARE Status IS ONE OF
    Active
    | Suspended STRING
    | Closed

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
    PARTY contract's buyer
    MUST `pay` (contract's amount)
    TO contract's seller
    WITHIN 30 days
    IF today >= contract's effectiveDate
```

**Combinators:** HENCE (fulfillment), LEST (breach)

---

## ELSE

**Syntax:** `IF cond THEN val1 ELSE val2`
**Semantics:** Alternative branch, required in IF expressions
**Note:** No dangling else, both branches mandatory

---

## EXACTLY

**Syntax:** `MUST EXACTLY action`
**Semantics:** Strict obligation modifier — action must be performed precisely as specified
**Use:** In deontic rules where the action is not merely a description but a precise requirement

```l4
PARTY lender MUST EXACTLY send capital to borrower
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

**Syntax:** `... HENCE fulfilledRule`
**Semantics:** Chained obligation on fulfillment
**Use:** Sequential deontic rules

```l4
PARTY buyer MUST pay TO seller
HENCE PARTY seller MUST deliver TO buyer
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

**Syntax:** `DECLARE TypeName IS ONE OF Ctor1 | Ctor2 Type | ...`
**Semantics:** Sum type / discriminated union / algebraic data type
**Haskell:** `data TypeName = Ctor1 | Ctor2 Type`
**Use:** Pattern match with CONSIDER

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

**Syntax:** `... LEST breachRule`
**Semantics:** Chained obligation on breach/failure
**Use:** Penalty clause, exception handling, breach consequence in deontic rules

```l4
PARTY buyer MUST pay WITHIN 30 days
LEST PARTY buyer MUST `pay penalty` TO seller
```

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
**Values:** `Nothing` or `Just value`
**Pattern match:** `WHEN Nothing THEN ...` / `WHEN Just x THEN ...`

```l4
DECLARE Person HAS
    name IS A STRING
    email IS A MAYBE STRING

CONSIDER person's email
WHEN Nothing THEN "no email"
WHEN Just addr THEN addr
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

**Syntax:** `PARTY actor MUST action TO beneficiary ...`
**Type:** `DEONTIC`
**Semantics:** Obligation (deontic modality)
**Legal:** "shall", "is required to", "is obligated to"
**Modal logic:** Box operator (□)

```l4
PARTY tenant
MUST `pay rent`
TO landlord
WITHIN 5 days OF `start of month`
```

---

## NOT

**Syntax:** `NOT expr`
**Type:** `BOOLEAN -> BOOLEAN`
**Semantics:** Logical negation
**Precedence:** Highest

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
**Type:** Any type, typically Person/Entity

---

## § (Section Scope)

**Syntax:** `§ SectionName` or `§§ SubsectionName`
**Semantics:** Nested scoping within a file, like sections in legislation
**Use:** Organize definitions into logical sections, control visibility
**Other languages:** namespace (C++), module (Python), package (Java)
**Note:** Multiple § levels for nesting depth (§, §§, §§§)

```l4
§ Definitions
  DECLARE Person HAS name IS A STRING

§ Eligibility Rules
  GIVEN p IS A Person
  DECIDE `is eligible` IF ...
```

---

## SHANT

**Syntax:** `PARTY actor SHANT action ...`
**Type:** `DEONTIC`
**Semantics:** Prohibition (deontic modality)
**Legal:** "shall not", "must not", "is prohibited from"
**Modal logic:** Negated box ¬□

---

## TIMES / PLUS (Arithmetic Operator Aliases)

**Syntax:** `expr TIMES expr`, `expr PLUS expr`
**Semantics:** Natural language aliases for arithmetic operators
**Mappings:** `TIMES` = `*`, `PLUS` = `+`, `MINUS` = `-`

```l4
DECIDE `twice of` IS 2 TIMES n
accrued MEANS capital PLUS capital * interest
```

---

## TO

**Syntax:** `... TO beneficiary`
**Semantics:** Beneficiary of deontic action
**Optional:** In MUST/MAY/SHANT

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
**See also:** BEFORE (for absolute deadlines)
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
-- Shows: PARTY lender MUST send 10000 to borrower BEFORE ...
--        HENCE PARTY borrower MUST send 10500 to lender BEFORE ...
```

---

## Mixfix Syntax

**Syntax:** Parameters interspersed with identifier words in MEANS/DECIDE line
**Semantics:** Infix/postfix function application without special delimiters
**Haskell:** Backtick infix: `` a `f` b ``
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

## Search Terms

**Functional programming:** algebraic data type, sum type, product type, pattern matching, pure function, type signature, lazy evaluation, currying, partial application, higher-order function, closure, lambda, anonymous function, immutable, referential transparency

**Formal methods:** modal logic, deontic logic, temporal logic, constitutive rule, regulative rule, obligation, permission, prohibition, formal verification, model checking, property assertion, Hohfeld, Searle, LegalRuleML

**Haskell correspondence:** case expression → CONSIDER, where clause → WHERE, let-in → LET...IN, data declaration → DECLARE, Maybe monad → MAYBE, guards → BRANCH, pattern matching → CONSIDER/WHEN, type class → (not yet), do-notation → (not yet), record syntax → DECLARE HAS, record access → 's possessive

**Other language mappings:** struct/class/interface → DECLARE HAS, enum/union/variant/tagged union → IS ONE OF, def/fn/function/method → MEANS or DECIDE, switch/case/match → CONSIDER or BRANCH, if-else/ternary/conditional → IF THEN ELSE, lambda/arrow function/(x) => → GIVEN...YIELD, let/const/var → WHERE or LET...IN, null/nil/None/undefined/optional → MAYBE, array/list/vector/sequence → LIST OF, dot notation/property access/.field → 's possessive, string concat/join/+ → CONCAT, modulo/remainder/% → MODULO, toString/str/show/cast → TOSTRING, import/require/include/use → IMPORT, namespace/module/package/scope → § sections

**L4-specific syntax:** mixfix (infix/postfix function application), caret ^ (ditto operator / vertical alignment / repeat token from previous line), backtick delimiters for multi-word identifiers, UNLESS (= AND NOT), 's possessive (= dot notation / record field access / member access / property access), § section scoping (= namespace / module), EXACTLY (strict deontic action), FULFILLED (terminal deontic value / base case)

**Type theory:** arrow type, unit type, product, coproduct, elimination form, constructor, introduction form, bidirectional type checking, Hindley-Milner, parametric polymorphism

**Legal/compliance:** shall, must, may, may not, shall not, obligation, permission, prohibition, entitlement, deadline, penalty, breach, fulfillment, liability, indemnity, contract, legislation, regulation, statutory instrument, rules as code, computational law

**Arithmetic operators:** PLUS (+), MINUS (-), TIMES (*), MODULO (%), EQUALS (=), GREATER THAN (>), LESS THAN (<), AT LEAST (>=), AT MOST (<=)

**Boolean operators:** AND (&&), OR (||), NOT (!), IMPLIES (=>), UNLESS (AND NOT)
