# L4 Keywords Reference

Dense technical specs. Each section: syntax, type, semantics, FP analog, example.

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

## BRANCH

**Syntax:** `BRANCH IF cond1 THEN val1 ... OTHERWISE default`
**Type:** `GIVETH T` (all branches same type)
**Semantics:** Multi-way conditional, top-to-bottom, first match wins
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

## CONSIDER

**Syntax:** `CONSIDER expr WHEN pattern THEN result ... OTHERWISE default`
**Type:** Pattern match on expr type
**Semantics:** Pattern matching elimination form
**Haskell:** `case ... of`
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

## GIVEN

**Syntax:** `GIVEN param1 IS A Type1, param2 IS A Type2 ...`
**Semantics:** Function parameter declarations with types
**Haskell:** Left side of `::`
**Note:** All parameters explicit (functional purity), no hidden dependencies

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

---

## IS ONE OF

**Syntax:** `DECLARE TypeName IS ONE OF Ctor1 | Ctor2 Type | ...`
**Semantics:** Sum type / discriminated union / algebraic data type
**Haskell:** `data TypeName = Ctor1 | Ctor2 Type`
**Use:** Pattern match with CONSIDER

---

## LEST

**Syntax:** `... LEST breachRule`
**Semantics:** Chained obligation on breach/failure
**Use:** Exception handling in deontic rules

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
**vs DECIDE:** Semantically same, MEANS reads like "equals", DECIDE like "determines"

```l4
GIVEN x IS A NUMBER, y IS A NUMBER
GIVETH A NUMBER
total MEANS x + y
```

---

## MUST

**Syntax:** `PARTY actor MUST action TO beneficiary ...`
**Type:** `DEONTIC`
**Semantics:** Obligation (deontic modality)
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

## SHANT

**Syntax:** `PARTY actor SHANT action ...`
**Type:** `DEONTIC`
**Semantics:** Prohibition (deontic modality)
**Modal logic:** Negated box ¬□

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

## WITHIN

**Syntax:** `... WITHIN duration [OF event]`
**Semantics:** Temporal deadline in deontic rule
**Examples:** `WITHIN 30 days`, `WITHIN 5 days OF notice`

---

## Search Terms

Functional programming: algebraic data type, sum type, product type, pattern matching, pure function, type signature, lazy evaluation

Formal methods: modal logic, deontic logic, temporal logic, constitutive rule, regulative rule

Haskell: case expression, where clause, data declaration, Maybe monad, list comprehension, guard

Type theory: arrow type, unit type, product, coproduct, elimination form, constructor
