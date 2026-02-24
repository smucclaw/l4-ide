# AI Reference for L4

Terse technical reference optimized for AI assistants, RAG systems, and context-limited LLMs.

## Contents

- [keywords.md](keywords.md) - All keywords with syntax, types, examples
- [patterns.md](patterns.md) - Canonical code templates
- [types.md](types.md) - Type system and FP correspondence
- [errors.md](errors.md) - Common errors and fixes

## L4 in Brief

Domain-specific (specification) language for computational law. Haskell-inspired: layout-sensitive, strongly typed, algebraic data types, pure functional core with an "effect system" for multi-party deontic/temporal modal logics.

**Application domain paradigm:** From Searle, and also LegalRuleML, Constitutive rules (DECIDE/MEANS) + Regulative rules (PARTY MUST/MAY/SHANT action BEFORE deadline). From Haskell, destructuring (CONSIDER), case-like statements (BRANCH), and ternary IF/THEN/ELSE; also LET/IN and WHERE. Module system allows imports and also supports multiple nested section scopes within a file using § symbols.

Uses: automated extraction and refinement into operational runtimes, e.g. code-generation of an expert system web UI; a decision service API to support AI chats, with an evaluation trace facility for audit logs.

## Quick Syntax

L4's standard library defines lists and dictionaries, and some other utility functions expected in the FP paradigm.
``` l4
IMPORT prelude
```

`Backtick delimiters` allow multi-word identifiers containing whitespace characters.

```l4
-- Type definitions
DECLARE T HAS
   field1 IS A Type1
   field2 IS A Type2
DECLARE Status IS ONE OF
   Active
   Suspended
     HAS details      IS A STRING
         `start date` IS A DATE
   Closed

-- Function signatures
GIVEN param1 IS A Type1
      param2 IS A Type2
GIVETH A ReturnType2
identifier1 MEANS some expression

-- alternative style: as above, parameters are automatically positional from the GIVEN; below, they can be, but do not need to be, spelled out in the MEANS line.
GIVEN param1 IS A Type1
      param2 IS A Type2
GIVETH A ReturnType2
identifier2 param1 param2 MEANS some expression

```

Similar to Haskell's infix and postfix syntax for function application, L4 supports mixfix syntax, without requiring backticks to indicate infix positions:


``` l4
-- infix/mixfix syntax
GIVEN mom IS A STRING
      dad IS A STRING
      kid IS A STRING
GIVES A STRING
mom and dad `have a baby named` kid MEANS
  CONCAT "mother: ", mom, "\n"
         "father: ", dad, "\n"
         "child:  ", kid

#EVAL "alice" and "bob" `have a baby named` "carol"
-- returns "mother: alice
--          father: bob
--          child:  carol"


```
Decisions tend to work with booleans, but can operate against, and return, any type. "DECIDE ... IF" and "DECIDE ... IS" are alternative forms for human readability.
``` l4

-- Decisions (boolean predicates)
-- type inference allows omission of the GIVETH return type
GIVEN n IS A NUMBER
DECIDE `is even` IF n MODULO 2 EQUALS 0

-- ordinary arithmetic calculation
GIVEN n IS A NUMBER
DECIDE `twice of` IS 2 TIMES n
```

Where other languages would use parentheses for grouping, L4 uses indentation.

The `UNLESS` keyword means `AND NOT`.

The caret operator means "the token in this position from the previous line", and is used for human readability.

An apostrophe s (`'s`) possessive means record deference.

``` l4

-- while L4's operator precedence follows standard conventions, for clarity, operators within a sub-expression, at a certain indentation depth, should be the same.
GIVEN p IS A BOOLEAN
      q IS A BOOLEAN
`tautology` MEANS
          p EQUALS TRUE
      OR  p EQUALS FALSE
   AND    q ^      TRUE
      OR  q ^      FALSE
 UNLESS   p EQUALS q AND p EQUALS NOT q
-- equivalent to (p || ! p) && (q || ! q) && ! (p == q && p == ! q)
 
-- Pattern matching / destructuring
CONSIDER value
WHEN Constructor args THEN result
OTHERWISE default

-- Regulative rules are a graph of atomic "deontons":
GIVEN borrower  IS A Person
      lender    IS A Person
      capital   IS A NUMBER -- in practice we would import the `currency` library
      interest  IS A NUMBER
      closing   IS A NUMBER -- [TODO] -- this needs to become a DATE
      repayment IS A DATE   -- ditto. see the mengwong/deadlines branch
      `conditions precedent are met` IS A BOOLEAN
`Loan Contract` MEANS
  IF     NOT `conditions precedent are met`
  THEN   FULFILLED
  ELSE   PARTY lender
         MUST  EXACTLY send capital to borrower
         BEFORE closing
         HENCE    PARTY  borrower
                  MUST   EXACTLY send accrued to lender
                  BEFORE repayment

  WHERE
    send money to person MEANS
      CONCAT "an event appears in the trace reflecting that "
             person's name   -- person.name in any other language
             " receives "
             TOSTRING(money) -- type coercion, an inbuilt
    accrued MEANS
           capital
      PLUS ^       * interest

DECLARE Person
  HAS name IS A STRING
```

## For Humans

See [doc/](../doc/) for narrative tutorials and learning paths.
