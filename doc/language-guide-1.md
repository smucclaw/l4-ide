# L4 Language Guide

## Introduction

L4 is a domain-specific language for law.

L4's language design optimizes for editability and learnability. Most uses of L4 involve _tweaking_ existing text rather than drafting from scratch. Accordingly, L4 can be learned by looking at existing code. You don't need to read this entire guide to be productive as a legal engineer, but it will help you understand the language's syntax and semantics.

## Basic Syntax

### Strings and Terms

Strings are quoted using double-quotes: `"Alice Avocado"` is a string.

Identifiers, such as function names and record attributes, are quoted using backticks. Where other programming languages might use `camelCase` or `snake_case`, L4 allows the use of `` `space separated words` `` to form a single identifier.

### Comments

Comments are written using the `--` syntax.

```l4
-- This is a comment
```

You can also use `{- .... -}` syntax.

### Sections

Sections are denoted by the `§` symbol followed by the section title. Subsections can be created by repeating the `§` symbol.

```l4
§ Section Title
§§ Subsection Title
§§§ Sub-subsection Title
```

This mechanism creates lexical scope.

Scope gives us the fine grain needed to encode legal expressions like
"For the purposes of sections 2 and 3(a), `tomato` means ..."

### Representing and Combining Data

Every language offers mechanisms for data modelling. In most languages, variables have types. So does L4.

#### Native and User-Defined Types

L4 is equipped with basic types: Booleans, Numbers, and Strings.

We can declare our own types in terms of those native types:

```
DECLARE `count of fruit in my bag` IS A NUMBER
DECLARE `common name of fruit`     IS A STRING
DECLARE `scientific name of fruit` IS A STRING
DECLARE `my bag has a hole`        IS A BOOLEAN
```

This creates aliases from a user-defined type to a native type.

Complex types like Dates, Entities, Money, and Places are provided in the standard library.

L4 also supports algebraic types, of which more later.

### Simple Expressions with MEANS

Expressions can be written in a straightforward manner:

```l4
x MEANS 2 + 2
y MEANS x * x
```

Under the hood, these "variables" `x` and `y` are actually functions that take no arguments.

Usually, functions do take arguments...

#### Function Example with GIVEN, GIVETH, DECIDE/IS, MEANS

Perhaps we have three kinds of fruit in our grocery bag, and some more already in a bowl on the table.

How many fruit do we have in total?

```l4
GIVEN apples        IS A `count of fruit in my bag`
      bananas       IS A `count of fruit in my bag`
      oranges       IS A `count of fruit in my bag`
      `in the bowl` IS A NUMBER
DECIDE `total fruit` IS
  apples + bananas + oranges + `in the bowl`
```

You can see this example in the file <tutorial-code/fruit.l4>.

The `GIVEN` keywords indicate the types of the input parameters.

The `GIVETH` keyword indicates the return type of the function. It is optional; it will be inferred when omitted.

The name of the function comes between `DECIDE` and `IS`. Boolean functions read more naturally when you use the word `IF` instead of `IS`.

### DECIDE ... IS/IF == MEANS

These forms are equivalent:

```l4
DECIDE `total fruit` IS ...
```

```l4
`total fruit` MEANS ...
```

#### Constant Values

Constant values are simply functions which aren't given any arguments.

```l4
`bananas per bunch` MEANS 5
```

Maybe the bananas were counted by the bunch. To get the actual number of bananas, we need to multiply by 5.

```l4
`total fruit` MEANS
     apples
   + bananas * `bananas per bunch`
   + oranges
   + `in the bowl`
```

In real-world L4, you will see that the definitions section of a legal
text shows up as a long sequence of `MEANS` statements.

#### WHERE syntax

The fact that bananas come five to a bunch may not be relevant
anywhere outside this fruit-counting function.

We can relegate it to a coda at the end of the function definition:

```l4
`total fruit` MEANS
     apples
   + bananas * `bananas per bunch`
   + oranges
   + `in the bowl`
  WHERE `bananas per bunch` MEANS 5
```

This is a common pattern.

### Collections: Lists

Lists are created using the `LIST` keyword and can be manipulated using standard list operations.

```l4
GIVETH A LIST OF NUMBER
primes MEANS LIST 2, 3, 5, 7, 11
```

If your comma key is broken, lists can go down the page instead:

```l4
GIVETH A LIST OF NUMBER
primes MEANS LIST 2
                  3
                  5
                  7
                  11
```

### Conditionals

Conditionals are written using the `IF`, `THEN`, and `ELSE` keywords.

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
factorial n MEANS
  IF n EQUALS 0
  THEN 1
  ELSE n * factorial (n - 1)
```

Open <tutorial-code/factorial.l4> in VS Code and mouse-over the `#EVAL` line.

### IDE feature: Inline EVAL

This is a quick way to test expressions.

In the VS Code IDE with L4 extensions enabled, if you write

```
#EVAL `numbers are big` 1 2

#EVAL `numbers are big` 1000 1000
#EVAL `numbers are big` 1000 1001
#EVAL `numbers are big` 1001 1000
#EVAL `numbers are big` 1001 1001

#EVAL `numbers are big` 10001 0
#EVAL `numbers are big` 0 20001
```

You can mouseover the expressions and see the result of evaluation:

```
False

False
False
False
True

True
True
```

### Enumerations

Many programming languages have enums.

```l4
DECLARE Colour IS ONE OF red, green, blue
```

If you try to use `purple` as a `Colour`, it will be rejected.

#### Destructuring with CONSIDER

We pattern-match the possible values of a variable using `CONSIDER / WHEN / THEN`:

```l4
GIVEN c IS A Colour
DECIDE `is qing` IF
   CONSIDER c
       WHEN red   THEN FALSE
       WHEN green THEN TRUE
       WHEN blue  THEN TRUE
```

In classical Chinese, blue is 青, but green is also 青.

https://www.lomography.com/magazine/337259-color-chronicles-deconstructing-qing

### Indentation in Expressions

In a conventional language, parentheses are used to group boolean and arithmetic expressions:

```typescript
/**
 * Determines if the given numbers are considered "big".
 * @param {number} x - The first number.
 * @param {number} y - The second number.
 * @returns {boolean} - True if the numbers are big, false otherwise.
 */
function numbersAreBig(x: number, y: number): boolean {
  return (x > 1000 && y > 250 * (2 + 2)) || x > 10000 || y > 20000;
}
```

In L4, indentation replaces parentheses:

```l4
GIVEN x IS A NUMBER
      y IS A NUMBER
DECIDE `numbers are big`
    IF     x GREATER THAN 1000
       AND y GREATER THAN   250
                          *   2
                            + 2
    OR x GREATER THAN 10000
    OR y GREATER THAN 20000
```

This method of grouping was inspired by legal sub-paragraphs and sub-lists. The main difference is in where the "OR" and "AND" words appear.

### Records

Records in L4 are used to group related data together. They can be defined using the `WITH ... IS ...` syntax or the shorter `Constructor OF ...` syntax.

#### Record Definition

Record types are declared using `DECLARE ... HAS ... IS A ...` syntax.

```l4
DECLARE Person
  HAS name IS A STRING
      age  IS A NUMBER
```

Once the type has been declared, you can instantiate it by defining variables using `MEANS` syntax.

There are two ways to give the attributes.

Using `WITH ... IS ...` syntax:

```l4
alice MEANS
  Person WITH name IS "Alice Avocado"
              age  IS 21
```

This can also be written using the shorter `Type OF ...` syntax:

```l4
alice MEANS Person OF "Alice Avocado", 21
```

You can omit the `OF` when the attributes go down the page:

```l4
alice MEANS Person   "Alice Avocado"
                     21
```

#### Accessing Record Fields

Fields of a record can be accessed using the possessive apostrophe-s (`'s`) notation:

```l4
GIVEN john IS A Person
GIVETH A STRING
johnsName MEANS john's name

GIVETH A NUMBER
johnsAge MEANS john's age
```

This is analogous to the use of a dot (`.`) used in most other languages for record accessors: `john.age` becomes `john's age`

## Advanced Features

### Ditto Syntax

Strunk & White said: "Omit needless words". Edward Tufte talked about "data-ink".

L4 introduces "ditto syntax". A caret (`^`) expands to the word appearing directly above it, in the same column.

Judicious use of this convention improves the readability of multiline
expressions that would otherwise be over-noised with boilerplate.
Linguistically, this reads as an example of "conjunction reduction",
or "ellipsis". We use a caret instead of a literal ellipsis ("...").

### Annotations

Annotations can be added to various constructs using the `@` symbol.

```l4
@ref-src citations.csv
@ref url https://...
```

### AKA

Aliases can be created inline by inserting an `AKA xxx` after an expression.

### Natural Language Generation (NLG)

NLG annotations can be added to sections and other constructs.

```l4
§ `Section Head` [NLG annotation to section names are valid.]
```

### Type-Directed Name Resolution (TDNR)

TDNR allows the same identifier to be in scope multiple times with different types.

```l4
ASSUME foo IS A NUMBER
ASSUME foo IS A BOOLEAN
ASSUME foo IS A STRING
```

## Examples

### Example 1: XOR Function

```l4
§ xor

GIVEN x IS A BOOLEAN, y IS A BOOLEAN
GIVETH A BOOLEAN
DECIDE xor x y IS
     x AND NOT y
  OR NOT x AND y
```

### Example 2: Type Declarations

```l4
§ `Type declarations`

DECLARE bool IS ONE OF
  true
  false

DECLARE bool2 IS ONE OF true, false

DECLARE BOOL IS ONE OF
  TRUE
  FALSE
```

### Example 3: Fibonacci Function

```l4
§ `Fibonacci function`

GIVEN n IS A NUMBER
GIVETH A NUMBER
DECIDE fibNaive n IS
  IF n EQUALS 0
  THEN 0
  ELSE IF n EQUALS 1
  THEN 1
  ELSE fibNaive (n - 1) + fibNaive (n - 2)

#EVAL fibNaive 20
```

## IDE Affordances

### #EVAL and #CHECK

Besides the inline `#EVAL` discussed above, the L4 IDE plugin also supports the `#CHECK` directive. This shows typechecking.

### Jump To Definition and References

Legal drafters may also appreciate VS Code's native "jump to definition" and "jump to references" features, available with a right-click on an expression of interest.

### Decision Logic Visualizer

Click on "visualize" to see a visual representation of a given Boolean function, as a circuit. "OR" disjunctions are represented as parallel circuits. "AND" conjunctions are represented as series circuits.

### Future Features

Asyndetic conjunction operator: `..` instead of "AND" for readability.

Strings to be allowed in Boolean expressions as non-valuatable visible comments.

Three carets together will mean "repeat everything above to the end of the line".

Syntax and semantics for regulative rules.

Syntax and semantics for property assertions and bounded deontics. Transpilation to verification reasoner backends: UPPAAL, NuSMV, SPIN, Maude, Isabelle/HOL, Lean.

Transpilation to automatic web app generation.

Set-theoretic syntax for UNION and INTERSECT. Sometimes set-and means logical-or.

WHEN should not be needed at each line in a CONSIDER.

### Design Principles

L4 encodings of existing legal agreements are intended to support _isomorphism_: the L4 encoding closely follows the structure and the verbiage of the original text. At the same time, L4 is a _formal_ language, intended to support computer reasoning.

People with a background in programming should find many ideas familiar. Interestingly, people with a background in law should also find many ideas familiar. This is because legal drafters and programmers do similar work: they try to write down a comprehensive specification for how a system with multiple moving parts should behave, and what each of those moving parts ought to do, in a timely fashion. Decisions and actions frequently depend on complicated sets of conditions involving multiple input variables. Good drafters anticipate common problems and talk about how to get things back on track. They wonder what bugs and loopholes might lie latent in the code, and try to imagine scenarios that might put their drafting to the test.

Key differences from mainstream languages such as Python and Javascript:

- L4 is layout-sensitive. Instead of parenthesis, indentation is used for grouping. Instead of commas, list elements are delimited by new lines. This is intended to make L4 more approachable for non-programmers.
- L4 is strongly typed. Algebraic data types are used to improve data modelling and reduce unintentional ambiguity.
- L4 is as much a specification language as it is a programming language. The "letter of the law" can be written down in L4. The "spirit of the law" can also be written in L4, and the "spirit" part can be used to test the "letter".
- Legal idioms are translated, where possible, into corresponding L4 idioms.

L4 is intended to be written not just by humans but by AIs too. L4 ships with extensions for VS Code to support syntax highlighting, type checking, and logic visualization. Copilot is capable of automatically translating existing legal text into L4, reducing the burden of authoring to one of review and correction. Just press TAB.

After a legal agreement or legislation/regulation has been encoded in L4, the L4 suite of tools helps to answer common questions like:

- given certain events that have occurred so far, what state am I in?
- what are my immediate obligations?
- if I want to achieve a certain outcome, what must I do? By when?
- if I want to avoid a certain outcome, what can I do? How soon should I do it?
- if a certain amount of money has been calculated, what was the basis for that calculation?
- if a malicious counterparty wanted to violate the spirit but obey the letter, what might they try to do?

These questions can be answered visually or interactively with a chatbot.

## Conclusion

This guide provides an overview of the L4 language, including its syntax and semantics. For more detailed examples and advanced features, refer to the sample programs provided in the `examples` directory.
