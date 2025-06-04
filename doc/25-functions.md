# Functions: Decisions and Definitions

L4 programs use functions to express **definitions** and **decisions**.

## Expressing Definitions with MEANS

Expressions can be written in a straightforward manner:

```l4
x MEANS 2 + 2
y MEANS x * x
```

Under the hood, the constant `x` and the variable `y` are actually
functions that take no arguments. They are "defined terms".

Usually, functions do take arguments...

## Decision Function Example with GIVEN, GIVETH, DECIDE/IS, MEANS

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

Decision functions rely purely on the inputs that are given, and on
values that are available in the environment's scope.

You can see this example in the file [fruit.l4](./tutorial-code/fruit.l4).

The `GIVEN` keywords indicate the types of the input parameters.

The `GIVETH` keyword indicates the return type of the function. It is optional; it will be inferred when omitted.

The name of the function comes between `DECIDE` and `IS`. Boolean functions read more naturally when you use the word `IF` instead of `IS`.

### `MEANS` == `DECIDE ... IS/IF`

These forms are equivalent:

```l4
DECIDE `total fruit` IS ...
```

```l4
`total fruit` MEANS ...
```

## Constant Values

Constant values are simply functions which aren't given any arguments.

```l4
`pi` MEANS 3.1415926
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

## WHERE syntax

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

## Conditional Expressions

Conditionals are written using the `IF`, `THEN`, and `ELSE` keywords.

Some programmers may be familiar with the punctuated form: `if ? then : else`.

```l4
GIVEN n IS A NUMBER
GIVETH A NUMBER
factorial n MEANS
  IF n EQUALS 0
  THEN 1
  ELSE n * factorial (n - 1)
```

Open <tutorial-code/factorial.l4> in VS Code and mouse-over the `#EVAL` line.

## IDE feature: Inline EVAL

This is a quick way to test expressions.

In the VS Code IDE with L4 extensions enabled, if you write

```l4
#EVAL `numbers are big` 1 2

#EVAL `numbers are big` 1000 1000
#EVAL `numbers are big` 1000 1001
#EVAL `numbers are big` 1001 1000
#EVAL `numbers are big` 1001 1001

#EVAL `numbers are big` 10001 0
#EVAL `numbers are big` 0 20001
```

You can mouseover the expressions and see the result of evaluation:

```l4
FALSE

FALSE
FALSE
FALSE
TRUE

TRUE
TRUE
```

## Example: Fibonacci Function

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

### #EVAL and #CHECK

Besides the inline `#EVAL` discussed above, the L4 IDE plugin also supports the `#CHECK` directive. This shows typechecking.
