# Basic Data Types

L4's basic data types are

- Booleans
- strings
- numbers
- records

## Booleans

L4's Boolean values are `TRUE` and `FALSE`. The type of Booleans
is called `BOOLEAN`.

## Strings

Strings are quoted using double-quotes: `"Alice Avocado"` is a string.
(Note that `` `Alice Avocado` `` is _not_ a string, it is an identifer
with a compound name, and if used, its meaning must be defined somewhere
in the program.)
The type of strings is called `STRING`.

## Numbers

L4 does not currently distinguish between integers and floats and just
has a common type called `NUMBER`.

Literal numbers can be written as usual: `333`, `0.25` and `-77.2`
are examples of valid numbers.

# User-Defined Types

## Enumerations, or Enums

An enumeration type can be declared as follows:

```l4
DECLARE Colour IS ONE OF red
                         green
						 blue
```

This introduces a new type `Colour` (distinct from all other types) with
exactly three values, which are called `red`, `green` and `blue`.

If you try to use `purple` as a `Colour`, it will be rejected.

### Destructuring with CONSIDER

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

## Type Aliases

We can declare our own types in terms of the above basic types:

```
DECLARE `count of fruit in my bag` IS A NUMBER
DECLARE `common name of fruit`     IS A STRING
DECLARE `scientific name of fruit` IS A STRING
DECLARE `my bag has a hole`        IS A BOOLEAN
```

This creates aliases from a user-defined type to a native type. Aliases are
exchangeable with the type they expand to, their primary intent is to provide
more descriptive names in specific situations.

## Records

Records in L4 are used to group related data together.

### Record Definition

Record types are declared using `DECLARE ... HAS ... IS A ...` syntax.

```l4
DECLARE Person
  HAS name IS A STRING
      age  IS A NUMBER
```

This introduces a new type `Person` (distinct from all other types) comprising
two fields, one called `name` being a string, and one called `age` being a number.

Once the type has been declared, you can instantiate it by defining variables using `MEANS` syntax.

There are two ways to give the attributes.

Using `WITH ... IS ...` syntax:

```l4
alice MEANS
  Person WITH name IS "Alice Avocado"
              age  IS 21
```

If using `WITH ... IS ...` syntax, argument order is flexible, so you could
also define:

```l4
bob MEANS
  Person WITH age  IS 20
              name IS "Bob Blueberry"
```

One can also use the shorter `Type OF ...` syntax:

```l4
alice MEANS Person OF "Alice Avocado", 21
```

In this case, the arguments have to appear exactly in the order they have
been listed in the declaration of the type `Person` above, so the name has
to come before the age.

You can omit the `OF` when the attributes go down the page:

```l4
alice MEANS Person   "Alice Avocado"
                     21
```

### Accessing Record Fields

Fields of a record can be accessed using the possessive apostrophe-s (`'s`) notation:

```l4
GIVEN john IS A Person
GIVETH A STRING
johnsName MEANS john's name

GIVETH A NUMBER
johnsAge MEANS john's age
```

This is analogous to the use of a dot (`.`) used in most other languages for record accessors: `john.age` becomes `john's age`

## Algebraic data types

Some types are neither enumeration nor record types, but a combination of the two. (TODO.)

## Advanced Concepts

More on these later.

### Type-Directed Name Resolution (TDNR)

TDNR allows the same identifier to be in scope multiple times with different types.

```l4
ASSUME foo IS A NUMBER
ASSUME foo IS A BOOLEAN
ASSUME foo IS A STRING
```
