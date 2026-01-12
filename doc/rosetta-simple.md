# Rosetta Stone: Simple Circle Area Calculation

This document shows a simple function to calculate the area of a circle, translated across L4, TypeScript, Python, Perl, Fortran, COBOL, Haskell, OCaml, SWI-Prolog, C, C++, Java, Ruby, Elixir, Go, Rust, Swift, Racket, and various styles of legal English.

## L4 (Original)

```l4
-- TEST 4: Postfix helper defined inside WHERE
GIVEN radius IS A NUMBER
GIVETH A NUMBER
`circle area` radius MEANS
  LET pi BE 3
  IN radius squared TIMES pi
  WHERE
    GIVEN r IS A NUMBER
    r squared MEANS r * r

#EVAL `circle area` OF 2
-- Expected: 12
-- Actual: 12 ✓
```

From `jl4/examples/ok/postfix-with-variables.l4:70-81`

## TypeScript

```typescript
function circleArea(radius: number): number {
  // Helper function defined locally
  function squared(r: number): number {
    return r * r;
  }

  const pi: number = 3;
  return squared(radius) * pi;
}

// Example usage
console.log(circleArea(2)); // Expected: 12
```

## Python

```python
def circle_area(radius: float) -> float:
    """Calculate the area of a circle using a local helper function."""

    # Helper function defined locally
    def squared(r: float) -> float:
        return r * r

    pi: float = 3
    return squared(radius) * pi

# Example usage
print(circle_area(2))  # Expected: 12
```

## Perl

```perl
use strict;
use warnings;
use feature 'signatures';
no warnings 'experimental::signatures';

sub circle_area ($radius) {
    # Helper function defined locally
    my $squared = sub ($r) {
        return $r * $r;
    };

    my $pi = 3;
    return $squared->($radius) * $pi;
}

# Example usage
print circle_area(2), "\n";  # Expected: 12
```

## Fortran

```fortran
program circle_area_example
    implicit none
    real :: result

    result = circle_area(2.0)
    print *, 'Result:', result  ! Expected: 12

contains

    real function circle_area(radius)
        real, intent(in) :: radius
        real :: pi

        pi = 3.0
        circle_area = squared(radius) * pi

    contains

        real function squared(r)
            real, intent(in) :: r
            squared = r * r
        end function squared

    end function circle_area

end program circle_area_example
```

## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIRCLE-AREA-EXAMPLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 RADIUS           PIC 9V9 VALUE 2.0.
       01 RESULT           PIC 99V9.
       01 PI               PIC 9V9 VALUE 3.0.
       01 TEMP-SQUARED     PIC 99V9.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM CIRCLE-AREA.
           DISPLAY "Result: " RESULT.
           STOP RUN.

       CIRCLE-AREA.
           PERFORM SQUARED.
           COMPUTE RESULT = TEMP-SQUARED * PI.

       SQUARED.
           COMPUTE TEMP-SQUARED = RADIUS * RADIUS.
```

## Haskell

```haskell
circleArea :: Float -> Float
circleArea radius = squared radius * pi
  where
    squared r = r * r
    pi = 3

-- Example usage
main :: IO ()
main = print (circleArea 2)  -- Expected: 12
```

## OCaml

```ocaml
let circle_area radius =
  let squared r = r *. r in
  let pi = 3.0 in
  squared radius *. pi

(* Example usage *)
let () = print_float (circle_area 2.0)  (* Expected: 12 *)
```

## SWI-Prolog (Standard)

```prolog
% Helper predicate
squared(R, Result) :-
    Result is R * R.

% Main predicate
circle_area(Radius, Result) :-
    Pi = 3,
    squared(Radius, RadiusSquared),
    Result is RadiusSquared * Pi.

% Example usage
?- circle_area(2, Result).
% Result = 12
```

## SWI-Prolog (with CLP(Q))

```prolog
:- use_module(library(clpq)).

% Helper constraint
squared(R, Result) :-
    { Result = R * R }.

% Main predicate with constraints
circle_area(Radius, Result) :-
    Pi = 3,
    squared(Radius, RadiusSquared),
    { Result = RadiusSquared * Pi }.

% Example usage
?- circle_area(2, Result).
% Result = 12
```

## C

```c
#include <stdio.h>

// Helper function
double squared(double r) {
    return r * r;
}

// Main function
double circle_area(double radius) {
    double pi = 3.0;
    return squared(radius) * pi;
}

// Example usage
int main() {
    printf("Result: %g\n", circle_area(2.0));  // Expected: 12
    return 0;
}
```

## C++

```cpp
#include <iostream>

// Helper function
double squared(double r) {
    return r * r;
}

// Main function
double circleArea(double radius) {
    double pi = 3.0;
    return squared(radius) * pi;
}

// Example usage
int main() {
    std::cout << "Result: " << circleArea(2.0) << std::endl;  // Expected: 12
    return 0;
}
```

## Java

```java
public class CircleArea {
    // Helper method
    private static double squared(double r) {
        return r * r;
    }

    // Main method
    public static double circleArea(double radius) {
        double pi = 3.0;
        return squared(radius) * pi;
    }

    // Example usage
    public static void main(String[] args) {
        System.out.println("Result: " + circleArea(2.0));  // Expected: 12
    }
}
```

## Ruby

```ruby
# Helper method
def squared(r)
  r * r
end

# Main method
def circle_area(radius)
  pi = 3
  squared(radius) * pi
end

# Example usage
puts "Result: #{circle_area(2)}"  # Expected: 12
```

## Elixir

```elixir
defmodule CircleArea do
  # Helper function
  defp squared(r) do
    r * r
  end

  # Main function
  def circle_area(radius) do
    pi = 3
    squared(radius) * pi
  end
end

# Example usage
IO.puts("Result: #{CircleArea.circle_area(2)}")  # Expected: 12
```

## Go

```go
package main

import "fmt"

// Helper function
func squared(r float64) float64 {
    return r * r
}

// Main function
func circleArea(radius float64) float64 {
    pi := 3.0
    return squared(radius) * pi
}

// Example usage
func main() {
    fmt.Printf("Result: %g\n", circleArea(2.0))  // Expected: 12
}
```

## Rust

```rust
// Helper function
fn squared(r: f64) -> f64 {
    r * r
}

// Main function
fn circle_area(radius: f64) -> f64 {
    let pi = 3.0;
    squared(radius) * pi
}

// Example usage
fn main() {
    println!("Result: {}", circle_area(2.0));  // Expected: 12
}
```

## Swift

```swift
// Helper function
func squared(_ r: Double) -> Double {
    return r * r
}

// Main function
func circleArea(radius: Double) -> Double {
    let pi = 3.0
    return squared(radius) * pi
}

// Example usage
print("Result: \(circleArea(radius: 2.0))")  // Expected: 12
```

## Racket

```racket
#lang racket

; Helper function
(define (squared r)
  (* r r))

; Main function
(define (circle-area radius)
  (let ([pi 3])
    (* (squared radius) pi)))

; Example usage
(displayln (string-append "Result: " (number->string (circle-area 2))))  ; Expected: 12
```

## Legal English (18th Century)

```
WHEREBY IT IS GIVEN that there shall be a RADIUS, being a NUMBER;

AND WHEREAS for the Purpose of ascertaining the Area of a Circle, it is
necessary and expedient to establish certain Definitions and Calculations
hereinafter set forth;

NOW THEREFORE BE IT KNOWN that the Area of said Circle (hereinafter
referred to as "the Circle Area") shall be computed and determined in the
following Manner, to wit:

FIRST, let it be understood that the Mathematical Constant π (pi), for the
Purposes of this Calculation, shall take the Value of Three (3);

SECOND, the said RADIUS, being multiplied by itself (which Operation is
hereinafter defined as "SQUARED"), shall yield a Product;

WHEREIN the Operation of SQUARING any Number, being a NUMBER denoted as r,
shall consist of the Multiplication of said r by itself, yielding the
Product r × r;

THIRD, the Product so obtained by SQUARING the RADIUS shall be multiplied
by the aforesaid Constant π;

WHEREBY the final Result of said Computation shall constitute and represent
the Area of the Circle.

IN WITNESS WHEREOF, when the RADIUS equals Two (2), the Circle Area equals
Twelve (12).
```

## Legal English (19th Century)

```
Article I. Definitions

For the purposes of this calculation, the following terms shall have the
meanings herein ascribed to them:

"Radius" means a number representing the distance from the center to the
circumference of a circle.

"Squared" means, with respect to any number r, the product of r multiplied
by itself.

"Pi" means the mathematical constant having the value of three (3) for
purposes of this calculation.

Article II. Method of Calculation

The area of a circle (the "Circle Area") shall be calculated as follows:

(a) The Radius shall be Squared in accordance with the definition set forth
    in Article I hereof;

(b) The result obtained in subsection (a) shall be multiplied by Pi as
    defined in Article I;

(c) The product obtained in subsection (b) shall constitute the Circle Area.

Article III. Example

By way of illustration and not limitation, if the Radius equals two (2),
then the Circle Area equals twelve (12).
```

## Legal English (20th Century)

```
§ 1. Calculation of Circle Area

(a) Definitions. As used in this section:
    (1) "radius" means a number;
    (2) "squared" means, with respect to any number r, the value r × r;
    (3) "pi" means the constant value 3.

(b) Formula. The area of a circle shall be calculated by:
    (1) squaring the radius; and
    (2) multiplying the result by pi.

(c) Example. If the radius is 2, the circle area is 12.
```

## Plain English (21st Century)

```
How to calculate a circle's area:

1. Take the radius (a number)
2. Square it (multiply it by itself)
3. Multiply the result by pi (which we'll use as 3)

Example: If the radius is 2:
- Squared: 2 × 2 = 4
- Multiply by pi: 4 × 3 = 12
- Result: The circle's area is 12
```

## Latin

```
DE AREA CIRCULI CALCULANDA

I. Definitiones

Sit RADIUS numerus quidam datus.

Operatio QUADRANDI, pro numero quolibet r, est multiplicatio ipsius r per
seipsum, unde fit r × r.

Constans π (pi), pro hoc calculo, valorem trium (III) habeat.

II. Methodus Calculationis

Area Circuli (quae "Area Circuli" nominatur) hoc modo calculanda est:

Primum: RADIUS quadrandus est, secundum definitionem supra positam;

Deinde: Productum ex quadratione RADII multiplicandum est per π;

Unde: Productum finale aream circuli praebet.

III. Exemplum

Si RADIUS aequatur duobus (II), tunc Area Circuli aequatur duodecim (XII).

Q.E.D.
```

## Key Translation Notes

1. **Function Signature**:
   - L4: `GIVEN radius IS A NUMBER` / `GIVETH A NUMBER`
   - TypeScript: `function name(radius: number): number`
   - Python: `def name(radius: float) -> float:`
   - Perl: `sub name ($radius)` (requires `use feature 'signatures'`)
   - Fortran: `real function name(radius)` with `intent(in)` for parameters
   - COBOL: Uses `PERFORM` to call paragraphs/sections, not functions
   - Haskell: `name :: Float -> Float` (type signature separate from definition)
   - OCaml: `let name radius =` (type inference, no explicit signature needed)
   - Prolog: `name(Input, Output) :-` (predicates relate inputs to outputs)
   - C: `double name(double radius)` (return type prefix)
   - C++: `double name(double radius)` (return type prefix, similar to C)
   - Java: `public static double name(double radius)` (return type with access modifiers)
   - Ruby: `def name(radius)` (no type annotations, dynamic typing)
   - Elixir: `def name(radius) do` (within module; use `defp` for private functions)
   - Go: `func name(radius float64) float64` (return type suffix)
   - Rust: `fn name(radius: f64) -> f64` (explicit types with arrow syntax)
   - Swift: `func name(radius: Double) -> Double` (named parameters available)
   - Racket: `(define (name radius) ...)` (S-expression syntax, dynamic typing)

2. **Local Helper Functions**:
   - L4: Defined in a `WHERE` clause at the end
   - TypeScript/Python: Defined inside the main function body
   - Perl: Defined as anonymous subroutines (`sub { ... }`) assigned to lexical variables
   - Fortran: Nested `CONTAINS` sections for internal procedures
   - COBOL: Separate paragraphs in PROCEDURE DIVISION
   - Haskell: Defined in a `where` clause at the end (mirrors L4!)
   - OCaml: Defined with `let ... in` expressions
   - Prolog: Separate predicates (no true local scope; all predicates are global)
   - C/C++: Separate functions (typically at file scope; can use `static` for file-local linkage)
   - Java: Private static methods within the class
   - Ruby: Separate methods at the same scope (methods are global or instance methods)
   - Elixir: Private functions with `defp` within the same module
   - Go: Separate functions at package level (lowercase names are package-private)
   - Rust: Can define functions inside other functions (true nested functions)
   - Swift: Can define functions inside other functions (nested/local functions)
   - Racket: Separate top-level definitions with `define` (can use internal definitions or `let`)

3. **Local Variables**:
   - L4: `LET pi BE 3 IN ...`
   - TypeScript: `const pi: number = 3;`
   - Python: `pi: float = 3`
   - Perl: `my $pi = 3;`
   - Fortran: `real :: pi` followed by `pi = 3.0`
   - COBOL: `01 PI PIC 9V9 VALUE 3.0` in WORKING-STORAGE
   - Haskell: Defined in `where` clause: `pi = 3`
   - OCaml: `let pi = 3.0 in ...`
   - Prolog: Unification with atoms: `Pi = 3` (variables are uppercase, atoms lowercase)
   - C/C++: `double pi = 3.0;`
   - Java: `double pi = 3.0;`
   - Ruby: `pi = 3` (local variable, no declaration keyword needed)
   - Elixir: `pi = 3` (immutable binding)
   - Go: `pi := 3.0` (short variable declaration with type inference)
   - Rust: `let pi = 3.0;` (immutable by default)
   - Swift: `let pi = 3.0` (immutable, type inferred)
   - Racket: `(let ([pi 3]) ...)` (lexical binding within `let` form)

4. **Type System**:
   - L4: Explicit type annotations (`IS A NUMBER`)
   - TypeScript: Static typing with `: number`
   - Python: Optional type hints with `: float`
   - Perl: Dynamic typing (no type declarations)
   - Fortran: Static typing with `real`, `integer`, etc.
   - COBOL: Picture clauses (`PIC 9V9` for decimal numbers)
   - Haskell: Strong static typing with type inference and explicit signatures (`:: Float -> Float`)
   - OCaml: Strong static typing with type inference (explicit signatures optional)
   - Prolog: Untyped (dynamically typed); CLP(Q) provides domain constraints for rationals
   - C: Static typing with explicit type declarations (`double`, `int`, etc.)
   - C++: Static typing like C, with additional type features (classes, templates, etc.)
   - Java: Strong static typing with primitive types (`double`, `int`) and reference types
   - Ruby: Dynamic typing with duck typing ("if it quacks like a duck...")
   - Elixir: Dynamic typing with pattern matching and guards for runtime type checking
   - Go: Static typing with type inference (`:=`) and explicit types (`float64`, `int`, etc.)
   - Rust: Strong static typing with powerful type inference and ownership/borrowing system
   - Swift: Strong static typing with type inference and optionals for null safety
   - Racket: Dynamic typing with optional contracts for runtime type checking

5. **Operators**:
   - L4: Uses postfix notation (`radius squared`) and infix (`TIMES`)
   - TypeScript/Python/Perl/Fortran/Haskell/C/C++/Java/Ruby/Elixir/Go/Rust/Swift: Standard infix operators (`*`)
   - COBOL: `COMPUTE` statement for arithmetic operations
   - OCaml: Type-specific operators (`*.` for float multiplication)
   - Prolog (Standard): `is` operator for arithmetic evaluation
   - Prolog (CLP(Q)): Constraint notation `{ Result = Expression }` for declarative constraints
   - Racket: Prefix notation `(* a b)` (S-expression syntax, operator comes first)

6. **Calling Local Functions**:
   - TypeScript/Python/Fortran/Haskell/OCaml/C/C++/Java/Ruby/Elixir/Go/Rust/Swift: Direct invocation `squared(radius)` or `squared radius`
   - Perl: Dereference syntax `$squared->($radius)`
   - COBOL: `PERFORM` statement to execute paragraphs
   - Prolog: Goal invocation `squared(Input, Output)` as part of predicate body
   - Racket: Prefix notation `(squared radius)` (function application as S-expression)

7. **Naming Conventions**:
   - L4: Uses backticks for multi-word identifiers
   - TypeScript: camelCase
   - Python: snake_case
   - Perl: snake_case with sigils ($, @, %)
   - Fortran: snake_case or mixed case (case-insensitive)
   - COBOL: UPPER-CASE-WITH-HYPHENS
   - Haskell: camelCase
   - OCaml: snake_case
   - Prolog: snake_case for predicates; Variables start with uppercase, atoms with lowercase
   - C: snake_case (by convention)
   - C++: camelCase or snake_case (varies by style guide)
   - Java: camelCase for methods, PascalCase for classes
   - Ruby: snake_case for methods and variables
   - Elixir: snake_case for functions and variables, PascalCase for modules
   - Go: camelCase (uppercase = exported, lowercase = private)
   - Rust: snake_case for functions and variables, PascalCase for types
   - Swift: camelCase for functions and variables, PascalCase for types
   - Racket: kebab-case (words separated by hyphens, common in Lisp family)

8. **Historical Context & Design Philosophy**:
   - Fortran (1957): Oldest high-level language, still dominant in scientific computing
   - Lisp (1958): Second-oldest high-level language, pioneered garbage collection, symbolic computation, and code-as-data
   - COBOL (1959): Designed for business applications, still runs major financial systems
   - Scheme (1975): Minimalist Lisp dialect emphasizing simplicity and lexical scope
   - C (1972): Systems programming language that influenced nearly all modern languages
   - Prolog (1972): Logic programming language based on first-order logic, foundational for AI and computational linguistics
   - C++ (1985): Object-oriented extension of C, adding classes and generic programming
   - Haskell (1990): Pure functional language with lazy evaluation, named after logician Haskell Curry
   - Racket (1995): Modern descendant of Scheme, designed for language-oriented programming and education
   - Ruby (1995): Dynamic OOP language emphasizing developer happiness and productivity
   - Java (1995): Class-based OOP language designed for portability ("write once, run anywhere")
   - OCaml (1996): Functional language with imperative features, widely used in formal verification
   - Go (2009): Simple, concurrent language from Google emphasizing fast compilation and built-in concurrency
   - Elixir (2011): Functional language for the Erlang VM, emphasizing scalability and fault tolerance
   - TypeScript (2012): Superset of JavaScript adding static typing
   - Swift (2014): Modern language from Apple combining safety with performance, replacing Objective-C
   - Rust (2015): Systems language emphasizing memory safety without garbage collection through ownership system
   - SWI-Prolog: Modern Prolog implementation with rich library ecosystem including CLP(Q) for constraint solving
   - L4's `WHERE` clause mirrors Haskell's `where` syntax, showing functional programming influence
   - L4's declarative nature and focus on logic also resonates with Prolog's logic programming paradigm
   - Racket's S-expressions and homoiconicity (code as data) influenced domain-specific language design
   - Ruby's elegant syntax and Elixir's immutability demonstrate modern approaches to readability and reliability
   - Go, Rust, and Swift represent modern systems languages prioritizing safety, concurrency, and developer experience
   - Rust's ownership model and Swift's optionals show how modern languages prevent entire classes of bugs at compile time
   - These languages span multiple paradigms: imperative (C, C++, Java), procedural (Fortran, COBOL), functional (Haskell, OCaml, Elixir, Racket), logic (Prolog), dynamic OOP (Ruby), and modern systems (Go, Rust, Swift)

9. **Legal English Evolution**:
   - **18th Century**: Highly verbose with "WHEREAS", "WHEREBY", "hereinafter" - reflects an era when legal documents had to be extremely explicit due to limited precedent
   - **19th Century**: Article-based structure emerges, still formal but more organized with definitions sections - Victorian era systematization
   - **20th Century**: Modern statutory drafting with section numbers (§), subsections, and cross-references - influence of codification movements
   - **21st Century Plain English**: Plain language movement prioritizes clarity and accessibility - recognition that law should be understandable to those governed by it
   - **Latin**: Classical legal language, still used in phrases like "Q.E.D." (quod erat demonstrandum), shows ancient roots of legal reasoning
   - L4 aims to combine the precision of formal legal language with the executability of programming languages
   - The evolution from 18th century verbosity to 21st century plain language mirrors the journey from assembly to high-level languages
   - Just as COBOL tried to be "English-like" for business users, L4 aims to be legally-precise yet computationally-executable
