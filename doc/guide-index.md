# L4

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

### Jump To Definition and References

Legal drafters may also appreciate VS Code's native "jump to definition" and "jump to references" features, available with a right-click on an expression of interest.

### Decision Logic Visualizer

Click on "visualize" to see a visual representation of a given Boolean function, as a circuit. "OR" disjunctions are represented as parallel circuits. "AND" conjunctions are represented as series circuits.

### Implemented Features

**Asyndetic conjunction (`...`)**: Implicit AND using three-dot ellipsis syntax, allowing `... expr` instead of `AND expr`. Named after the rhetorical device where conjunctions are omitted. See [Asyndetic Conjunction](20-basic-syntax.md#asyndetic-conjunction-).

**Asyndetic disjunction (`..`)**: Implicit OR using two-dot ellipsis syntax, allowing `.. expr` instead of `OR expr`. See [Asyndetic Disjunction](20-basic-syntax.md#asyndetic-disjunction-).

**Inert elements**: String literals in boolean context serve as grammatical scaffolding—they evaluate to TRUE in AND chains and FALSE in OR chains, preserving original legal language without affecting logical outcomes. See [Inert Elements](10-boolean-logic.md#inert-elements-grammatical-scaffolding).

### Future Features

Three carets together will mean "repeat everything above to the end of the line".

Syntax and semantics for regulative rules.

Syntax and semantics for property assertions and bounded deontics. Transpilation to verification reasoner backends: UPPAAL, NuSMV, SPIN, Maude, Isabelle/HOL, Lean.

Transpilation to automatic web app generation.

Set-theoretic syntax for UNION and INTERSECT. Sometimes set-and means logical-or.

WHEN should not be needed at each line in a CONSIDER.

## Conclusion

This guide provides an overview of the L4 language, including its syntax and semantics. For more detailed examples and advanced features, refer to the sample programs provided in the `examples` directory.
