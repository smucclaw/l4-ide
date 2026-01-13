# Sections

Sections are denoted by the `§` symbol followed by the section title. Subsections can be created by repeating the `§` symbol.

```l4
§ Section Title
§§ Subsection Title
§§§ Sub-subsection Title
```

This mechanism creates lexical scope.

Scope gives us the fine grain needed to encode legal expressions like
"For the purposes of sections 2 and 3(a), `tomato` means ..."

## Quoted Section Names

Section names can contain spaces and special characters when enclosed in backticks:

```l4
§ `Section Alpha`
§§ `Subsection with spaces`
```

## Referencing Entities Across Sections

Entities defined in one section can be referenced from another section using qualified names.

### Unqualified References

When sections are siblings (at the same level), entities from one section are visible in other sections without qualification:

```l4
§ `Section A`
sharedValue MEANS 42

§ `Section B`
-- sharedValue is visible here without qualification
result MEANS sharedValue PLUS 1
```

### Qualified References with Dot Notation

You can explicitly qualify a reference with its section path using dot notation:

```l4
§ `Section A`
value MEANS 42

§ `Section B`
-- Explicit qualification with section name
result MEANS `Section A`.value PLUS 1
```

For nested sections, chain the section names:

```l4
§ `Section Alpha`
§§ `Subsection Beta`
§§§ `Deep Section`
deepValue MEANS 99

§ `Other Section`
-- Reference through the full path
result MEANS `Section Alpha`.`Subsection Beta`.`Deep Section`.deepValue PLUS 1

#EVAL result  -- evaluates to 100
```

### Genitive Syntax ('s)

L4 supports an alternative genitive syntax using `'s` (apostrophe-s), which reads more naturally in legal English:

```l4
§ `Section A`
value MEANS 42

§ `Section B`
-- Using genitive syntax instead of dot
result MEANS `Section A`'s value PLUS 1
```

The genitive syntax is equivalent to dot notation and can be mixed:

```l4
-- These all refer to the same value:
result1 MEANS `Section Alpha`.`Subsection Beta`.`Deep Section`.deepValue
result2 MEANS `Section Alpha`'s `Subsection Beta`'s `Deep Section`'s deepValue
result3 MEANS `Section Alpha`'s `Subsection Beta`.`Deep Section`'s deepValue
```

### When to Use Qualified References

Qualified references are useful when:

1. **Disambiguation**: Two sections define entities with the same name
2. **Clarity**: Making the source of a value explicit in complex documents
3. **Cross-references**: Mirroring legal language like "the value defined in Section 2(a)"

```l4
§ `Part 1`
threshold MEANS 1000

§ `Part 2`
threshold MEANS 500

§ `Comparison`
-- Need qualification to distinguish between the two thresholds
difference MEANS `Part 1`.threshold MINUS `Part 2`'s threshold
```

### Section Aliases

Sections can have aliases using the `AKA` keyword. Aliases provide shorter names that can be used in qualified references:

```l4
§ `Definitions for Part VII` AKA defs

taxableIncome MEANS 50000

§ `Calculations`
-- Can use either the full name or the alias
result1 MEANS `Definitions for Part VII`.taxableIncome
result2 MEANS defs.taxableIncome
```
