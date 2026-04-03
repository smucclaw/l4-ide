# Legislative Ingestion with LLM

Use LLMs to help encode legislation in L4.

**Audience:** Legal engineers, legislative drafters  
**Prerequisites:** [LLM Getting Started](llm-getting-started.md), intermediate L4 knowledge  
**Time:** 30 minutes  
**Goal:** Learn a workflow for LLM-assisted legislation encoding

---

## The Challenge

Converting legislation to L4 involves:

1. **Understanding** the legal text
2. **Identifying** types and rules
3. **Translating** to L4 syntax
4. **Verifying** the encoding

LLMs can help with steps 1-3, but humans must verify step 4.

---

## The Workflow

```
┌──────────────────────────────────────────────┐
│ 1. SEGMENT: Break legislation into chunks    │
└──────────────────────────────────────────────┘
                      │
                      ▼
┌──────────────────────────────────────────────┐
│ 2. EXTRACT: LLM identifies types & rules     │
└──────────────────────────────────────────────┘
                      │
                      ▼
┌──────────────────────────────────────────────┐
│ 3. TRANSLATE: LLM generates L4 draft         │
└──────────────────────────────────────────────┘
                      │
                      ▼
┌──────────────────────────────────────────────┐
│ 4. VALIDATE: Human reviews & L4 type-checks  │
└──────────────────────────────────────────────┘
                      │
                      ▼
┌──────────────────────────────────────────────┐
│ 5. ITERATE: Fix issues, re-validate          │
└──────────────────────────────────────────────┘
```

---

## Step 1: Segment the Legislation

Break legislation into manageable chunks. Each chunk should be:

- Self-contained (one concept)
- Referenced by section/article number
- Small enough for LLM context

### Example Legislation

```
Charities (Jersey) Law 2014

Article 2 - Interpretation
In this Law –
"charitable purpose" means any of the purposes specified in Schedule 1;
"charity" means an entity that is established for charitable purposes only;
"governor" means any person who is responsible for the control and
management of the administration of a registered charity;

Article 5 - Charity test
An entity meets the charity test if –
(a) all of its purposes are charitable purposes; and
(b) it satisfies the public benefit requirement.

Article 7 - Public benefit
(1) The public benefit requirement is that –
    (a) any benefit that may result from carrying out the purposes is
        a benefit to the public or a section of the public; and
    (b) any benefit to persons who are not members of the public but
        who have a material connection to the entity is not unduly
        favourable.
```

---

## Step 2: Extract Structure with LLM

### Prompt for Type Extraction

```
You are analyzing legislation to extract data types for encoding in L4.

Legislation text:
"""
Article 2 - Interpretation
In this Law –
"charitable purpose" means any of the purposes specified in Schedule 1;
"charity" means an entity that is established for charitable purposes only;
"governor" means any person who is responsible for the control and
management of the administration of a registered charity;
"""

Extract all defined terms as types. For each type, identify:
1. Name of the type
2. Whether it's an enumeration (fixed list) or record (has fields)
3. The fields or variants
4. Any referenced types

Output as JSON:
```

### Expected LLM Response

```json
{
  "types": [
    {
      "name": "CharitablePurpose",
      "kind": "enumeration",
      "note": "Variants from Schedule 1 - not defined here",
      "reference": "Schedule 1"
    },
    {
      "name": "Charity",
      "kind": "record",
      "fields": [
        {
          "name": "purposes",
          "type": "LIST OF CharitablePurpose",
          "constraint": "charitable only"
        }
      ]
    },
    {
      "name": "Governor",
      "kind": "record",
      "fields": [
        { "name": "person", "type": "Person" },
        { "name": "charity", "type": "RegisteredCharity" }
      ],
      "note": "responsible for control and management"
    }
  ]
}
```

---

## Step 3: Generate L4 Draft

### Prompt for L4 Generation

```
You are generating L4 code from legislative requirements.

L4 syntax reference:
- Types: DECLARE TypeName HAS field IS A FieldType
- Enums: DECLARE TypeName IS ONE OF Variant1, Variant2
- Rules: DECIDE `rule name` IF condition1 AND condition2

Requirement:
"""
Article 5 - Charity test
An entity meets the charity test if –
(a) all of its purposes are charitable purposes; and
(b) it satisfies the public benefit requirement.
"""

Types already defined:
- Charity: has purposes (LIST OF Purpose)
- Purpose: enumeration of charitable purposes

Generate L4 code with:
1. Type declarations if needed
2. The main rule
3. Comments referencing the article
```

### Expected LLM Response

```l4
-- Article 5: Charity test
-- An entity meets the charity test if:
-- (a) all of its purposes are charitable purposes; and
-- (b) it satisfies the public benefit requirement.

GIVEN entity IS A Charity
GIVETH A BOOLEAN
DECIDE `meets charity test` IF
    `all purposes are charitable` entity    -- Art 5(a)
    AND `satisfies public benefit` entity   -- Art 5(b)

-- Art 5(a): All purposes must be charitable
GIVEN entity IS A Charity
GIVETH A BOOLEAN
`all purposes are charitable` MEANS
    all (GIVEN p YIELD `is charitable purpose` p) (entity's purposes)

-- Art 5(b): Public benefit requirement (see Art 7)
GIVEN entity IS A Charity
GIVETH A BOOLEAN
ASSUME `satisfies public benefit` IS BOOLEAN
-- Note: Full definition requires Article 7 encoding
```

---

## Step 4: Validate and Refine

### Check Type Correctness

Run the L4 type checker:

```bash
cabal run jl4-cli -- charity-test.l4
```

### Common Issues

| LLM Output              | Problem        | Fix                                       |
| ----------------------- | -------------- | ----------------------------------------- |
| `purposes IS A Purpose` | Should be LIST | `purposes IS A LIST OF Purpose`           |
| Missing GIVETH          | No return type | Add `GIVETH A BOOLEAN`                    |
| `IF x = TRUE`           | Redundant      | Just `IF x`                               |
| Wrong field access      | Missing 's     | `entity's purposes` not `entity purposes` |

### Human Review Checklist

- [ ] Types match legislation definitions
- [ ] Rules capture all conditions
- [ ] Cross-references are correct
- [ ] Edge cases are handled
- [ ] Code compiles without errors

---

## Step 5: Iterate

Feed validation errors back to LLM:

```
The L4 type checker found this error:

Error: Type 'Purpose' not in scope
Location: line 12

Current code:
all (GIVEN p YIELD `is charitable purpose` p) (entity's purposes)

Available types:
- CharitablePurpose
- Charity
- Entity

Fix the type name.
```

---

## Complete Example

### Input Legislation

```
Article 19 - Disqualification of governors
(1) A person is disqualified from acting as a governor of a charity if –
    (a) the person is an undischarged bankrupt;
    (b) the person has an unspent conviction for an offence involving
        dishonesty or deception;
    (c) the person is disqualified from acting as a company director; or
    (d) the person is subject to a disqualification order under Article 23.
```

### LLM-Generated L4 (After Refinement)

```l4
§ `Article 19 - Governor Disqualification`

-- Types for disqualification grounds
DECLARE DisqualificationGround IS ONE OF
    Bankruptcy                           -- Art 19(1)(a)
    UnspentConviction HAS offence IS A STRING  -- Art 19(1)(b)
    DirectorDisqualification             -- Art 19(1)(c)
    DisqualificationOrder HAS orderRef IS A STRING  -- Art 19(1)(d)

-- Person record for governors
DECLARE Person
    HAS name IS A STRING
        isBankrupt IS A BOOLEAN
        convictions IS A LIST OF Conviction
        isDirectorDisqualified IS A BOOLEAN
        disqualificationOrders IS A LIST OF STRING

DECLARE Conviction
    HAS offence IS A STRING
        isSpent IS A BOOLEAN
        involvesDishonesty IS A BOOLEAN

-- Article 19(1)(a): Bankruptcy
GIVEN person IS A Person
GIVETH A BOOLEAN
`is undischarged bankrupt` MEANS person's isBankrupt

-- Article 19(1)(b): Unspent conviction for dishonesty
GIVEN person IS A Person
GIVETH A BOOLEAN
`has disqualifying conviction` MEANS
    any (GIVEN c YIELD
        c's involvesDishonesty
        AND NOT c's isSpent
    ) (person's convictions)

-- Article 19(1)(c): Director disqualification
GIVEN person IS A Person
GIVETH A BOOLEAN
`is director disqualified` MEANS person's isDirectorDisqualified

-- Article 19(1)(d): Disqualification order
GIVEN person IS A Person
GIVETH A BOOLEAN
`has disqualification order` MEANS
    length (person's disqualificationOrders) > 0

-- Main rule: Article 19(1)
GIVEN person IS A Person
GIVETH A BOOLEAN
DECIDE `is disqualified as governor` IF
    `is undischarged bankrupt` person          -- (a)
    OR `has disqualifying conviction` person   -- (b)
    OR `is director disqualified` person       -- (c)
    OR `has disqualification order` person     -- (d)
```

---

## Best Practices

### 1. Chunk Size

- One article/section per prompt
- Include cross-references in context

### 2. Iterative Refinement

- Start with types
- Then rules
- Then edge cases

### 3. Human-in-the-Loop

- Always validate LLM output
- Run type checker
- Test with #EVAL/#TRACE

### 4. Version Control

- Track both source legislation and L4
- Document which LLM/version used
- Keep prompts for reproducibility

---

## Limitations

- LLMs can hallucinate legal interpretations
- Complex cross-references may be missed
- Human expertise still essential
- Output needs validation

---

## What You Learned

- Workflow for LLM-assisted legislation encoding
- Prompt design for type extraction
- Prompt design for L4 generation
- Validation and iteration

---

## Next Steps

- [LLM Getting Started](llm-getting-started.md) - Basics of LLM integration
- [Advanced Course Module A1](../../courses/advanced/module-a1-regulatory.md) - Manual legislative encoding
- Practice with real legislation from your jurisdiction
