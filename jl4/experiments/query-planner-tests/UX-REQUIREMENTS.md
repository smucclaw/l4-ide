# Query Planner UX Requirements

## Core Principle: Minimize User Frustration

The query planner should help users provide information efficiently while **minimizing frustration** by:
1. Always showing all input widgets (don't hide anything)
2. Visually prioritizing relevant questions
3. Allowing answers at any level of the expression tree

## Visual Prioritization (Not Hiding)

### ✅ DO: Gray out irrelevant parameters
```
Inputs:
  age_21_plus: [✓ True]  ← User selected
  married: [ ]           ← Relevant, normal emphasis
  spousal_approval: [ ]  ← Relevant, normal emphasis
  beer_only: [ ]         ← Relevant, normal emphasis
  parental_approval: [ ] ← GRAYED OUT (irrelevant given age_21_plus=true)
  legally_emancipated: [ ] ← GRAYED OUT (irrelevant given age_21_plus=true)
```

### ❌ DON'T: Hide irrelevant parameters
```
Inputs:
  age_21_plus: [✓ True]
  married: [ ]
  spousal_approval: [ ]
  beer_only: [ ]

  [Where are parental_approval and legally_emancipated?!]
```

### Why?
- Users may be **uncertain** about what they know
- They should be able to **provide any information** they have
- System shouldn't **force** a specific questioning order
- Users can **correct mistakes** by seeing all options

## Answer at Parent Node Level

### Requirement
Users should be able to assert that a **compound expression** is true without providing all details.

### Example: `A AND (B OR C)`

**Traditional approach (too restrictive):**
```
System: Is A true?
User: Yes
System: Is B true?
User: Actually, I'm not sure about B or C individually,
      but I know at least one of them is true.
System: ??? (stuck, can't proceed)
```

**Parent node approach (flexible):**
```
System shows all inputs:
  A: [✓]
  B OR C: [✓]  ← User can assert the disjunction itself

Result: DETERMINED TRUE
No need to ask about B or C individually!
```

### Benefits
1. **Privacy**: "I qualify under Section 3" without revealing which clause
2. **Uncertainty**: User knows aggregate but not components
3. **Efficiency**: Skip unnecessary detail questions

## Implementation Requirements

### 1. Query Plan Response Must Include:
- `stillNeeded`: Variables that could affect outcome
- `dontCare`: Variables that cannot affect outcome
- `asks`: Prioritized list of what to ask next
- `impact`: Numeric score for each variable's relevance

### 2. Bindings Must Support:
- Leaf-level assertions: `{"a": true, "b": false}`
- Parent-level assertions: `{"b_or_c": true}` (for expression labeled `b_or_c`)
- Mixed-level assertions: `{"a": true, "b_or_c": true}`

### 3. UI Must:
- Render all input widgets
- Apply visual styling based on relevance:
  - Normal: `impact > 0` (variable is relevant)
  - Grayed: `impact == 0` (variable is don't-care)
  - Highlighted: Variable appears in `asks[0]` (top priority)
- **CRITICAL**: Allow input on ALL widgets, even grayed-out ones
  - Grayed-out only means "currently irrelevant"
  - User may provide out-of-order input
  - Later inputs may make grayed parameters relevant again
  - Never disable or prevent input on grayed fields
- Allow input at any level of expression tree
- Update styling reactively as user provides input
- Re-query planner after each input to update relevance

### 4. Expression Tree Labels
L4 code should support labeling intermediate expressions:
```l4
DECIDE result IF a AND (b OR c)
  WHERE
    `b_or_c` MEANS b OR c
```

This allows:
- User to assert `b_or_c = true`
- System to bind at that level: `{"label": {"b_or_c": true}}`
- Query planner to skip asking about `b` and `c` individually

## Test Scenarios

### Scenario 1: Alcohol Purchase with Privacy
```
User provides: age_21_plus = true
System response:
  - GRAYS OUT: parental_approval, legally_emancipated (don't-care)
  - NORMAL: married, spousal_approval, beer_only (still relevant)
  - NEXT ASK: "married" (highest impact)

User provides: married = false
System response:
  - DETERMINED: True (unmarried person 21+ can purchase)
  - GRAYS OUT: spousal_approval (now don't-care)
```

### Scenario 2: Parent Node Assertion
```
User knows they satisfy "left branch" of (A AND B) OR (C AND D)
Instead of asking about A and B individually:
  User asserts: left_branch = true
  System: DETERMINED True

No need to reveal whether specifically A or B or both!
```

### Scenario 3: User Provides Extra Information
```
User provides multiple inputs upfront (even if not all needed):
  age_21_plus = true
  married = false
  beer_only = true
  parental_approval = false  ← Don't care, but user provided anyway

System:
  - Accepts all inputs
  - Uses relevant ones
  - Grays out don't-care ones
  - Shows result: True

No error or rejection of "unnecessary" information!
```

### Scenario 4: Reactive Relevance Changes
```
Step 1: User provides age_21_plus = true
  System grays out: parental_approval, legally_emancipated

Step 2: User fills in grayed-out field anyway: parental_approval = true
  System: Accepts input (even though currently grayed)

Step 3: User changes mind: age_21_plus = false
  System:
    - UN-grays parental_approval (now relevant!)
    - Uses the value user already provided
    - Grays married, spousal_approval, beer_only (now irrelevant)

Result: Smooth UX, no data loss, no frustration!
```

**Key Insight**: Grayed ≠ Disabled. Users can always provide input.

## Anti-Patterns to Avoid

### ❌ Wizard-Style Forced Order
```
System: Question 1: Are you 21+?
User: Yes
System: Question 2: Are you married?
User: Wait, I want to go back and also tell you I'm buying beer...
System: You can't go back, answer this question first!
```

### ❌ Hiding Information
```
System: Based on your age, I'm hiding these questions:
  - parental_approval (hidden)
  - legally_emancipated (hidden)

User: But I want to see all the criteria! How do I know what matters?
```

### ❌ Rejecting Extra Information
```
User provides all 6 inputs at once
System: ERROR - you provided unnecessary information
User: ??? I thought you wanted to help me!
```

## Implementation Notes

The query planner's `/query-plan` endpoint should return:
```json
{
  "outcome": "Unknown | True | False",
  "stillNeeded": ["var1", "var2"],
  "dontCare": ["var3", "var4"],
  "impact": {
    "var1": 1.5,
    "var2": 1.0,
    "var3": 0.0,
    "var4": 0.0
  },
  "asks": [
    {
      "label": "What is the value of var1?",
      "atoms": ["var1"],
      "schema": {"type": "boolean"}
    }
  ]
}
```

The UI uses this to:
1. Render all inputs (always visible)
2. Apply styles based on `impact` scores
3. Highlight top asks
4. Accept input at any level (leaf or parent)
