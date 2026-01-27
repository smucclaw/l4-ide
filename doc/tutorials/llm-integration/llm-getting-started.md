# Getting Started with LLM Integration

Set up and use Large Language Models with L4.

**Audience:** Developers, AI practitioners  
**Prerequisites:** Basic L4 knowledge, API access to an LLM  
**Time:** 15 minutes  
**Goal:** Understand how to combine LLM capabilities with L4's formal reasoning

---

## Why Combine LLM and L4?

LLMs and L4 have complementary strengths:

| LLM Strengths                  | L4 Strengths         |
| ------------------------------ | -------------------- |
| Natural language understanding | Formal precision     |
| Handling ambiguity             | Logical consistency  |
| Text generation                | Verifiable reasoning |
| Flexibility                    | Reproducibility      |

**Combined approach:**

- LLM handles fuzzy interpretation
- L4 handles formal reasoning
- Together: explainable, auditable decisions

---

## The Hybrid Pattern

```
┌─────────────────────────────────────────────────┐
│                  Input Text                      │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│     LLM: Extract structured facts               │
│     "Is this a 'reasonable time'?" → TRUE/FALSE │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│     L4: Apply formal rules                      │
│     IF `was reasonable time` THEN ...           │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│     Output: Decision + Explanation              │
└─────────────────────────────────────────────────┘
```

---

## Step 1: Define LLM-Sourced Facts

Use `ASSUME` to declare facts that come from LLM analysis:

```l4
§ `LLM Integration Example`

-- Facts determined by LLM
ASSUME `was reasonable time` IS BOOLEAN
ASSUME `tone is professional` IS BOOLEAN
ASSUME `contains required elements` IS BOOLEAN

-- Pure L4 logic
DECIDE `is valid notice` IF
    `was reasonable time`
    AND `tone is professional`
    AND `contains required elements`
```

### Key Insight

The LLM provides **inputs** (the ASSUME values). L4 provides **reasoning** (the DECIDE logic). This separation keeps the formal logic clean and auditable.

---

## Step 2: Design LLM Prompts

Create prompts that return structured answers:

```l4
{-
Prompt Template for "reasonable time" judgment:

"You are a legal assistant analyzing timing in a legal context.

Question: Was the action taken within a reasonable time?

Context:
- Required deadline: [DEADLINE]
- Actual date: [ACTUAL_DATE]
- Circumstances: [CIRCUMSTANCES]

Consider:
1. Industry standard timelines
2. Any extenuating circumstances
3. Whether delay caused harm

Answer with exactly one of:
- TRUE (reasonable time)
- FALSE (unreasonable delay)

Then explain your reasoning in 2-3 sentences."
-}
```

---

## Step 3: Create a Wrapper Type

Define a type that captures both the judgment and its provenance:

```l4
-- LLM judgment with audit trail
DECLARE LLMJudgment
    HAS question IS A STRING
        answer IS A BOOLEAN
        confidence IS A NUMBER   -- 0.0 to 1.0
        reasoning IS A STRING
        model IS A STRING        -- e.g., "gpt-4"
        timestamp IS A STRING

-- Example judgment
timingJudgment MEANS LLMJudgment
    "Was delivery within reasonable time?"
    TRUE
    0.85
    "The 5-day delivery is within industry standard of 7 days, and no harm resulted from the timing."
    "gpt-4"
    "2024-01-15T10:30:00Z"
```

---

## Step 4: Integrate with L4 Rules

Use the LLM judgment in your L4 rules:

```l4
-- Extract the answer from judgment
GIVEN judgment IS A LLMJudgment
GIVETH A BOOLEAN
`judgment is yes` MEANS judgment's answer

-- Use in rule with confidence threshold
GIVEN judgment IS A LLMJudgment
      threshold IS A NUMBER
GIVETH A BOOLEAN
`confident yes` MEANS
    judgment's answer
    AND judgment's confidence >= threshold

-- Main rule using LLM input
GIVEN deliveryJudgment IS A LLMJudgment
      qualityJudgment IS A LLMJudgment
GIVETH A BOOLEAN
DECIDE `delivery acceptable` IF
    `confident yes` deliveryJudgment 0.7
    AND `confident yes` qualityJudgment 0.7
```

---

## Step 5: Preserve Audit Trail

Keep full records for compliance:

```l4
-- Decision record with full audit trail
DECLARE Decision
    HAS rule IS A STRING
        inputs IS A LIST OF LLMJudgment
        outcome IS A BOOLEAN
        timestamp IS A STRING

-- Create decision record
GIVEN outcome IS A BOOLEAN
      judgments IS A LIST OF LLMJudgment
GIVETH A Decision
`record decision` MEANS Decision
    "delivery acceptable"
    judgments
    outcome
    "2024-01-15T10:35:00Z"
```

---

## Example: Complete Integration

```l4
§ `Contract Compliance Check with LLM`

-- LLM-sourced judgments (would come from API in practice)
ASSUME timingJudgment IS A LLMJudgment
ASSUME qualityJudgment IS A LLMJudgment
ASSUME professionalismJudgment IS A LLMJudgment

-- Confidence threshold for accepting LLM judgments
minConfidence MEANS 0.75

-- Individual checks
DECIDE `timing ok` IF
    timingJudgment's answer
    AND timingJudgment's confidence >= minConfidence

DECIDE `quality ok` IF
    qualityJudgment's answer
    AND qualityJudgment's confidence >= minConfidence

DECIDE `professionalism ok` IF
    professionalismJudgment's answer
    AND professionalismJudgment's confidence >= minConfidence

-- Combined compliance check
DECIDE `is compliant` IF
    `timing ok`
    AND `quality ok`
    AND `professionalism ok`

-- Test with sample data
sampleTiming MEANS LLMJudgment
    "Was delivery timely?"
    TRUE
    0.90
    "Delivered 2 days early"
    "gpt-4"
    "2024-01-15T10:30:00Z"

sampleQuality MEANS LLMJudgment
    "Does quality meet specifications?"
    TRUE
    0.85
    "All quality metrics passed"
    "gpt-4"
    "2024-01-15T10:31:00Z"

sampleProfessionalism MEANS LLMJudgment
    "Was conduct professional?"
    TRUE
    0.80
    "Communication was clear and timely"
    "gpt-4"
    "2024-01-15T10:32:00Z"

#CHECK `is compliant` WITH
    timingJudgment IS sampleTiming,
    qualityJudgment IS sampleQuality,
    professionalismJudgment IS sampleProfessionalism
```

---

## Best Practices

### 1. Clear Prompt Design

- Ask for binary (TRUE/FALSE) answers
- Request confidence scores
- Require reasoning explanation

### 2. Confidence Thresholds

- Set minimum confidence levels
- Have fallback for low-confidence responses
- Consider human review for edge cases

### 3. Audit Everything

- Log all LLM calls
- Store prompts, responses, timestamps
- Preserve for compliance

### 4. Separation of Concerns

- LLM: interpretation, classification
- L4: logic, consequences, obligations
- Keep formal rules in L4, not prompts

---

## Limitations

- **LLM responses can vary** - same prompt may give different answers
- **No guaranteed consistency** - unlike L4's deterministic logic
- **Confidence scores are subjective** - calibrate carefully
- **Audit requirements** - ensure you can explain decisions

---

## What You Learned

- How to combine LLM and L4 strengths
- Using ASSUME for LLM-sourced facts
- Designing prompts for structured answers
- Creating audit trails
- Best practices for hybrid systems

---

## Next Steps

- [Legislative Ingestion](legislative-ingestion.md) - Use LLM to help encode legislation
- [LLM Integration Spec](../../../specs/done/LLM-INTEGRATION-SPEC.md) - Technical details
- [Foundation Course](../../courses/foundation/README.md) - Learn L4 fundamentals
