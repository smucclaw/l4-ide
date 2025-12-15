# Module A4 — Decision Service & API Integration

## Overview

L4's **Decision Service** transforms your L4 functions into REST APIs that can be called from any programming language or system. This enables:

- **Integration with external systems** (CRMs, ERPs, databases)
- **Web and mobile applications** calling L4 business rules
- **Microservices architecture** with L4 as a rules engine
- **Batch processing** of applications
- **Real-time policy evaluation**

This module covers:

1. How to expose L4 functions as API endpoints
2. Using `@desc export` annotations (replacing YAML files)
3. Automatic exposure from the Web IDE
4. Making REST API calls
5. JSON input/output handling

## The Decision Service Architecture

```
┌─────────────────┐
│  External App   │  (Web, Mobile, Backend)
│  (JavaScript,   │
│   Python, etc)  │
└────────┬────────┘
         │ HTTP/JSON
         ▼
┌─────────────────┐
│ Decision Service│
│   REST API      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   L4 Runtime    │
│   (Evaluator)   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   L4 Files      │
│  (.l4 source)   │
└─────────────────┘
```

## Part 1: Exposing Functions with @desc export

### The Old Way: YAML Files

Previously, you needed two files:

**eligibility.l4:**

```l4
GIVEN age IS A NUMBER
GIVETH A BOOLEAN
isEligible age MEANS age >= 18
```

**eligibility.yaml:**

```yaml
name: isEligible
description: "Check if person is eligible based on age"
parameters:
  type: object
  properties:
    age:
      type: number
      description: "The person's age"
  required: [age]
supportedEvalBackend: [jl4]
```

**Problems:**

- Duplication between L4 and YAML
- Synchronization burden (change function → update YAML)
- Deployment friction (two files per function)

### The New Way: @desc export Annotations

Now, everything is in one L4 file:

**eligibility.l4:**

```l4
@desc export Check if person is eligible based on age
GIVEN age IS A NUMBER @desc The person's age in years
GIVETH A BOOLEAN
isEligible age MEANS age >= 18
```

**That's it!** The function is automatically exposed via the API.

### Annotation Syntax

```l4
@desc [default] [export] <description text>
```

- **`export`**: Expose this function via API
- **`default`**: This is the primary function (implies `export`)
- **`<description>`**: Human-readable description for API docs

### Examples

#### Basic Export

```l4
@desc export Calculate insurance premium based on risk factors
GIVEN riskScore IS A NUMBER @desc Risk assessment score (0-1)
      age IS A NUMBER @desc Applicant age in years
GIVETH A NUMBER
calculatePremium riskScore age MEANS
    baseAmount TIMES riskMultiplier
    WHERE
        baseAmount MEANS 1000
        riskMultiplier MEANS
            IF riskScore GREATER THAN 0.7
            THEN 1.5
            ELSE 1.0
```

#### Default Export

When a module has one primary function:

```l4
@desc default export Main eligibility checker for work pass applications
GIVEN applicant IS An Employee @desc The employee applying for work pass
      employer IS A Company @desc The sponsoring company
GIVETH AN EligibilityResult
checkEligibility applicant employer MEANS
    -- implementation
```

The `default` keyword means this function is called when the module is invoked by UUID without specifying a function name.

#### Multiple Exports in One File

```l4
IMPORT prelude
IMPORT daydate

§ `Policy Evaluation Service`

@desc default export Comprehensive policy evaluation
GIVEN policy IS A Policy @desc The insurance policy to evaluate
      claim IS A Claim @desc The claim being filed
GIVETH A ClaimResult
evaluateClaim policy claim MEANS
    -- main evaluation logic

@desc export Validate policy data for completeness
GIVEN policy IS A Policy @desc The policy to validate
GIVETH A ValidationResult
validatePolicy policy MEANS
    -- validation logic

@desc export Calculate premium adjustments
GIVEN policy IS A Policy @desc Existing policy
      changes IS A LIST OF PolicyChange @desc Requested changes
GIVETH A NUMBER
calculateAdjustment policy changes MEANS
    -- adjustment calculation

-- This function is NOT exported (no @desc export)
-- Internal helper only
GIVEN x IS A NUMBER
helperFunction x MEANS x * 2
```

This exposes three API endpoints:

- `/functions/evaluateClaim/evaluation` (default)
- `/functions/validatePolicy/evaluation`
- `/functions/calculateAdjustment/evaluation`

### Parameter Descriptions

Add `@desc` annotations inline with parameters:

```l4
@desc export Determine loan eligibility
GIVEN
    income IS A NUMBER @desc Annual income in dollars
    creditScore IS A NUMBER @desc Credit score (300-850)
    requestedAmount IS A NUMBER @desc Loan amount requested
    hasCollateral IS A BOOLEAN @desc Whether collateral is provided
GIVETH A BOOLEAN
isEligibleForLoan income creditScore requestedAmount hasCollateral MEANS
    meetsIncomeRequirement AND meetsCreditRequirement
    WHERE
        meetsIncomeRequirement MEANS
            income GREATER THAN requestedAmount TIMES 0.3

        meetsCreditRequirement MEANS
            IF hasCollateral
            THEN creditScore AT LEAST 620
            ELSE creditScore AT LEAST 680
```

## Part 2: Automatic Exposure from Web IDE

### How It Works

When you save a file in the **L4 Web IDE** (https://jl4.legalese.com/):

1. Your L4 code is **automatically parsed and type-checked**
2. Functions marked with `@desc export` are **detected**
3. API endpoints are **immediately available** (no deployment step!)
4. You can **test them immediately** via the API

### Example Workflow

1. **Write L4 code** in the Web IDE:

```l4
@desc export Check voting eligibility
GIVEN age IS A NUMBER @desc Voter's age in years
GIVETH A BOOLEAN
canVote age MEANS age >= 18
```

2. **Save the file** (Ctrl+S or Cmd+S)

3. **API endpoint is live!**

   ```
   POST https://jl4.legalese.com/api/functions/canVote/evaluation
   ```

4. **Test it** with curl:

```bash
curl -X POST https://jl4.legalese.com/api/functions/canVote/evaluation \
  -H "Content-Type: application/json" \
  -d '{"age": 25}'
```

Response:

```json
{
  "result": true,
  "trace": [...]
}
```

### Finding Your Function's UUID

After saving, the Web IDE shows:

- **Function UUID**: Unique identifier for your function
- **API Endpoint**: Full URL to call
- **OpenAPI Schema**: JSON schema for parameters

You can share the UUID with others to let them call your function.

## Part 3: Making REST API Calls

### Endpoint Structure

```
POST /api/functions/{functionName}/evaluation
```

Or with UUID:

```
POST /api/uuid/{moduleUUID}/functions/{functionName}/evaluation
```

Or call default function:

```
POST /api/uuid/{moduleUUID}/evaluation
```

### Request Format

**Headers:**

```
Content-Type: application/json
```

**Body:**

```json
{
  "paramName1": value1,
  "paramName2": value2,
  ...
}
```

### Response Format

**Success (200 OK):**

```json
{
  "result": <evaluated_result>,
  "trace": [
    {
      "step": "Evaluating isEligible",
      "input": {"age": 25},
      "output": true
    }
  ],
  "executionTime": 45
}
```

**Error (400 Bad Request):**

```json
{
  "error": "Type error: Expected NUMBER, got STRING",
  "details": "Parameter 'age' must be a number"
}
```

### Visual Traces via GraphViz

Want more than plain JSON? Add `?trace=full&graphviz=true` to any evaluation call and the response includes a `graphviz` field containing DOT text you can send straight to `dot -Tsvg`. When you need ready-made images, call the companion endpoints:

```
POST /functions/{name}/evaluation/trace.png?trace=full
POST /functions/{name}/evaluation/trace.svg?trace=full
```

Each one reruns the evaluation with tracing enabled and streams back a PNG or SVG—perfect for design reviews or slide decks. Batch runs get the same option: append `graphviz=true` to `/functions/{name}/batch?trace=full` and every case shows an `@graphviz` attribute alongside the usual outputs. Install GraphViz’ `dot` binary (e.g. `brew install graphviz` or `apt-get install graphviz`) to take advantage of the PNG/SVG routes locally.

## Part 4: Calling from Different Languages

### JavaScript / TypeScript

```typescript
async function checkEligibility(age: number): Promise<boolean> {
  const response = await fetch(
    "https://jl4.legalese.com/api/functions/isEligible/evaluation",
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ age }),
    },
  );

  if (!response.ok) {
    throw new Error(`API error: ${response.statusText}`);
  }

  const data = await response.json();
  return data.result;
}

// Usage
const eligible = await checkEligibility(25);
console.log(eligible); // true
```

### Python

```python
import requests

def check_eligibility(age: int) -> bool:
    url = 'https://jl4.legalese.com/api/functions/isEligible/evaluation'
    payload = {'age': age}

    response = requests.post(url, json=payload)
    response.raise_for_status()

    return response.json()['result']

# Usage
eligible = check_eligibility(25)
print(eligible)  # True
```

### cURL

```bash
curl -X POST \
  https://jl4.legalese.com/api/functions/isEligible/evaluation \
  -H 'Content-Type: application/json' \
  -d '{"age": 25}'
```

### Java

```java
import java.net.http.*;
import java.net.URI;
import com.google.gson.Gson;
import com.google.gson.JsonObject;

public class L4Client {
    private static final String BASE_URL =
        "https://jl4.legalese.com/api/functions";
    private static final HttpClient client = HttpClient.newHttpClient();
    private static final Gson gson = new Gson();

    public static boolean checkEligibility(int age) throws Exception {
        JsonObject payload = new JsonObject();
        payload.addProperty("age", age);

        HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create(BASE_URL + "/isEligible/evaluation"))
            .header("Content-Type", "application/json")
            .POST(HttpRequest.BodyPublishers.ofString(payload.toString()))
            .build();

        HttpResponse<String> response =
            client.send(request, HttpResponse.BodyHandlers.ofString());

        JsonObject result = gson.fromJson(response.body(), JsonObject.class);
        return result.get("result").getAsBoolean();
    }

    public static void main(String[] args) throws Exception {
        boolean eligible = checkEligibility(25);
        System.out.println("Eligible: " + eligible);
    }
}
```

## Part 5: Complex Data Types in JSON

### Records as JSON Objects

**L4:**

```l4
DECLARE Employee HAS
    name IS A STRING
    age IS A NUMBER
    salary IS A NUMBER

@desc export Check if employee qualifies for bonus
GIVEN employee IS AN Employee @desc The employee to evaluate
GIVETH A BOOLEAN
qualifiesForBonus employee MEANS
    employee's salary GREATER THAN 50000
    AND employee's age AT LEAST 25
```

**API Call:**

```json
{
  "employee": {
    "name": "Alice",
    "age": 30,
    "salary": 60000
  }
}
```

### Enums as Strings

**L4:**

```l4
DECLARE EmploymentCategory IS ONE OF
    TechProfessional
    HealthcareWorker
    Researcher

@desc export Get minimum salary for category
GIVEN category IS AN EmploymentCategory @desc Employment category
GIVETH A NUMBER
minimumSalary category MEANS
    CONSIDER category
    WHEN TechProfessional  THEN 5000
    WHEN HealthcareWorker  THEN 4500
    WHEN Researcher        THEN 5500
```

**API Call:**

```json
{
  "category": "TechProfessional"
}
```

**Response:**

```json
{
  "result": 5000
}
```

### Lists as JSON Arrays

**L4:**

```l4
IMPORT prelude

@desc export Calculate total salary for all employees
GIVEN employees IS A LIST OF Employee @desc List of employees
GIVETH A NUMBER
totalSalary employees MEANS
    sum salaries
    WHERE
        salaries MEANS map getSalary employees
        getSalary emp MEANS emp's salary
```

**API Call:**

```json
{
  "employees": [
    { "name": "Alice", "age": 30, "salary": 60000 },
    { "name": "Bob", "age": 25, "salary": 50000 },
    { "name": "Charlie", "age": 35, "salary": 70000 }
  ]
}
```

**Response:**

```json
{
  "result": 180000
}
```

### MAYBE Types

**L4:**

```l4
IMPORT prelude

DECLARE Document HAS
    title IS A STRING
    expiryDate IS A MAYBE DATE

@desc export Check if document is expired
GIVEN doc IS A Document @desc The document to check
      checkDate IS A DATE @desc Date to check against
GIVETH A BOOLEAN
isExpired doc checkDate MEANS
    CONSIDER doc's expiryDate
    WHEN NOTHING THEN FALSE  -- No expiry = never expired
    WHEN JUST expiry THEN checkDate GREATER THAN expiry
```

**API Call (with expiry):**

```json
{
  "doc": {
    "title": "Passport",
    "expiryDate": "2025-12-31"
  },
  "checkDate": "2026-01-01"
}
```

**API Call (no expiry):**

```json
{
  "doc": {
    "title": "Birth Certificate",
    "expiryDate": null
  },
  "checkDate": "2026-01-01"
}
```

### Dates as ISO 8601 Strings

**L4:**

```l4
IMPORT daydate

@desc export Calculate age as of a specific date
GIVEN birthDate IS A DATE @desc Person's date of birth
      asOfDate IS A DATE @desc Date to calculate age as of
GIVETH A NUMBER
ageAsOf birthDate asOfDate MEANS
    FLOOR ((asOfDate MINUS birthDate) DIVIDED BY 365.2425)
```

**API Call:**

```json
{
  "birthDate": "1990-03-15",
  "asOfDate": "2025-12-01"
}
```

**Response:**

```json
{
  "result": 35
}
```

## Part 6: Batch Processing

### Single Evaluation Endpoint

For one-off evaluations:

```
POST /api/functions/{functionName}/evaluation
```

### Batch Endpoint

For processing multiple cases efficiently:

```
POST /api/functions/{functionName}/batch
```

**Request:**

```json
{
  "cases": [
    { "age": 17, "salary": 45000 },
    { "age": 25, "salary": 55000 },
    { "age": 30, "salary": 60000 },
    { "age": 16, "salary": 40000 }
  ]
}
```

**Response:**

```json
{
  "results": [
    { "result": false, "case": 0 },
    { "result": true, "case": 1 },
    { "result": true, "case": 2 },
    { "result": false, "case": 3 }
  ],
  "totalProcessed": 4,
  "executionTime": 123
}
```

### Batch Processing Example (Python)

```python
import requests

def batch_check_eligibility(applicants):
    url = 'https://jl4.legalese.com/api/functions/isEligible/batch'
    cases = [{'age': a['age'], 'salary': a['salary']}
             for a in applicants]

    response = requests.post(url, json={'cases': cases})
    response.raise_for_status()

    return response.json()['results']

# Process 1000 applications
applicants = load_applications()  # Returns list of dicts
results = batch_check_eligibility(applicants)

# Filter approved
approved = [app for app, result in zip(applicants, results)
            if result['result']]
```

## Part 7: Error Handling

### Common Errors

#### 1. Type Mismatch

**Request:**

```json
{ "age": "twenty-five" }
```

**Response (400):**

```json
{
  "error": "Type error",
  "message": "Expected NUMBER for parameter 'age', got STRING",
  "parameter": "age",
  "expectedType": "NUMBER",
  "actualValue": "twenty-five"
}
```

#### 2. Missing Required Parameter

**Request:**

```json
{}
```

**Response (400):**

```json
{
  "error": "Missing parameter",
  "message": "Required parameter 'age' not provided",
  "parameter": "age"
}
```

#### 3. Runtime Error

**Response (500):**

```json
{
  "error": "Runtime error",
  "message": "Division by zero in calculation",
  "trace": [...]
}
```

### Handling Errors in Code

**JavaScript:**

```typescript
async function safeEvaluate(params: any) {
  try {
    const response = await fetch(apiUrl, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(params),
    });

    if (!response.ok) {
      const error = await response.json();
      console.error("API Error:", error.message);
      return null;
    }

    const data = await response.json();
    return data.result;
  } catch (error) {
    console.error("Network error:", error);
    return null;
  }
}
```

## Part 8: OpenAPI Schema

The Decision Service automatically generates OpenAPI (Swagger) schemas for your functions.

**Access the schema:**

```
GET /api/functions/{functionName}/schema
```

**Example Response:**

```json
{
  "openapi": "3.0.0",
  "info": {
    "title": "isEligible Function",
    "version": "1.0.0",
    "description": "Check if person is eligible based on age"
  },
  "paths": {
    "/evaluation": {
      "post": {
        "summary": "Evaluate isEligible",
        "requestBody": {
          "required": true,
          "content": {
            "application/json": {
              "schema": {
                "type": "object",
                "properties": {
                  "age": {
                    "type": "number",
                    "description": "The person's age in years"
                  }
                },
                "required": ["age"]
              }
            }
          }
        },
        "responses": {
          "200": {
            "description": "Successful evaluation",
            "content": {
              "application/json": {
                "schema": {
                  "type": "object",
                  "properties": {
                    "result": { "type": "boolean" },
                    "trace": { "type": "array" }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
```

You can import this schema into **Postman** or **Swagger UI** for interactive testing.

## Part 9: Best Practices

### 1. Clear Function Names

```l4
-- GOOD: Descriptive mixfix names
@desc export
`applicant` `is eligible for` `category`

-- AVOID: Cryptic names
@desc export
chkElg app cat
```

### 2. Comprehensive Descriptions

```l4
-- GOOD: Explains what, when, and why
@desc export Calculate late payment penalty based on days overdue and original amount. Caps at 25% of original amount per regulations section 4.5.

-- AVOID: Too brief
@desc export Calculate penalty
```

### 3. Document Parameter Units

```l4
@desc export Calculate compound interest
GIVEN
    principal IS A NUMBER @desc Initial amount in dollars
    rate IS A NUMBER @desc Annual interest rate (as decimal, e.g. 0.05 for 5%)
    years IS A NUMBER @desc Number of years
GIVETH A NUMBER
```

### 4. Handle Edge Cases

```l4
@desc export Calculate discount percentage
GIVEN
    price IS A NUMBER @desc Original price
    discountAmount IS A NUMBER @desc Discount amount
GIVETH A NUMBER
calculateDiscountPercent price discountAmount MEANS
    IF price EQUALS 0
    THEN 0  -- Avoid division by zero
    ELSE (discountAmount DIVIDED BY price) TIMES 100
```

### 5. Validate Inputs

```l4
@desc export Calculate age-based premium multiplier
GIVEN age IS A NUMBER @desc Customer age (must be 0-120)
GIVETH A NUMBER
premiumMultiplier age MEANS
    IF age LESS THAN 0 OR age GREATER THAN 120
    THEN 0  -- Invalid age
    ELSE IF age LESS THAN 25
    THEN 1.5
    ELSE IF age LESS THAN 65
    THEN 1.0
    ELSE 1.3
```

## Key Takeaways

1. **Use `@desc export`** to expose functions as APIs (no YAML needed)
2. **Web IDE auto-exposes** functions when you save
3. **@desc annotations on parameters** become API documentation
4. **Call from any language** using standard HTTP/JSON
5. **Records → JSON objects**, **Enums → strings**, **Lists → arrays**
6. **Batch endpoints** for processing multiple cases efficiently
7. **OpenAPI schema** generated automatically
8. **Clear descriptions** make APIs self-documenting

## Exercises

### Exercise 1: Simple Export

Create an L4 function that calculates BMI, export it, and call it from curl.

### Exercise 2: Complex Types

Create a function that takes an Employee record and returns a salary recommendation. Test with Postman.

### Exercise 3: Batch Processing

Write a Python script that processes 100 loan applications in batch.

### Exercise 4: Error Handling

Create a function with validation, intentionally trigger errors, and handle them gracefully in client code.

## Next Steps

In **Module A5**, we'll cover regression testing and change control—ensuring that updates to your L4 code don't break existing behavior.
