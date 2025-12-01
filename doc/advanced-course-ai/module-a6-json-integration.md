# Module A6 — JSON Integration (Input & Output)

## Overview

Real-world L4 systems need to exchange data with:
- **Databases** (PostgreSQL, MongoDB, etc.)
- **External APIs** (REST, GraphQL)
- **Web applications** (React, Vue, Angular)
- **Mobile apps** (iOS, Android)
- **Enterprise systems** (ERPs, CRMs)

JSON is the universal data interchange format. This module covers:
1. Mapping L4 types to JSON schemas
2. Handling JSON input in decision service calls
3. Generating JSON output
4. Dealing with mismatched schemas
5. Integration patterns with external systems

## Part 1: L4 to JSON Type Mapping

### Basic Types

| L4 Type | JSON Type | Example |
|---------|-----------|---------|
| `NUMBER` | `number` | `42`, `3.14` |
| `STRING` | `string` | `"hello"` |
| `BOOLEAN` | `boolean` | `true`, `false` |
| `DATE` | `string` (ISO 8601) | `"2025-12-01"` |

### Records → JSON Objects

**L4:**
```l4
DECLARE Employee HAS
    name IS A STRING
    age IS A NUMBER
    salary IS A NUMBER
```

**JSON:**
```json
{
  "name": "Alice",
  "age": 30,
  "salary": 60000
}
```

### Enums → Strings

**L4:**
```l4
DECLARE EmploymentCategory IS ONE OF
    TechProfessional
    HealthcareWorker
    Researcher
```

**JSON:**
```json
{
  "category": "TechProfessional"
}
```

### Lists → JSON Arrays

**L4:**
```l4
DECLARE Team HAS
    members IS A LIST OF STRING
```

**JSON:**
```json
{
  "members": ["Alice", "Bob", "Charlie"]
}
```

### MAYBE → Nullable Fields

**L4:**
```l4
DECLARE Document HAS
    title IS A STRING
    expiryDate IS A MAYBE DATE
```

**JSON with value:**
```json
{
  "title": "Passport",
  "expiryDate": "2025-12-31"
}
```

**JSON without value:**
```json
{
  "title": "Birth Certificate",
  "expiryDate": null
}
```

## Part 2: Generating JSON Schemas from L4

### Automatic Schema Generation

The L4 compiler can generate JSON schemas for your types:

```bash
# Generate schema for a type
cabal run jl4-cli -- --export-json-schema src/types.l4 > schema.json
```

### Example: Employee Type

**L4 Input:**
```l4
DECLARE Employee HAS
    name IS A STRING
    age IS A NUMBER
    salary IS A NUMBER
    category IS AN EmploymentCategory

DECLARE EmploymentCategory IS ONE OF
    TechProfessional
    HealthcareWorker
    Researcher
```

**Generated JSON Schema:**
```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Employee",
  "type": "object",
  "properties": {
    "name": {
      "type": "string",
      "description": "Employee name"
    },
    "age": {
      "type": "number",
      "description": "Employee age"
    },
    "salary": {
      "type": "number",
      "description": "Monthly salary"
    },
    "category": {
      "type": "string",
      "enum": ["TechProfessional", "HealthcareWorker", "Researcher"],
      "description": "Employment category"
    }
  },
  "required": ["name", "age", "salary", "category"]
}
```

### Using Schemas for Validation

**TypeScript (with Zod):**
```typescript
import { z } from 'zod';

const EmployeeSchema = z.object({
  name: z.string(),
  age: z.number(),
  salary: z.number(),
  category: z.enum(['TechProfessional', 'HealthcareWorker', 'Researcher']),
});

type Employee = z.infer<typeof EmployeeSchema>;

// Validate JSON before sending to L4
const employee = EmployeeSchema.parse(jsonData);
```

**Python (with pydantic):**
```python
from pydantic import BaseModel
from enum import Enum

class EmploymentCategory(str, Enum):
    TECH_PROFESSIONAL = "TechProfessional"
    HEALTHCARE_WORKER = "HealthcareWorker"
    RESEARCHER = "Researcher"

class Employee(BaseModel):
    name: str
    age: int
    salary: float
    category: EmploymentCategory

# Validate JSON
employee = Employee.parse_obj(json_data)
```

## Part 3: Handling JSON Input

### Simple Function Call

**L4 Function:**
```l4
@desc export Check if employee qualifies for bonus
GIVEN employee IS AN Employee @desc The employee to evaluate
GIVETH A BOOLEAN
qualifiesForBonus employee MEANS
    employee's salary GREATER THAN 50000
    AND employee's age AT LEAST 25
```

**API Call:**
```bash
curl -X POST http://localhost:3000/api/functions/qualifiesForBonus/evaluation \
  -H "Content-Type: application/json" \
  -d '{
    "employee": {
      "name": "Alice",
      "age": 30,
      "salary": 60000,
      "category": "TechProfessional"
    }
  }'
```

**Response:**
```json
{
  "result": true,
  "trace": [...]
}
```

### Nested Objects

**L4:**
```l4
DECLARE Address HAS
    street IS A STRING
    city IS A STRING
    country IS A STRING

DECLARE Person HAS
    name IS A STRING
    age IS A NUMBER
    address IS An Address

@desc export Check if person is local resident
GIVEN person IS A Person
GIVETH A BOOLEAN
isLocalResident person MEANS
    person's address's country EQUALS "Singapore"
```

**JSON Input:**
```json
{
  "person": {
    "name": "Alice",
    "age": 30,
    "address": {
      "street": "123 Main St",
      "city": "Singapore",
      "country": "Singapore"
    }
  }
}
```

### Lists of Objects

**L4:**
```l4
IMPORT prelude

@desc export Calculate total team salary
GIVEN employees IS A LIST OF Employee
GIVETH A NUMBER
totalSalary employees MEANS
    sum salaries
    WHERE
        salaries MEANS map getSalary employees
        getSalary emp MEANS emp's salary
```

**JSON Input:**
```json
{
  "employees": [
    {"name": "Alice", "age": 30, "salary": 60000, "category": "TechProfessional"},
    {"name": "Bob", "age": 25, "salary": 50000, "category": "TechProfessional"},
    {"name": "Charlie", "age": 35, "salary": 70000, "category": "Researcher"}
  ]
}
```

## Part 4: Generating JSON Output

### Simple Return Values

**L4:**
```l4
@desc export Calculate employee bonus
GIVEN employee IS AN Employee
GIVETH A NUMBER
calculateBonus employee MEANS
    IF employee's salary GREATER THAN 50000
    THEN employee's salary TIMES 0.15
    ELSE employee's salary TIMES 0.10
```

**Response:**
```json
{
  "result": 9000.0
}
```

### Record Return Values

**L4:**
```l4
DECLARE BonusCalculation HAS
    baseBonus IS A NUMBER
    performanceBonus IS A NUMBER
    totalBonus IS A NUMBER
    percentage IS A NUMBER

@desc export Calculate detailed bonus breakdown
GIVEN employee IS AN Employee
      performanceScore IS A NUMBER
GIVETH A BonusCalculation
detailedBonus employee performanceScore MEANS
    BonusCalculation WITH
        baseBonus IS base
        performanceBonus IS perf
        totalBonus IS total
        percentage IS pct
    WHERE
        base MEANS employee's salary TIMES 0.10
        perf MEANS performanceScore TIMES 100
        total MEANS base PLUS perf
        pct MEANS (total DIVIDED BY employee's salary) TIMES 100
```

**Response:**
```json
{
  "result": {
    "baseBonus": 6000.0,
    "performanceBonus": 850.0,
    "totalBonus": 6850.0,
    "percentage": 11.42
  }
}
```

### List Return Values

**L4:**
```l4
IMPORT prelude

@desc export Filter eligible employees
GIVEN employees IS A LIST OF Employee
GIVETH A LIST OF Employee
eligibleEmployees employees MEANS
    filter isEligible employees
    WHERE
        isEligible emp MEANS emp's salary GREATER THAN 50000
```

**Response:**
```json
{
  "result": [
    {"name": "Alice", "age": 30, "salary": 60000, "category": "TechProfessional"},
    {"name": "Charlie", "age": 35, "salary": 70000, "category": "Researcher"}
  ]
}
```

## Part 5: Handling Date/Time

### Date Input Formats

L4 accepts ISO 8601 date strings:

**Supported formats:**
```json
{
  "birthDate": "2025-12-01",
  "timestamp": "2025-12-01T14:30:00Z",
  "localTime": "2025-12-01T14:30:00+08:00"
}
```

**L4:**
```l4
IMPORT daydate

@desc export Calculate age as of specific date
GIVEN birthDate IS A DATE
      asOfDate IS A DATE
GIVETH A NUMBER
ageAsOf birthDate asOfDate MEANS
    FLOOR ((asOfDate MINUS birthDate) DIVIDED BY 365.2425)
```

**Request:**
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

### Date Output

L4 dates are serialized as ISO 8601 strings:

```json
{
  "result": "2025-12-01"
}
```

## Part 6: Error Handling and Validation

### Type Mismatch Errors

**Request (wrong type):**
```json
{
  "age": "thirty"
}
```

**Response (400 Bad Request):**
```json
{
  "error": "Type mismatch",
  "message": "Expected NUMBER for parameter 'age', got STRING",
  "parameter": "age",
  "expectedType": "NUMBER",
  "receivedValue": "thirty"
}
```

### Missing Required Fields

**Request (missing field):**
```json
{
  "name": "Alice"
}
```

**Response (400 Bad Request):**
```json
{
  "error": "Missing required field",
  "message": "Field 'age' is required but not provided",
  "field": "age"
}
```

### Enum Validation

**Request (invalid enum value):**
```json
{
  "category": "InvalidCategory"
}
```

**Response (400 Bad Request):**
```json
{
  "error": "Invalid enum value",
  "message": "Value 'InvalidCategory' is not a valid EmploymentCategory",
  "field": "category",
  "validValues": ["TechProfessional", "HealthcareWorker", "Researcher"]
}
```

## Part 7: Integration Patterns

### API Discovery with Swagger

The L4 decision service automatically generates Swagger/OpenAPI documentation for all exported functions:

**Swagger UI** (interactive documentation):
```
http://localhost:3000/swagger-ui
```

**OpenAPI Specification** (JSON):
```
http://localhost:3000/swagger
```

The Swagger UI provides:
- **Interactive API exploration** - Test endpoints directly from your browser
- **Automatic schema documentation** - See request/response types for all functions
- **Example requests** - Copy-paste ready API calls
- **Try it out** - Execute functions with sample data

**Example: Using Swagger UI**

1. Open http://localhost:3000/swagger-ui in your browser
2. Find the `qualifiesForBonus` endpoint
3. Click "Try it out"
4. Enter sample JSON:
```json
{
  "employee": {
    "name": "Alice",
    "age": 30,
    "salary": 60000,
    "category": "TechProfessional"
  }
}
```
5. Click "Execute" to see the result

**Generating Client Libraries**

Use the OpenAPI spec to generate type-safe client libraries:

```bash
# Generate TypeScript client
npx openapi-generator-cli generate \
  -i http://localhost:3000/swagger \
  -g typescript-axios \
  -o ./generated/l4-client

# Generate Python client
openapi-generator-cli generate \
  -i http://localhost:3000/swagger \
  -g python \
  -o ./generated/l4-client
```

### Pattern 1: REST API Bridge

```typescript
// TypeScript client for L4 decision service

interface L4Client {
  evaluate<T>(functionName: string, params: any): Promise<T>;
}

class L4RestClient implements L4Client {
  constructor(private baseUrl: string) {}

  async evaluate<T>(functionName: string, params: any): Promise<T> {
    const response = await fetch(
      `${this.baseUrl}/api/functions/${functionName}/evaluation`,
      {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(params),
      }
    );

    if (!response.ok) {
      const error = await response.json();
      throw new L4Error(error.message, error);
    }

    const data = await response.json();
    return data.result;
  }
}

// Usage
const client = new L4RestClient('https://l4.example.com');

const bonus = await client.evaluate<number>('calculateBonus', {
  employee: {
    name: 'Alice',
    age: 30,
    salary: 60000,
    category: 'TechProfessional',
  },
});

console.log(`Bonus: $${bonus}`);
```

### Pattern 2: Database Integration

```python
# Python script to evaluate L4 rules on database records

import psycopg2
import requests

# Connect to database
conn = psycopg2.connect("dbname=hr_system")
cursor = conn.cursor()

# Fetch employees
cursor.execute("SELECT name, age, salary, category FROM employees")
employees = cursor.fetchall()

# Evaluate each employee
l4_url = "https://l4.example.com/api/functions/qualifiesForBonus/evaluation"

results = []
for name, age, salary, category in employees:
    response = requests.post(l4_url, json={
        "employee": {
            "name": name,
            "age": age,
            "salary": salary,
            "category": category
        }
    })

    if response.ok:
        qualifies = response.json()['result']
        results.append((name, qualifies))

# Update database with results
for name, qualifies in results:
    cursor.execute(
        "UPDATE employees SET qualifies_for_bonus = %s WHERE name = %s",
        (qualifies, name)
    )

conn.commit()
```

### Pattern 3: Batch Processing

```javascript
// Node.js batch processor

const axios = require('axios');

async function batchEvaluate(functionName, cases) {
  const response = await axios.post(
    `https://l4.example.com/api/functions/${functionName}/batch`,
    { cases }
  );

  return response.data.results;
}

// Load data from CSV, database, etc.
const employees = loadEmployees();

// Process in batches of 100
const batchSize = 100;
for (let i = 0; i < employees.length; i += batchSize) {
  const batch = employees.slice(i, i + batchSize);
  const cases = batch.map(emp => ({ employee: emp }));

  const results = await batchEvaluate('qualifiesForBonus', cases);

  // Store results
  batch.forEach((emp, idx) => {
    emp.qualifies = results[idx].result;
  });
}
```

### Pattern 4: Event-Driven Architecture

```python
# AWS Lambda handler calling L4 service

import json
import requests
import boto3

def lambda_handler(event, context):
    """
    Triggered by S3 upload of new employee data.
    Evaluates eligibility and stores result in DynamoDB.
    """

    # Parse S3 event
    bucket = event['Records'][0]['s3']['bucket']['name']
    key = event['Records'][0]['s3']['object']['key']

    # Fetch employee data
    s3 = boto3.client('s3')
    obj = s3.get_object(Bucket=bucket, Key=key)
    employee_data = json.loads(obj['Body'].read())

    # Call L4 service
    response = requests.post(
        'https://l4.example.com/api/functions/qualifiesForBonus/evaluation',
        json={'employee': employee_data}
    )

    result = response.json()['result']

    # Store in DynamoDB
    dynamodb = boto3.resource('dynamodb')
    table = dynamodb.Table('EmployeeEligibility')

    table.put_item(Item={
        'employeeId': employee_data['id'],
        'qualifies': result,
        'evaluatedAt': context.request_id,
        'timestamp': context.get_remaining_time_in_millis()
    })

    return {'statusCode': 200, 'body': json.dumps({'result': result})}
```

## Part 8: Schema Evolution

### Handling Missing Optional Fields

**Old L4 (v1):**
```l4
DECLARE Employee HAS
    name IS A STRING
    age IS A NUMBER
```

**New L4 (v2):**
```l4
DECLARE Employee HAS
    name IS A STRING
    age IS A NUMBER
    department IS A MAYBE STRING  -- New optional field
```

**Old JSON still works:**
```json
{
  "name": "Alice",
  "age": 30
}
```

L4 treats missing `department` as `NOTHING`.

### Backward-Compatible Changes

**Safe changes:**
- Adding optional (MAYBE) fields
- Adding new functions
- Adding new enum values (if handled with OTHERWISE)

**Breaking changes:**
- Removing fields
- Changing field types
- Making optional fields required
- Removing enum values

### Versioning Strategy

```l4
-- Version in API path
-- v1/api/functions/calculate/evaluation
-- v2/api/functions/calculate/evaluation

§ `Version 1 - Legacy`
@desc export V1 calculation (deprecated)
GIVEN employee IS An EmployeeV1
GIVETH A NUMBER
calculateV1 employee MEANS ...

§ `Version 2 - Current`
@desc default export V2 calculation
GIVEN employee IS An EmployeeV2
GIVETH A NUMBER
calculateV2 employee MEANS ...
```

## Part 9: Performance Optimization

### Minimize JSON Size

```l4
-- AVOID: Returning full objects when only IDs needed
@desc export Get eligible employee IDs
GIVEN employees IS A LIST OF Employee
GIVETH A LIST OF STRING
eligibleIds employees MEANS
    map getIdstr (filter isEligible employees)
    WHERE
        isEligible emp MEANS emp's salary GREATER THAN 50000
        getId emp MEANS emp's id

-- GOOD: Return only what's needed
```

### Batch vs Individual Calls

```
Single calls: 100 employees × 50ms = 5000ms
Batch call:   1 call × 500ms = 500ms
```

Use batch endpoints for processing multiple items.

### Caching Results

```python
import requests
from functools import lru_cache

@lru_cache(maxsize=1000)
def evaluate_eligibility(employee_id: str, salary: float) -> bool:
    """Cache results for repeated queries"""
    response = requests.post(
        'https://l4.example.com/api/functions/qualifiesForBonus/evaluation',
        json={'employee': {'id': employee_id, 'salary': salary}}
    )
    return response.json()['result']
```

## Key Takeaways

1. **L4 types map naturally to JSON** (records→objects, enums→strings, lists→arrays)
2. **Generate JSON schemas** from L4 for validation
3. **Use MAYBE** for optional/nullable fields
4. **Dates are ISO 8601 strings**
5. **Batch processing** is more efficient than individual calls
6. **Validate input on client side** before sending to L4
7. **Handle schema evolution** with optional fields and versioning
8. **Cache results** when appropriate
9. **Return minimal data** for performance

## Exercises

### Exercise 1: Schema Generation
Create an L4 type for a complex domain (e.g., insurance policy) and generate its JSON schema.

### Exercise 2: Integration Client
Write a TypeScript client library that wraps L4 API calls with type safety.

### Exercise 3: Database Bridge
Create a Python script that syncs a PostgreSQL database with L4 evaluation results.

### Exercise 4: Batch Processor
Implement a batch processor that handles 1000+ records efficiently, with error handling and retry logic.

## Next Steps

In **Module A7**, we'll cover regression testing strategies to ensure that schema changes and rule updates don't break existing integrations.
