# Specification: Batch Processing for L4 Programs

## Executive Summary

This document specifies a batch processing capability for L4 programs that allows users to process multiple sets of input parameters from JSON, YAML, or CSV files. The feature extends `jl4-cli` to:

1. Accept a batch input file containing multiple parameter sets
2. Identify and validate against an L4 entrypoint function (marked with `@export` or `@desc export`)
3. Process inputs in batch mode, calling the entrypoint repeatedly
4. Output results in the same format as input (or a specified alternative format)
5. Report validation errors for malformed inputs

## Current Implementation Status

**Issue**: TBD
**Branch**: `mengwong/json-grinder` (may be renamed to `json-yaml-csv-grinder`)
**Related PR**: #682 (Enhanced `@desc` annotation support for leading/inline parameter descriptions)

### What Already Exists

| Component                       | Status | Location                                     |
| ------------------------------- | ------ | -------------------------------------------- |
| `jl4-cli` tool                  | ✅     | `jl4/app/Main.hs`                            |
| `@export` annotation parsing    | ✅     | `jl4-core/src/L4/Export.hs`                  |
| `@desc` parameter annotations   | ✅     | PR #682 (leading + inline styles)            |
| Schema extraction utilities     | ✅     | `L4.Export.extractParams`, `ExportedParam`   |
| Entrypoint extraction utilities | ✅     | `getExportedFunctions`, `getDefaultFunction` |
| Decision service integration    | ✅     | `jl4-decision-service/src/`                  |
| JSONDECODE built-in             | ✅     | `jl4/examples/ok/jsondecode.l4`              |

### What Needs to Be Built

- [ ] Batch input file parser (JSON, YAML, CSV) -- extend L4 to handle YAMLDECODE and CSVDECODE
- [ ] Input schema validation using `ExportedParam` metadata
- [ ] Type matching/coercion logic (leverage existing type system)
- [ ] Batch execution engine (iterate over inputs, - [ ] Multi-format output writer (JSON, YAML, CSV)
- [ ] Multi-format output writer (JSON, YAML, CSV)
- [ ] Error reporting for validation failures (use `paramDescription` for context)
- [ ] CLI integration and command-line options

---

## Motivation

### Use Cases

1. **Automated testing**: Run compliance checks on hundreds of test cases
2. **Data processing**: Apply legal rules to datasets (e.g., eligibility determination for applicants)
3. **Validation**: Batch-validate structured data against legal constraints
4. **Reporting**: Generate compliance reports for multiple scenarios
5. **Integration**: Enable L4 programs to be used in data pipelines

### Example Scenario

A legal team wants to check 1000 insurance applicants for eligibility. Instead of:

- Running `jl4-cli` 1000 times
- Manually invoking the decision service API 1000 times
- Writing custom scripts to loop through data

They can:

```bash
jl4-cli --batch applicants.json eligibility.l4
```

---

## Design

### Command-Line Interface

#### New Options

```bash
jl4-cli [OPTIONS] --batch BATCH_FILE L4FILE
```

**Required flags:**

- `--batch FILE` or `-b FILE`: Path to batch input file (JSON/YAML/CSV)

**Optional flags:**

- `--output FILE` or `-o FILE`: Output file path (default: stdout)
- `--format FORMAT` or `-f FORMAT`: Output format (json|yaml|csv, default: same as input)
- `--entrypoint NAME` or `-e NAME`: Specific function to call (default: find `@export default`)
- `--continue-on-error` or `-c`: Continue processing on validation errors (default: stop on first error)
- `--validate-only`: Only validate inputs without executing
- `--parallel N`: Process N inputs in parallel (default: 1, sequential)

**Existing flags still supported:**

- `--verbose` or `-v`: Show detailed output
- `--fixed-now ISO8601`: Pin evaluation clock

#### Examples

```bash
# Process JSON inputs, output JSON
jl4-cli --batch inputs.json myprogram.l4

# Process CSV inputs, output YAML
jl4-cli --batch inputs.csv --format yaml myprogram.l4

# Use specific entrypoint function
jl4-cli --batch data.json --entrypoint checkEligibility myprogram.l4

# Continue processing even if some inputs fail validation
jl4-cli --batch data.json --continue-on-error myprogram.l4

# Validate inputs without executing
jl4-cli --batch data.json --validate-only myprogram.l4

# Process in parallel
jl4-cli --batch data.json --parallel 4 myprogram.l4
```

---

### Input File Formats

#### JSON Format

**Array of objects:**

```json
[
  {
    "age": 25,
    "income": 50000,
    "hasViolations": false
  },
  {
    "age": 17,
    "income": 30000,
    "hasViolations": true
  }
]
```

**Single object (treated as batch of 1):**

```json
{
  "age": 25,
  "income": 50000,
  "hasViolations": false
}
```

#### YAML Format

**Array of objects:**

```yaml
- age: 25
  income: 50000
  hasViolations: false
- age: 17
  income: 30000
  hasViolations: true
```

**Single object:**

```yaml
age: 25
income: 50000
hasViolations: false
```

#### CSV Format

**Header row required:**

```csv
age,income,hasViolations
25,50000,false
17,30000,true
```

**Type inference rules for CSV:**

- Numbers without decimals → `NUMBER` (Int or Integer)
- Numbers with decimals → `NUMBER` (Double)
- `true`/`false` (case-insensitive) → `BOOLEAN`
- ISO8601 dates → `DATE`
- Everything else → `STRING`

**Special considerations:**

- We support MAYBE types as a special case; when a value is given, it becomes a (JUST x). Empty cells → `NOTHING`.
- Nested objects not supported directly in CSV (use JSON/YAML for complex structures)
- Arrays encoded as JSON strings in cells (e.g., `"[1,2,3]"`)

---

### Entrypoint Discovery

#### Finding the Entrypoint Function

1. **Explicit**: If `--entrypoint NAME` is specified, use that function
2. **Default export**: Look for `@export default` annotation
3. **Single export**: If only one `@export` or `@desc export` exists, use it
4. **Error**: If multiple exports exist and no default, error with list of available functions

#### Example L4 Programs

**Single export (implicit):**

```l4
@export This function checks if a person is eligible
GIVEN age IS A Number @desc The applicant's age
      income IS A Number @desc Annual income in dollars
      hasViolations IS A Boolean @desc Whether they have violations
GIVETH A Boolean
checkEligibility age income hasViolations MEANS
  age >= 18 AND income >= 25000 AND NOT hasViolations
```

**Multiple exports with default:**

```l4
@export default Main eligibility check
GIVEN applicant IS A Person
GIVETH A Boolean
isEligible applicant MEANS ...

@export Detailed eligibility with reasons
GIVEN applicant IS A Person
GIVETH A EligibilityResult
checkEligibilityDetailed applicant MEANS ...
```

---

### Type Validation

#### Schema Extraction

**Use existing `L4.Export` infrastructure** (jl4-core/src/L4/Export.hs:122-136):

For a given entrypoint function, `L4.Export.extractParams` already extracts parameter metadata from `GIVEN` clauses:

```l4
@export default Check eligibility for insurance
GIVEN
  @desc The applicant's age in years
  age IS A NUMBER
  @desc The applicant's full name
  name IS A STRING
  @desc Whether the applicant is currently active
  isActive IS A BOOLEAN
GIVETH A BOOLEAN
checkEligibility age name isActive MEANS ...
```

The `extractParams` function returns `[ExportedParam]` where each parameter contains:

```haskell
data ExportedParam = ExportedParam
  { paramName :: Text             -- "age", "name", "isActive"
  , paramType :: Maybe (Type' Resolved)  -- Number, String, Boolean
  , paramDescription :: Maybe Text       -- From @desc annotations (PR #682)
  , paramRequired :: Bool                -- Currently always True
  }
```

**Key improvements from PR #682:**

- Both **leading** (`@desc` before parameter) and **inline** (`@desc` after type) styles supported
- Parameter descriptions extracted and available for validation error messages
- Inline descriptions take priority when both exist

#### Validation Rules

**For each input record:**

1. **Required fields**: All non-`Maybe` parameters must be present
2. **Type matching**: Values must match expected types
3. **Extra fields**: Warn but allow (ignore unmapped fields)
4. **Missing fields**: Error if required, skip if optional (`Maybe`)

**Type coercion (following L4's existing rules):**

- String → Number (if parseable)
- Number → String (always allowed)
- Number → Boolean (0 = false, non-zero = true)
- See `TYPE-COERCION-AND-TRUNC-SPEC.md` for full rules

#### Error Messages

**Format:**

```
Error in input record #3:
  Missing required field: 'age'
    Description: The applicant's age in years
  Expected type Number for field 'income', got String: "not-a-number"
    Description: Annual income in dollars

Input was:
  {"name": "Alice", "income": "not-a-number"}

Expected schema (from @desc annotations):
  { age :: Number      -- The applicant's age in years
  , name :: String     -- The applicant's full name
  , income :: Number   -- Annual income in dollars
  }
```

**Note**: Parameter descriptions come from `ExportedParam.paramDescription`, populated by PR #682's enhanced `@desc` support.

---

### Batch Execution

#### Processing Model

**Sequential (default):**

```
For each input in batch:
  1. Validate input against schema
  2. If validation fails:
     - Log error
     - If --continue-on-error: continue to next input
     - Else: exit with error
  3. If validation succeeds:
     - Call entrypoint with input parameters
     - Collect result
  4. Append result to output
```

**Parallel (with `--parallel N`):**

- Process up to N inputs concurrently
- Maintain input order in output
- Aggregate validation errors at the end

#### Execution Context

- Each input is independent (no shared state between calls)
- The L4 program is loaded once, inputs are evaluated separately
- Evaluation configuration (e.g., `--fixed-now`) applies to all inputs
- For REPL-style stateful programs, consider sequential processing only

---

### Output Formats

#### Structure

**Success case:**

```json
[
  {
    "input": { "age": 25, "income": 50000, "hasViolations": false },
    "output": true,
    "status": "success"
  },
  {
    "input": { "age": 17, "income": 30000, "hasViolations": true },
    "output": false,
    "status": "success"
  }
]
```

**With errors (when `--continue-on-error` is set):**

```json
[
  {
    "input": { "age": 25, "income": 50000, "hasViolations": false },
    "output": true,
    "status": "success"
  },
  {
    "input": { "name": "Bob" },
    "error": "Missing required field: 'age'",
    "status": "error"
  }
]
```

**Simplified output (if `--simple` flag is added):**

```json
[true, false, true, true, false]
```

#### Format Conversion

**JSON → YAML:**

```yaml
- input:
    age: 25
    income: 50000
    hasViolations: false
  output: true
  status: success
```

**JSON → CSV:**

```csv
input_age,input_income,input_hasViolations,output,status
25,50000,false,true,success
17,30000,true,false,success
```

**CSV → JSON/YAML:**

- Read CSV as records
- Output as structured JSON/YAML

---

## Implementation Plan

### Phase 1: Core Batch Processing

**Goal**: Basic JSON batch processing without validation

**Tasks:**

1. Add command-line option parsing for `--batch`
2. Implement JSON array parser
3. Wire up `getExportedFunctions` to find entrypoint
4. Implement batch loop (sequential)
5. Output JSON results to stdout

**Acceptance criteria:**

- `jl4-cli --batch inputs.json program.l4` works for simple programs
- Output is JSON array of results

### Phase 2: Type Validation

**Goal**: Validate inputs against L4 function signatures

**Tasks:**

1. Use `L4.Export.extractParams` to get parameter schema from entrypoint
2. Map JSON/YAML/CSV field names to `ExportedParam.paramName`
3. Implement type validation logic (reuse existing coercion rules from `L4.TypeCheck`)
4. Generate user-friendly error messages using `paramDescription` for context
5. Add `--validate-only` flag
6. Add `--continue-on-error` flag

**Acceptance criteria:**

- Validation errors include parameter descriptions from `@desc` annotations
- Validation errors are reported clearly with record numbers
- `--validate-only` checks inputs without executing
- `--continue-on-error` collects all errors

### Phase 3: YAML and CSV Support

**Goal**: Support multiple input/output formats

**Tasks:**

1. Add YAML parser (use existing `yaml` library)
2. Add CSV parser (use `cassava` library)
3. Implement CSV type inference
4. Add `--format` flag for output format selection
5. Implement format converters (JSON↔YAML↔CSV)

**Acceptance criteria:**

- `--batch inputs.yaml` works
- `--batch inputs.csv` works
- `--format yaml` converts JSON input to YAML output

### Phase 4: Advanced Features

**Goal**: Parallel processing and output options

**Tasks:**

1. Add `--simple` flag for minimal output
2. Add `--output FILE` for writing results to file
3. Add progress indicator for large batches
4. Support `--trace` to dump the EVALTRACE workings of how an answer was achieved to a companion output file, or inilne alongside the usual responses. For example, a special JSON attribute in each response object could have the name "TRACE" and contain a JSON represenation of the evaluation trace.

**Acceptance criteria:**

- Large batches show progress (e.g., "Processed 500/1000")
- Output file writing works

---

## Testing Strategy

### Unit Tests

- [ ] JSON/YAML/CSV parsing
- [ ] Schema extraction from L4 functions
- [ ] Type validation (valid and invalid inputs)
- [ ] Format conversion (JSON↔YAML↔CSV)
- [ ] Entrypoint discovery (default, explicit, ambiguous)

### Integration Tests

- [ ] End-to-end batch processing with sample L4 programs
- [ ] Error handling (`--continue-on-error`)
- [ ] Output format options

### Golden File Tests

Create golden files for:

- Batch processing outputs for various input formats
- Error messages for validation failures
- Schema extraction for different L4 function signatures

### Test Data

Create sample files:

- `test/batch/simple-inputs.json`
- `test/batch/simple-inputs.yaml`
- `test/batch/simple-inputs.csv`
- `test/batch/invalid-inputs.json` (for error cases)
- `test/batch/nested-inputs.json` (complex types)

Sample L4 programs:

- `test/batch/simple-eligibility.l4` (Boolean output)
- `test/batch/detailed-check.l4` (Record output)
- `test/batch/multi-export.l4` (multiple entrypoints)

---

## Error Handling

### Categories of Errors

1. **File errors**:

   - Batch file not found
   - L4 file not found
   - Unable to parse batch file (malformed JSON/YAML/CSV)

2. **Schema errors**:

   - No entrypoint found
   - Multiple entrypoints without default
   - Ambiguous entrypoint specification

3. **Validation errors**:

   - Missing required field
   - Type mismatch
   - Coercion failure

4. **Execution errors**:
   - Runtime evaluation error in L4 program
   - Timeout (for future extension)

### Error Reporting Format

```
jl4-cli: Batch processing failed

File: inputs.json
L4 Program: eligibility.l4
Entrypoint: checkEligibility

Validation errors:

[Record #3]
  Missing required field: 'age'
  Input: {"name": "Alice", "income": 50000}

[Record #5]
  Type mismatch for field 'income':
    Expected: Number
    Got: String ("not-a-number")
  Input: {"age": 30, "name": "Bob", "income": "not-a-number"}

2 validation errors found out of 100 records.

Use --continue-on-error to process valid records.
```

---

## Open Questions and Future Enhancements

### Questions for Discussion

1. **Streaming vs. in-memory**: Should we support streaming for very large files (millions of records)?
2. **Schema generation**: Should we generate JSON Schema / OpenAPI specs for entrypoints? Or will some other component in the system do this independently?
3. **Type hints**: Should CSV support type hint headers (e.g., `age:number,name:string`)?
4. **Output structure**: Should we support "compact" mode that omits `input` and `status` fields?
5. **Progress callback**: Should we support webhooks or callbacks for long-running batches?

### Future Enhancements

- **Database input/output**: Read from SQL queries, write to databases
- **Incremental processing**: Resume from checkpoint for interrupted batches
- **Filtering**: `--filter` flag to select subset of inputs (e.g., `--filter "age > 18"`)
- **Aggregation**: Compute statistics over batch results (count, sum, average)
- **Distributed processing**: Run batches across multiple machines
- **Watch mode**: Auto-reprocess when input file changes
- **Interactive mode**: Prompt for missing/invalid fields

---

## Dependencies

### New Haskell Libraries

Just as support for JSONENCODE and JSONDECODE built-ins required important a JSON library at the Haskell level, support for YAML and CSV may require:

- `cassava` (CSV parsing)
- `yaml` (YAML parsing) — may already be in deps
- `optparse-applicative` (extended CLI parsing) — already in deps

### Existing Code to Leverage

- `L4.Export.getExportedFunctions` — entrypoint discovery (jl4-core/src/L4/Export.hs:86-96)
- `L4.Export.getDefaultFunction` — default entrypoint (jl4-core/src/L4/Export.hs:98-102)
- `L4.Export.extractParams` — parameter schema extraction with descriptions (jl4-core/src/L4/Export.hs:122-136)
- `L4.Export.ExportedParam` — parameter metadata including `@desc` annotations from PR #682
- `L4.Syntax` — AST and type information
- `L4.TypeCheck` — type coercion and validation rules
- `L4.EvaluateLazy` — evaluation engine
- `jl4-decision-service` — may have JSON encoding/decoding utilities for decision service API

---

## Documentation Updates

After implementation:

1. Update `doc/README.md` with batch processing examples
2. Add `doc/tutorials/BATCH-PROCESSING.md` tutorial
3. Update `jl4-cli --help` output
4. Add batch processing to L4 user guide (if one exists)
5. Update AGENTS.md if batch processing changes development workflow

---

## Metrics and Observability

For production use, consider tracking:

- **Processing rate**: Records per second
- **Success rate**: Percentage of valid inputs
- **Error distribution**: Counts by error type
- **Execution time**: Per-record and total batch time
- **Memory usage**: Peak memory for large batches

**Logging levels:**

- `ERROR`: Fatal errors (file not found, parse failures)
- `WARN`: Validation errors (when `--continue-on-error` is set)
- `INFO`: Progress updates (every N records)
- `DEBUG`: Per-record details (with `--verbose`)

---

## Security Considerations

In a future version, consider:

1. **Resource limits**: Prevent DOS by limiting:

   - Maximum file size (e.g., 100MB)
   - Maximum records per batch (e.g., 100,000)
   - Maximum execution time per record (e.g., 10 seconds)

2. **Input sanitization**: Ensure CSV parsing doesn't execute formulas (e.g., Excel formula injection)

3. **File permissions**: Respect filesystem permissions for input/output files

4. **Error message sanitization**: Don't leak sensitive data in error messages

---

## Backwards Compatibility

- Existing `jl4-cli` behavior must remain unchanged when `--batch` is not specified
- The user can specify either `--ast` or `--batch` but not both.
- Other existing flags (`--verbose`, `--fixed-now`) continue to work
- No breaking changes to L4 syntax or semantics

---

## Acceptance Criteria

**Definition of Done:**

- [ ] All four phases implemented and tested
- [ ] Test suite passes (`cabal test all`)
- [ ] Documentation updated
- [ ] Spec moved to `doc/dev/specs/done/`
- [ ] Branch merged to main

**Minimum Viable Product (MVP):**

- [ ] Phases 1 and 2 complete (JSON batch processing with validation)
- [ ] Basic error handling and reporting
- [ ] At least 3 golden file tests
