# Module A1 — L4 Development Workflow in VSCode

## Overview

In the Foundation Course, you learned L4 basics through single-file examples. Real-world legal systems require **multi-file projects** with:

- Modular organization
- Shared libraries
- Version control
- Automated testing
- IDE support

This module covers professional L4 development workflows using VSCode.

## Project Structure

### Recommended Directory Layout

```
my-l4-project/
├── src/
│   ├── types/
│   │   ├── core-types.l4
│   │   ├── domain-types.l4
│   │   └── result-types.l4
│   ├── rules/
│   │   ├── eligibility.l4
│   │   ├── quotas.l4
│   │   └── risk-assessment.l4
│   ├── helpers/
│   │   ├── date-helpers.l4
│   │   └── list-helpers.l4
│   └── main.l4
├── tests/
│   ├── fixtures/
│   │   ├── test-employees.l4
│   │   └── test-companies.l4
│   └── test-suites/
│       ├── unit-tests.l4
│       └── integration-tests.l4
├── libs/
│   └── custom-prelude.l4
├── docs/
│   ├── architecture.md
│   └── business-rules.md
├── README.md
└── .gitignore
```

## Multi-File Organization

### Splitting by Concern

**core-types.l4**

```l4
§ `Core Type Definitions`

DECLARE EmploymentCategory IS ONE OF
    TechProfessional
    HealthcareWorker
    Researcher
    -- etc.

DECLARE Employee HAS
    name IS A STRING
    -- etc.
```

**domain-rules.l4**

```l4
IMPORT core-types

§ `Domain Business Rules`

GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `meets eligibility` IF
    -- implementation
```

### Import Paths

L4's import system works relative to your file location:

```l4
-- Import from same directory
IMPORT core-types

-- Import from parent directory (not supported yet in JL4)
-- In future: IMPORT ../types/core-types

-- Import standard libraries
IMPORT prelude
IMPORT daydate
```

## VSCode L4 Extension Features

### Installation

1. Build the extension:

```bash
cd ts-apps/vscode
npm install
npm run compile
```

2. Install in VSCode:
   - Open VSCode
   - Press F5 to launch Extension Development Host
   - Or package as VSIX: `npm run package`

### Key Features

#### 1. Syntax Highlighting

The extension provides semantic highlighting for:

- Keywords (`GIVEN`, `DECIDE`, `CONSIDER`)
- Types (`Employee`, `NUMBER`, `STRING`)
- Operators (`AND`, `OR`, `AT LEAST`)
- Comments and strings
- Section headers

#### 2. Type Checking on Save

As you type, the extension:

- Runs the L4 compiler in the background
- Shows type errors inline
- Highlights undefined references
- Catches pattern match exhaustiveness issues

#### 3. Inline Evaluation

Hover over `#EVAL` directives to see:

- Computed results
- Type information
- Execution traces

Or use CodeLens to execute evaluations inline.

#### 4. Decision Logic Visualization

For `DECIDE` rules, the extension can generate:

- Ladder diagrams (Boolean circuit visualization)
- Decision trees
- Flowcharts

Right-click on a DECIDE statement → "Visualize Decision Logic"

#### 5. Go to Definition

Cmd/Ctrl+Click on any identifier to jump to its definition, even across files.

#### 6. Find References

Right-click → "Find All References" to see where a function or type is used.

#### 7. Rename Refactoring

Select identifier → F2 → type new name → renames across all files

## Command Line Workflow

### Building and Running

```bash
# Build all L4 files
cabal build all

# Run specific file
cabal run jl4-cli -- src/main.l4

# Run with verbose output
cabal run jl4-cli -- --verbose src/main.l4

# Type check without evaluation
cabal run jl4-cli -- --check-only src/main.l4
```

### Watching for Changes

Use a file watcher to re-run tests on save:

```bash
# Install fswatch (macOS)
brew install fswatch

# Watch and re-run
fswatch -o src/**/*.l4 | xargs -n1 -I{} cabal run jl4-cli -- src/main.l4
```

Or use VSCode's built-in tasks:

**.vscode/tasks.json**

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Run L4 Tests",
      "type": "shell",
      "command": "cabal run jl4-cli -- tests/test-suites/unit-tests.l4",
      "group": {
        "kind": "test",
        "isDefault": true
      },
      "presentation": {
        "reveal": "always",
        "panel": "new"
      }
    }
  ]
}
```

Now: Cmd/Ctrl+Shift+B to run tests.

## Testing Workflow

### Test File Organization

**tests/fixtures/employees.l4**

```l4
§ `Test Fixtures - Employees`

`Alice - Senior Tech` MEANS Employee WITH
    -- fields...

`Bob - Junior Tech` MEANS Employee WITH
    -- fields...
```

**tests/unit-tests.l4**

```l4
IMPORT ../src/types/core-types
IMPORT ../src/rules/eligibility
IMPORT fixtures/employees

§ `Unit Tests - Eligibility Rules`

#ASSERT `meets age requirement` `Alice - Senior Tech`
#ASSERT NOT `meets age requirement` `Charlie - Underage`
```

### Running Tests

```bash
# Run all tests
cabal run jl4-cli -- tests/test-suites/unit-tests.l4

# Run specific test file
cabal run jl4-cli -- tests/test-suites/eligibility-tests.l4
```

### Continuous Testing

Use a test runner script:

**scripts/run-tests.sh**

```bash
#!/bin/bash
set -e

echo "Running unit tests..."
cabal run jl4-cli -- tests/test-suites/unit-tests.l4

echo "Running integration tests..."
cabal run jl4-cli -- tests/test-suites/integration-tests.l4

echo "Running end-to-end tests..."
cabal run jl4-cli -- tests/test-suites/e2e-tests.l4

echo "All tests passed! ✓"
```

```bash
chmod +x scripts/run-tests.sh
./scripts/run-tests.sh
```

## Git Integration

### .gitignore for L4 Projects

```gitignore
# Haskell build artifacts
dist-newstyle/
.stack-work/
*.hi
*.o

# L4 compiler outputs
*.md.out
*.json.out

# IDE files
.vscode/
.idea/
*.swp

# OS files
.DS_Store
Thumbs.db

# Test outputs
test-results/
*.log
```

### Commit Workflow

```bash
# Stage L4 source files
git add src/

# Commit with descriptive message
git commit -m "feat: Add quota checking rules for healthcare category"

# Run tests before pushing
./scripts/run-tests.sh

# Push
git push origin feature/healthcare-quota
```

### Pre-commit Hooks

**.git/hooks/pre-commit**

```bash
#!/bin/bash

echo "Running L4 type checks..."
cabal run jl4-cli -- --check-only src/main.l4

if [ $? -ne 0 ]; then
    echo "Type check failed. Commit aborted."
    exit 1
fi

echo "Running tests..."
./scripts/run-tests.sh

if [ $? -ne 0 ]; then
    echo "Tests failed. Commit aborted."
    exit 1
fi

exit 0
```

```bash
chmod +x .git/hooks/pre-commit
```

Now tests run automatically before each commit.

## Navigation Best Practices

### File Naming Conventions

- Use kebab-case: `employee-eligibility.l4`
- Group by feature: `quota-management.l4`
- Suffix test files: `eligibility-tests.l4`
- Use descriptive names: `healthcare-worker-rules.l4`

### Code Organization Within Files

```l4
-- 1. Imports at top
IMPORT prelude
IMPORT daydate
IMPORT ../types/core-types

-- 2. Section for public API
§ `Public Functions`

-- Most important functions first

-- 3. Section for helpers
§§ `Helper Functions`

-- Internal functions

-- 4. Tests at bottom (if inline testing)
§§ `Tests`

#EVAL ...
#ASSERT ...
```

### Cross-File Navigation Tips

1. **Use meaningful section headers** for quick scanning
2. **Keep related logic together** in same file
3. **Limit file size** to ~300-500 lines max
4. **Document inter-file dependencies** at top of each file

## Debugging Workflow

### 1. Compiler Errors

When you get a type error:

```
Type mismatch:
  Expected: NUMBER
  Got: STRING
  In: employee's age
```

**Fix:**

1. Check the field type in the DECLARE
2. Verify you're accessing the right field
3. Check if you need a type conversion

### 2. Runtime Errors (Pattern Match Failures)

```
Non-exhaustive pattern match in CONSIDER expression
Missing case: Researcher
```

**Fix:**

1. Add the missing case
2. Or add `OTHERWISE` clause

### 3. Logic Errors (Wrong Result)

When output is wrong but no errors:

**Strategy:**

1. Add `#EVAL` directives at intermediate steps
2. Check helper function outputs
3. Use `WHERE` clause values as checkpoints
4. Verify test fixtures have correct data

### 4. Performance Issues

If evaluation is slow:

1. Check for infinite recursion
2. Look for redundant computations
3. Consider memoizing expensive calculations
4. Profile with `--verbose` flag

## Keyboard Shortcuts (VSCode)

| Action           | macOS       | Windows/Linux |
| ---------------- | ----------- | ------------- |
| Go to Definition | Cmd+Click   | Ctrl+Click    |
| Find References  | Shift+F12   | Shift+F12     |
| Rename Symbol    | F2          | F2            |
| Format Document  | Shift+Opt+F | Shift+Alt+F   |
| Run Tests        | Cmd+Shift+T | Ctrl+Shift+T  |
| Toggle Terminal  | Ctrl+`      | Ctrl+`        |
| Command Palette  | Cmd+Shift+P | Ctrl+Shift+P  |

## Documentation Generation

L4 files can generate documentation automatically:

```bash
# Generate Markdown docs
cabal run jl4-cli -- --export-markdown src/main.l4 > docs/rules.md

# Generate JSON schema
cabal run jl4-cli -- --export-json-schema src/types/core-types.l4 > docs/schema.json
```

Include doc comments in your L4:

```l4
-- | Checks if an employee meets the minimum age requirement
-- of 18 years as mandated by the Employment Act
GIVEN employee IS AN Employee
GIVETH A BOOLEAN
DECIDE `meets minimum age` IF
    `age of` employee `as of` `today` AT LEAST 18
```

## Key Takeaways

1. **Organize by concern**—types, rules, helpers, tests in separate files
2. **Use VSCode extension** for syntax highlighting, type checking, visualization
3. **Set up automated testing** with scripts and pre-commit hooks
4. **Version control everything** except build artifacts
5. **Navigate efficiently** with Go to Definition and Find References
6. **Debug systematically** with #EVAL at intermediate steps
7. **Document as you go** with comments and section headers
8. **Keep files focused** (300-500 lines max)

## Exercises

### Exercise 1: Project Setup

Create a new L4 project with the recommended directory structure. Set up Git and add a .gitignore.

### Exercise 2: Multi-File Refactoring

Take your WPA pipeline from Module 5 and split it across multiple files (types, rules, helpers, tests).

### Exercise 3: Testing Infrastructure

Create a test runner script that runs all test files in your tests/ directory.

### Exercise 4: Pre-Commit Hook

Set up a pre-commit hook that runs type checking before allowing commits.

## Next Steps

In **Module A2**, we'll use AI to assist with ingesting real legislation and extracting formal rules from legal text.
