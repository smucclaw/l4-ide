# Module 0: Introduction to L4

Welcome to L4! This module introduces you to the language and helps you get started.

## What is L4?

**L4 is a programming language designed specifically for law.** It lets you translate legal documents into precise, executable code that computers can understand, verify, and run.

### Why L4?

Traditional legal documents have problems:

| Problem           | Example                          | L4 Solution                     |
| ----------------- | -------------------------------- | ------------------------------- |
| **Ambiguity**     | "reasonable time"                | Explicit deadlines: `WITHIN 30` |
| **Inconsistency** | Same term defined differently    | Single source of truth          |
| **Complexity**    | Nested conditions hard to follow | Visual logic diagrams           |
| **Testing**       | Can't simulate scenarios         | `#TRACE` runs simulations       |
| **Maintenance**   | Changes ripple unpredictably     | Type system catches errors      |

### What L4 Is For

L4 excels at:

- **Regulatory compliance** - Encode legislation as executable rules
- **Contract automation** - Model obligations, deadlines, and consequences
- **Decision support** - Build systems that explain their reasoning
- **Legal analysis** - Find edge cases and contradictions

### What L4 Is Not

L4 is not:

- A replacement for lawyers (it's a tool for lawyers)
- Natural language processing (you write structured code)
- A document generator (though it can feed into one)

---

## How L4 Works

L4 code looks like a hybrid of legal text and programming:

```l4
GIVEN charity IS A RegisteredCharity
IF charity's status EQUALS Active
PARTY charity
MUST `file annual return`
WITHIN 60
HENCE `compliance maintained`
LEST `Commissioner may issue notice`
```

This reads almost like English, but with precise meaning:

- **GIVEN** - Declares what entities are involved
- **IF** - States conditions that must be true
- **PARTY** - Identifies who has the obligation
- **MUST** - Creates a legal obligation
- **WITHIN** - Sets a deadline (in days)
- **HENCE** - What happens on compliance
- **LEST** - What happens on non-compliance

---

## Key Concepts

### 1. Types Define Structure

L4 uses types to define what things look like:

```l4
DECLARE RegisteredCharity
    HAS name IS A STRING
        registrationNumber IS A STRING
        status IS A Status
```

This prevents errors—you can't accidentally use a person where you need a charity.

### 2. Rules Define Logic

Rules capture legal reasoning:

```l4
DECIDE `is eligible` IF
    applicant's age >= 18
    AND applicant's hasValidID EQUALS TRUE
```

### 3. Contracts Define Obligations

Regulative rules capture who must do what:

```l4
PARTY seller
MUST `deliver goods`
WITHIN 14
HENCE PARTY buyer MUST `pay invoice` WITHIN 30
```

---

## Setting Up Your Environment

### Option 1: VS Code (Recommended)

1. Install [VS Code](https://code.visualstudio.com/)
2. Install the L4 extension from the marketplace
3. Create a file ending in `.l4`
4. Start writing!

The extension provides:

- Syntax highlighting
- Error checking as you type
- Hover documentation
- Visualization tools

### Option 2: Web Editor

Visit the [L4 Web Editor](https://l4-ide.smucclaw.dev/) to try L4 in your browser without installing anything.

### Option 3: Command Line

If you have the L4 tools installed:

```bash
# Run a file
cabal run jl4-cli -- myfile.l4

# Interactive REPL
cabal run jl4-repl -- myfile.l4
```

---

## Your First L4 File

Create a file called `hello.l4`:

```l4
-- This is a comment (starts with --)

-- Define a simple type
DECLARE Person
    HAS name IS A STRING
        age IS A NUMBER

-- Create a person
alice MEANS Person "Alice" 30

-- Define a rule
GIVEN p IS A Person
GIVETH A BOOLEAN
DECIDE `is adult` IF p's age >= 18

-- Test it
#EVAL `is adult` alice
```

Save the file. If you're using VS Code with the L4 extension, you'll see:

- Syntax highlighting
- The `#EVAL` result shown inline

---

## L4 Design Philosophy

L4 follows several key principles:

### 1. Layout Sensitivity

Indentation matters (like Python). This makes code visually match its logical structure.

### 2. Legal Isomorphism

L4 code mirrors the structure of legal text. A section in legislation becomes a section in L4.

### 3. Strong Typing

Types catch errors early. If a function expects a `Date`, you can't pass it a `STRING`.

### 4. Functional Style

L4 is based on functional programming. You define _what_ things are, not step-by-step procedures.

---

## What's Next?

In [Module 1: Your First Legal Rule](module-1-first-rule.md), you'll write a complete legal obligation with conditions, deadlines, and consequences.

---

## Quick Reference

| Keyword   | Purpose                       |
| --------- | ----------------------------- |
| `DECLARE` | Define a new type             |
| `DECIDE`  | Define a rule/function        |
| `GIVEN`   | Declare parameters            |
| `GIVETH`  | Declare return type           |
| `MEANS`   | Define what something equals  |
| `IF`      | Add a condition               |
| `PARTY`   | Who has the obligation        |
| `MUST`    | Obligation                    |
| `MAY`     | Permission                    |
| `SHANT`   | Prohibition                   |
| `WITHIN`  | Deadline                      |
| `HENCE`   | Consequence of compliance     |
| `LEST`    | Consequence of non-compliance |
| `#EVAL`   | Evaluate an expression        |
| `#TRACE`  | Simulate a scenario           |
