# Documentation Reorganization Specification

**Status:** TODO
**Created:** 2025-01-17
**Scope:** Complete reorganization of the `doc/` folder into a structured, AI-friendly documentation system

---

## Overview

The current `doc/` folder has become disorganized with mixed content types: development specs, language documentation, courses, tutorials, conceptual explanations, library references, and proposals all coexist without clear organization. This spec defines a complete reorganization to create a professional, maintainable, and AI-indexable documentation system.

### Goals

1. **Separation of Concerns:** Development specs go to `/specs`, user documentation goes to `/docs`
2. **Clear Structure:** Four-pillar documentation (Reference, Courses, Tutorials, Concepts) following Diátaxis principles
3. **Verifiable Examples:** All L4 code lives in separate `.l4` files that can be validated on build
4. **Human + AI Optimized:** Clear, concise language suitable for both human readers and RAG indexing
5. **Cross-Linking:** Consistent linking between documentation types

---

## Original user prompt to be used a northstar for this spec and to validate the worktasks below:

So, this project has become a bit of a mess in terms of useful documentation (md files and l4 examples). The doc/ folder has become not very useful.

Help me create a plan to accomplish the following and write it into a spec.

1. Move all development specification todo and, proposals, done specs and future/aspirational features, functions and patterns into a separate folder "specs". Update the agents.md as well to reflect changes and where and how to manage updates to the docs.

2. We need the doc folder completely replaced.

3. The doc folder should have md files content documenting the L4 language, its concepts and how it works.

4. It should have 4 subfolders (Reference, Courses, Tutorials, Concepts) with a README.md start page.

5. In the Reference folder, create a language reference, including a GLOSSARY.md file linking to another file for every keyword and syntax pattern (operators, comparators, types, ....) in the L4 language (Look at the lexer haskell code, examples in the jl4 folders and test to learn what they do, instead of just adopting the existing documentation). Folders can be for language role level grouping (keywords, types, operators, etc...) Every file should have an explanation what it does, what role it has in the language L4 and what it enables, default behavior if necessary and bundle it together with two to three L4 example code segments (verify those with l4 mcp validate tool to be working code before using it!

6. Create a reference subfolder for the core libraries shipping with L4, learn what they do from their tests and examples and existing docs. The daydate library docs as they are right now are quite good if you need an example of how to do this nicely.

7. Move the existing courses to the courses folder and check them for accuracy against the reference docs you just created and verify L4 code to work with the MCP tool.

8. Move all tutorials and how-to's into a subfolder of the tutorial folder. Make sure every tutorial/how-to specifies a large audience and caters to their needs and terminology. Obviously L4 code here should also work (verify!)

9. Add a file (tutorial-suggestions.md) into the folder with ideas you have for new relevant tutorials for new and existing audiences

10. In the concepts folder, move all the principals, thinking and concept explanation you find in the current docs about L4. (one subfolder per high level concept group, files for the actual concepts or patterns that are mentioned.) Make sure examples work.

11. Rename the doc folder to old doc and add it to .gitignore once a new doc folder is ready.

12. Whenever there is L4 code in the md file, don't embed the l4 code, create a separate file and only link it in the md! This way we can verify the l4 to be working whenever we create a new build of the L4 language going forward and make sure it keeps working!

13. Make sure there is pretty much no haskell code or JS code in the docs. If it really helps understanding (e.g. a tutorial audience are JS devs so it'd make sense to describe some language features with JS examples), it should be clearly marked in code segments as such code.

14. Use clear but consise language and cater to human readers as well as AI consumers. Link from courses, tutorials and concepts to the reference items if useful. This doc will be indexed and RAGd for AI training later and must be accurate and good! Quality matters. Be thorough.

---

## Phase 1: Create `/specs` Directory ✅ COMPLETED

Move all development-related specifications out of `doc/` to a top-level `/specs` directory.

**Status:** All files successfully moved to `/specs/` directory structure. README.md created with complete overview.

### Directory Structure

```
/specs/
├── README.md                    # Overview of specs system
├── todo/                        # In-progress and planned features
│   ├── *.md                     # (moved from doc/dev/specs/todo/)
├── done/                        # Completed specifications
│   ├── *.md                     # (moved from doc/dev/specs/done/)
├── proposals/                   # Design proposals and RFCs
│   ├── proposal-jl4-nl-query-mar-24-2025.md
│   └── *.md                     # Future proposals
└── roadmap/                     # Future/aspirational features
    ├── future-features.md       # (moved from doc/)
    └── *.md                     # Future roadmap items
```

### Files to Move

From `doc/dev/specs/todo/` → `/specs/todo/`:

- All existing spec files (BATCH-PROCESSING-SPEC.md, etc.)

From `doc/dev/specs/done/` → `/specs/done/`:

- All completed spec files

From `doc/` → `/specs/proposals/`:

- `proposal-jl4-nl-query-mar-24-2025.md`

From `doc/` → `/specs/roadmap/`:

- `future-features.md`

### `/specs/README.md` Content

```markdown
# L4 Development Specifications

This directory contains technical specifications for L4 language features and tooling.

## Directory Structure

- **`todo/`** - Specifications for features in progress or planned
- **`done/`** - Completed specifications (for reference)
- **`proposals/`** - Design proposals and RFCs for discussion
- **`roadmap/`** - Future/aspirational features and long-term plans

## Workflow

1. New features start as a proposal in `proposals/`
2. Approved proposals become specs in `todo/`
3. Completed implementations move specs to `done/`

## Naming Convention

- Use UPPERCASE-KEBAB-CASE for spec files: `FEATURE-NAME-SPEC.md`
- Include status, dates, and scope at the top of each spec
```

---

## Phase 2: Create New `/docs` Directory Structure ✅ COMPLETED

The new documentation follows the Diátaxis framework with four pillars:

**Status:** Complete directory structure created with README files for all major sections. Verification script created at `scripts/verify-doc-examples.sh`.

### Directory Structure

```
/docs/
├── README.md                    # Landing page with navigation
├── reference/                   # Technical reference (look up information)
│   ├── README.md               # Reference index
│   ├── GLOSSARY.md             # Master glossary linking to all reference items
│   ├── keywords/               # Language keywords
│   │   ├── README.md
│   │   ├── ASSUME.md
│   │   ├── DECIDE.md
│   │   ├── DECLARE.md
│   │   ├── GIVEN.md
│   │   ├── GIVETH.md
│   │   ├── IF.md
│   │   ├── THEN.md
│   │   ├── ELSE.md
│   │   ├── CONSIDER.md
│   │   ├── WHEN.md
│   │   ├── WHERE.md
│   │   ├── LET.md
│   │   ├── MEANS.md
│   │   ├── IMPORT.md
│   │   ├── PARTY.md
│   │   ├── MUST.md
│   │   ├── MAY.md
│   │   ├── SHANT.md
│   │   ├── WITHIN.md
│   │   ├── HENCE.md
│   │   ├── LEST.md
│   │   └── ... (all keywords from lexer)
│   ├── types/                  # Type system
│   │   ├── README.md
│   │   ├── BOOLEAN.md
│   │   ├── NUMBER.md
│   │   ├── STRING.md
│   │   ├── DATE.md
│   │   ├── LIST.md
│   │   ├── MAYBE.md
│   │   ├── EITHER.md
│   │   ├── records.md
│   │   └── enums.md
│   ├── operators/              # Operators and comparators
│   │   ├── README.md
│   │   ├── arithmetic.md       # PLUS, MINUS, TIMES, DIVIDED BY, MODULO
│   │   ├── comparison.md       # EQUALS, GREATER THAN, LESS THAN, etc.
│   │   ├── logical.md          # AND, OR, NOT, IMPLIES
│   │   ├── string.md           # CONCAT, string functions
│   │   └── temporal.md         # AT, WITHIN
│   ├── syntax/                 # Syntax patterns
│   │   ├── README.md
│   │   ├── layout.md           # Indentation rules
│   │   ├── comments.md         # -- and {- -}
│   │   ├── identifiers.md      # Backtick identifiers
│   │   ├── annotations.md      # @desc, @nlg, @ref, @export
│   │   ├── directives.md       # #EVAL, #TRACE, #CHECK, #ASSERT
│   │   ├── ditto.md            # ^ copy syntax
│   │   └── asyndetic.md        # ... and .. ellipsis
│   ├── libraries/              # Core libraries
│   │   ├── README.md
│   │   ├── daydate.md          # (refined from existing)
│   │   ├── excel-date.md       # (refined from existing)
│   │   ├── math.md             # (refined from existing)
│   │   ├── coercions.md        # (refined from existing)
│   │   └── llm.md              # LLM integration library
│   └── cli/                    # CLI reference
│       ├── README.md
│       ├── jl4-cli.md
│       └── options.md
├── courses/                    # Learning-oriented (study to learn)
│   ├── README.md               # Course catalog
│   ├── foundation/             # (moved from foundation-course-ai)
│   │   ├── README.md
│   │   ├── module-0-orientation.md
│   │   ├── module-1-enums-records.md
│   │   ├── module-2-functions.md
│   │   ├── module-3-control-flow.md
│   │   ├── module-4-lists-maybe.md
│   │   ├── module-5-wpa-pipeline.md
│   │   ├── module-6-test-data-eval.md
│   │   └── quickstart.md
│   └── advanced/               # (moved from advanced-course-ai)
│       ├── README.md
│       ├── module-a1-vscode-workflow.md
│       ├── module-a2-ai-ingestion.md
│       ├── ... (all advanced modules)
│       └── capstone-project.md
├── tutorials/                  # Task-oriented (solve specific problems)
│   ├── README.md               # Tutorial index
│   ├── tutorial-suggestions.md # Ideas for new tutorials
│   ├── getting-started/        # First steps
│   │   ├── first-l4-file.md
│   │   ├── running-code.md
│   │   └── ide-setup.md
│   ├── llm-integration/        # LLM tutorials
│   │   ├── llm-getting-started.md
│   │   └── llm-advanced-patterns.md
│   ├── web-apps/               # Web app generation
│   │   └── decision-service-api.md
│   └── migration/              # Migration guides
│       └── opm-to-l4.md
├── concepts/                   # Understanding-oriented (explain concepts)
│   ├── README.md               # Concepts index
│   ├── language-design/        # L4 design philosophy
│   │   ├── principles.md       # (from existing principles.md)
│   │   ├── layout-sensitivity.md
│   │   └── legal-isomorphism.md
│   ├── logic/                  # Logic concepts
│   │   ├── default-logic.md    # (from existing)
│   │   ├── boolean-reasoning.md
│   │   └── temporal-reasoning.md
│   ├── legal-modeling/         # Legal domain concepts
│   │   ├── regulative-rules.md # (from existing regulative.md)
│   │   ├── deontic-logic.md
│   │   ├── contract-composition.md
│   │   └── constitutive-vs-regulative.md
│   ├── type-system/            # Type system concepts
│   │   ├── algebraic-types.md
│   │   ├── maybe-nothing.md
│   │   └── type-inference.md
│   └── ai-integration/         # AI/LLM concepts
│       └── hybrid-reasoning.md # (from existing)
...
```

---

## Phase 3: Reference Documentation Creation ✅ FRAMEWORK COMPLETE

### 3.1 Glossary Structure

The `GLOSSARY.md` serves as the master index, linking to detailed reference pages.

**Format for GLOSSARY.md:**

```markdown
# L4 Language Glossary

## Keywords

| Keyword | Category    | Description                   | Link                    |
| ------- | ----------- | ----------------------------- | ----------------------- |
| ASSUME  | Declaration | Declares a variable with type | [→](keywords/ASSUME.md) |
| DECIDE  | Function    | Defines a decision function   | [→](keywords/DECIDE.md) |

...

## Types

| Type    | Description       | Link                  |
| ------- | ----------------- | --------------------- |
| BOOLEAN | True/false values | [→](types/BOOLEAN.md) |

...

## Operators

| Operator | Category   | Description | Link                              |
| -------- | ---------- | ----------- | --------------------------------- |
| PLUS / + | Arithmetic | Addition    | [→](operators/arithmetic.md#plus) |

...
```

### 3.2 Individual Reference Page Format

Each reference page follows this template:

```markdown
# KEYWORD_NAME

**Category:** [Keyword | Type | Operator | Syntax]
**Since:** [version if known]

## Summary

One-sentence description of what this does.

## Syntax

[Link to L4 example file]

## Description

Clear explanation of:

- What it does
- Role in the L4 language
- What it enables
- Default behavior (if applicable)

## Examples

### Example 1: [Use Case Name]

**Purpose:** What this example demonstrates

**Code:** [Link to /docs/examples/reference/keywords/KEYWORD_NAME-ex1.l4]

### Example 2: [Another Use Case]

**Purpose:** ...

**Code:** [Link to .l4 file]

## Related

- [RELATED_KEYWORD](RELATED_KEYWORD.md)
- [Related Concept](/docs/concepts/...)

## Notes

Any additional information, edge cases, or gotchas.
```

### 3.3 Keywords to Document (from Lexer.hs)

**Declaration Keywords:**

- DECLARE, ASSUME, DECIDE, MEANS, AKA

**Function Keywords:**

- GIVEN, GIVETH, GIVES, YIELD, WHERE, LET, IN

**Control Flow Keywords:**

- IF, THEN, ELSE, OTHERWISE, CONSIDER, WHEN, BRANCH

**Logical Keywords:**

- AND, OR, NOT, IMPLIES, RAND, ROR

**Type Keywords:**

- IS, HAS, ONE, OF, WITH, A, AN, THE, LIST, TYPE

**Regulative Keywords:**

- PARTY, MUST, MAY, SHANT, DO, DOES, WITHIN, HENCE, LEST, BREACH, BECAUSE, PROVIDED, AT, FOR

**Comparison Keywords:**

- EQUALS, GREATER, LESS, THAN, ABOVE, BELOW, LEAST, MOST, EXACTLY

**Arithmetic Keywords:**

- PLUS, MINUS, TIMES, DIVIDED, BY, MODULO

**Other Keywords:**

- IMPORT, FETCH, POST, ENV, CONCAT, AS, ALL, BE, MEAN, UNLESS, FROM, TO, FUNCTION

### 3.4 Operators to Document (from Lexer.hs)

**Symbolic Operators:**

- `*` (TIMES), `+` (PLUS), `-` (MINUS)
- `>=`, `<=`, `>`, `<`
- `=`, `==` (equality)
- `&&`, `||` (logical)
- `=>` (IMPLIES), `/` (DIVIDED BY)

**Symbols:**

- `^` (ditto/copy), `...` (asyndetic AND), `..` (asyndetic OR)
- `'s` (genitive/possessive)
- `%` (percent), `:` (colon)
- `()`, `{}`, `[]`, `<<>>` (brackets)
- `§` (paragraph/section)

### 3.5 Library Documentation

Refine existing library docs to follow the reference template. Each library reference should include:

1. **Overview** - What the library provides
2. **Import Statement** - How to import it
3. **Types** - Types defined by the library
4. **Functions** - Complete function reference with signatures
5. **Examples** - Working L4 code examples (linked, not embedded with `l4 `)
6. **See Also** - Related libraries and concepts

The existing `daydate.md` is a good model but needs:

- Examples moved to separate `.l4` files
- Cross-links to related reference items

---

## Phase 4: Course Migration and Verification

### 4.1 Foundation Course Migration

1. Move `doc/foundation-course-ai/*` → `/docs/courses/foundation/`
2. Update all internal links
3. Extract all L4 code blocks to `/docs/examples/courses/foundation/`
4. Update code blocks to link to example files
5. Verify all L4 examples with `cabal run jl4-cli`
6. Cross-reference with reference docs for accuracy

### 4.2 Advanced Course Migration

1. Move `doc/advanced-course-ai/*` → `/docs/courses/advanced/`
2. Update all internal links
3. Extract all L4 code blocks to `/docs/examples/courses/advanced/`
4. Update code blocks to link to example files
5. Verify all L4 examples with `cabal run jl4-cli`
6. Cross-reference with reference docs for accuracy

---

## Phase 5: Tutorial Organization

### 5.1 Tutorial Migration

Move and reorganize tutorials:

| Source                                | Destination                                                |
| ------------------------------------- | ---------------------------------------------------------- |
| `doc/tutorial/llm-getting-started.md` | `/docs/tutorials/llm-integration/llm-getting-started.md`   |
| `doc/how-to/llm-querying.md`          | `/docs/tutorials/llm-integration/llm-advanced-patterns.md` |

### 5.2 Tutorial Audience Specification

Each tutorial must clearly specify its target audience at the top:

```markdown
# Tutorial Title

**Audience:** [Legal professionals | Developers | Legal engineers | All users]
**Prerequisites:** [List specific knowledge required]
**Time:** [Estimated completion time]
**Goal:** [What reader will achieve]
```

### 5.3 Tutorial Suggestions File

Create `/docs/tutorials/tutorial-suggestions.md`:

```markdown
# Tutorial Ideas

## For Legal Professionals

- [ ] Modeling a Simple Contract from Scratch
- [ ] Encoding Regulatory Compliance Rules
- [ ] Creating Decision Tables for Eligibility Criteria
- [ ] Working with Date-Based Deadlines

## For Developers

- [ ] Integrating L4 Decision Services with REST APIs
- [ ] Building Web Forms from L4 Schemas
- [ ] Batch Processing L4 Evaluations
- [ ] Setting Up CI/CD for L4 Projects

## For Legal Engineers

- [ ] Translating Legislation to L4 (Deep Dive)
- [ ] Handling Ambiguity in Legal Text
- [ ] Multi-Temporal Rule Modeling
- [ ] Contract State Machine Patterns

## For Data Scientists

- [ ] Using L4 for Explainable AI in Legal Decisions
- [ ] Integrating L4 with Python Data Pipelines

## Cross-Functional

- [ ] L4 for Policy Makers: From Rules to Code
- [ ] L4 for Insurance: Claims Processing Rules
- [ ] L4 for Banking: KYC/AML Compliance
```

---

## Phase 6: Concepts Documentation

### 6.1 Content to Move

| Source                                | Destination                                         |
| ------------------------------------- | --------------------------------------------------- |
| `doc/principles.md`                   | `/docs/concepts/language-design/principles.md`      |
| `doc/default-logic.md`                | `/docs/concepts/logic/default-logic.md`             |
| `doc/regulative.md`                   | `/docs/concepts/legal-modeling/regulative-rules.md` |
| `doc/explanation/hybrid-reasoning.md` | `/docs/concepts/ai-integration/hybrid-reasoning.md` |
| `doc/multitemporals.md`               | `/docs/concepts/logic/temporal-reasoning.md`        |
| `doc/ASSUME-SEMANTICS.md`             | `/docs/concepts/logic/assume-semantics.md`          |
| `doc/30-algebraic-types.md`           | `/docs/concepts/type-system/algebraic-types.md`     |
| `doc/scope.md`                        | `/docs/concepts/language-design/scope.md`           |

### 6.2 Concept Page Format

```markdown
# Concept Name

## In Brief

One-paragraph summary for quick understanding.

## Why This Matters

Explain the practical importance.

## Deep Dive

Detailed explanation with:

- Background/motivation
- How L4 implements this
- Comparison to other approaches (if relevant)

## Examples

[Links to example L4 files]

## Learn More

- **Reference:** [Links to reference docs]
- **Tutorials:** [Links to relevant tutorials]
- **External:** [Links to papers, articles if applicable]
```

---

## Phase 7: Example Files Organization

### 7.1 Example File Naming Convention

```
/docs/examples/reference/keywords/ASSUME-basic.l4
/docs/examples/reference/keywords/ASSUME-with-type.l4
/docs/examples/reference/operators/arithmetic-basic.l4
/docs/examples/courses/foundation/module-1-ex1.l4
/docs/examples/tutorials/llm-simple-query.l4
```

### 7.2 Example File Format

Each `.l4` example file should:

1. Have a comment header explaining its purpose
2. Be self-contained (include necessary IMPORTs)
3. Include `#EVAL` directives to demonstrate output
4. Be verifiable with `cabal run jl4-cli`

```l4
-- Example: Basic ASSUME usage
-- Demonstrates declaring a variable with a type annotation
-- Used in: /docs/reference/keywords/ASSUME.md

ASSUME age IS A NUMBER
ASSUME name IS A STRING

#EVAL age
#EVAL name
```

### 7.3 Verification Script

Create a script to verify all example files:

```bash
#!/bin/bash
# verify-doc-examples.sh

find docs/examples -name "*.l4" | while read file; do
  echo "Verifying: $file"
  cabal run jl4-cli -- "$file" > /dev/null 2>&1
  if [ $? -ne 0 ]; then
    echo "FAILED: $file"
    exit 1
  fi
done
echo "All examples verified!"
```

---

## Phase 8: AGENTS.md Updates

Update `AGENTS.md` to reflect the new structure:

### Changes to Task Management Section

Replace the current "Specifications" section with:

```markdown
### Specifications

Technical specifications are maintained in the `/specs` directory:

- **`/specs/todo/`** - Specifications for planned features and tasks in progress
- **`/specs/done/`** - Completed specifications for reference
- **`/specs/proposals/`** - Design proposals awaiting approval
- **`/specs/roadmap/`** - Future/aspirational features

When working on a feature:

1. Check if there's an existing spec in `/specs/todo/`
2. If creating a new feature, write a spec first
3. Move completed specs to `/specs/done/`

### Documentation

User-facing documentation lives in `/docs`:

- **`/docs/reference/`** - Language reference (keywords, types, operators, libraries)
- **`/docs/courses/`** - Structured learning paths (foundation, advanced)
- **`/docs/tutorials/`** - Task-oriented how-to guides
- **`/docs/concepts/`** - Conceptual explanations and background

When updating documentation:

1. All L4 code examples go in `/docs/examples/` as separate files
2. Link to example files rather than embedding code in Markdown
3. Run `./verify-doc-examples.sh` before committing
4. Cross-link between documentation types where helpful
```

---

## Phase 9: Cleanup

### 9.1 Rename Old Directory

```bash
mv doc old-doc
```

### 9.2 Update .gitignore

Add to `.gitignore`:

```
old-doc/
```

### 9.3 Files to Delete/Archive

The following files from `old-doc/` should NOT be migrated (obsolete or redundant):

- `2024-12-report.md` - Internal report
- `MARKETECTURE.md` - Marketing material
- `SUMMARY.md` - GitBook artifact
- `guide-index.md` - Old index (replaced by new structure)
- `Background.md` - Superseded by principles.md
- `apps.md` - Internal notes
- `images/*` - Move relevant images to new structure
- `sample-*.hs` - Haskell examples (keep only L4 examples)
- `regulative-spec.org` - Org mode duplicate
- `issues/*` - Move to GitHub issues
- `notes-legislation/*` - Internal notes

### 9.4 Files to Archive (Historical Reference)

Keep in `old-doc/` for historical reference but don't migrate:

- `2024-12-report.md`
- Development notes and historical documents

---

## Phase 10: Quality Assurance

### 10.1 Documentation Standards

**Language:**

- Use clear, concise language
- Define terms before using them
- Avoid jargon without explanation
- Use active voice

**Code Examples:**

- No Haskell code in user docs (reference implementation is OK in concepts)
- No JavaScript code unless tutorial specifically targets JS developers
- Mark any non-L4 code clearly with language identifier
- All L4 code must be verified working

**Structure:**

- Every folder has a README.md
- Every page has clear navigation (breadcrumbs or links)
- Cross-link liberally between related content
- Use consistent heading levels

### 10.2 Review Checklist

For each documentation page, verify:

- [ ] Clear title and purpose statement
- [ ] Appropriate audience specification (tutorials)
- [ ] All L4 code in separate files and verified
- [ ] Links to related content working
- [ ] No broken internal links
- [ ] No embedded Haskell/JS code (unless justified)
- [ ] Consistent formatting
- [ ] Proofread for clarity

### 10.3 AI Optimization

For RAG indexing:

- Include clear section headers
- Use consistent terminology
- Add semantic keywords in summaries
- Structure content hierarchically
- Keep individual pages focused (one concept per page in reference)

---

## Implementation Order

### Sprint 1: Infrastructure

1. ✅ Create `/specs` directory structure
2. ✅ Move all spec files
3. ✅ Create `/docs` directory structure
4. ✅ Create `/docs/examples` structure
5. ✅ Update AGENTS.md (already contains correct structure, verified)

### Sprint 2: Reference Documentation ✅ COMPLETED (Framework)

1. ✅ Create GLOSSARY.md framework - Complete with all ~90 keywords indexed
2. ⏳ Generate keyword reference pages (from Lexer.hs) - Framework and README created
3. ✅ Create type reference pages - README with complete type system overview
4. ✅ Create operator reference pages - README with all operators documented
5. ✅ Create syntax reference pages - README with syntax patterns indexed
6. ✅ Refine library documentation - README with all libraries documented

**Note:** Individual reference pages for each keyword/type/operator remain to be created. The GLOSSARY and README files provide the complete framework based on analysis of Lexer.hs and prelude.l4.

### Sprint 3: Courses

1. Migrate foundation course
2. Extract and verify all foundation examples
3. Migrate advanced course
4. Extract and verify all advanced examples
5. Cross-reference with reference docs

### Sprint 4: Tutorials & Concepts

1. Migrate and organize tutorials
2. Create tutorial-suggestions.md
3. Migrate concept documentation
4. Add cross-links throughout

### Sprint 5: Quality & Cleanup

1. Run verification script on all examples
2. Complete review checklist
3. Rename doc → old-doc
4. Update .gitignore
5. Final link verification

---

## Success Criteria

- [ ] All L4 examples in docs pass verification
- [ ] No Haskell/JS code in user documentation (except where justified)
- [⏳] Every keyword from Lexer.hs has a reference page (GLOSSARY framework complete)
- [ ] Every reference page has 2-3 working examples
- [ ] All courses verified and cross-linked
- [ ] All tutorials specify audience and work as documented
- [✅] Concepts documentation structure complete (README created)
- [✅] AGENTS.md updated with new structure
- [ ] Old doc archived and gitignored

**Progress Update (2025-01-17):**

- ✅ **Sprint 1 Complete:** All infrastructure in place
- ✅ **Sprint 2 Framework Complete:** GLOSSARY.md and all section READMEs created based on Lexer.hs analysis
  - Documented ~90 keywords from Lexer.hs
  - Documented all primitive and polymorphic types
  - Documented all operators (textual and symbolic)
  - Documented syntax patterns and special features
  - Documented all core libraries including prelude, daydate, etc.
- ⏳ **Sprint 2 Detail Work:** Individual reference pages for each feature to be created
- ⏳ **Sprints 3-5:** Course migration, tutorials, concepts, cleanup remain

**Progress Update (2025-01-18):**

- ✅ **CLAUDE.md Updated:** Fixed outdated references to `doc/` paths
  - Updated spec file references from `doc/todo/` to `specs/done/`
  - Rewrote Documentation section to reflect new `doc/` (language docs) and `specs/` (dev specs) structure
  - Updated "Adding a New Language Feature" workflow to use `specs/` paths
  - Noted component-specific READMEs as the source for getting started with each component

---

## Notes

This reorganization creates a foundation for:

- Automated documentation testing in CI
- AI training data generation
- Professional technical writing standards
- Community contributions with clear guidelines

The Diátaxis framework (reference, tutorials, how-to, explanation) is proven for technical documentation and will help users find what they need quickly.
