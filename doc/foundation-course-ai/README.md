# L4 Foundation Course

Welcome to the L4 Foundation Course! This course teaches you how to write legal and regulatory rules as executable code using the L4 programming language.

## Who This Course Is For

- Legal professionals interested in rules-as-code
- Software developers moving into legal tech
- Policy makers exploring computational law
- Anyone who wants to make legal rules computable and testable

## Prerequisites

- Basic programming experience (JavaScript, Python, or similar)
- Familiarity with command line basics
- No functional programming experience required

## Course Structure

### [Quickstart](quickstart.md)
Jump right in with a complete example showing L4's most common patterns in action.

### [Module 0 — Orientation & File Anatomy](module-0-orientation.md)
What L4 is, why it exists, and how an L4 file is structured.

### [Module 1 — Enums & Records Using DECLARE](module-1-enums-records.md)
Defining enums, records, lists, and optional types in the WorkPass Authority domain.

### [Module 2 — Functions Using GIVEN / GIVETH / MEANS](module-2-functions.md)
Reusable logic, date arithmetic, Booleans, and numeric calculations with natural language syntax.

### [Module 3 — Control Flow: CONSIDER, WHEN, BRANCH](module-3-control-flow.md)
Decision tables, branching logic, and deterministic modeling patterns with pattern matching.

### [Module 4 — Lists, MAYBE, and Higher-Order Helpers](module-4-lists-maybe.md)
Working with lists, MAYBE values, map, filter, any, JUST, NOTHING.

### [Module 5 — WPA Eligibility Pipeline](module-5-wpa-pipeline.md)
Building a multi-step assessment rule using helper functions and WHERE clauses.

### [Module 6 — Test Data & #EVAL Execution](module-6-test-data-eval.md)
Defining reusable fixtures and running evaluations on WPA employees and companies.

## Learning Path

**Recommended sequence:**
1. Start with the **Quickstart** to see everything working together
2. Read **Module 0** for context and motivation
3. Work through **Modules 1-6** in order
4. Complete exercises in each module
5. Build your own domain model for a real legal problem

**Fast track (for experienced programmers):**
1. **Quickstart** → **Module 1** → **Module 5** → **Module 6**
2. Reference other modules as needed

## Running L4 Code

### Installation

See the main [README](../README.md) for installation instructions.

### Quick Test

```bash
# Build the project
cabal build all

# Run an example
cabal run jl4-cli -- jl4/examples/ok/factorial.l4

# Run your own file
cabal run jl4-cli -- your-file.l4
```

### VS Code Extension

For the best development experience, install the L4 VS Code extension:
- Syntax highlighting
- Type checking
- Inline evaluation
- Decision logic visualization

See [Dev.md](../Dev.md) for setup instructions.

## Domain Example: WorkPass Authority

Throughout this course, we use a fictional government agency called the **WorkPass Authority (WPA)** that processes work permit applications for foreign employees.

This domain demonstrates:
- Eligibility rules (age, education, experience)
- Business constraints (quotas, minimums)
- Multi-step decision processes
- Risk assessment
- Document requirements

The WPA domain is realistic enough to illustrate real-world legal computation while being simple enough to learn quickly.

## After This Course

Upon completion, you'll be able to:
- ✅ Model legal domains with types
- ✅ Write decision logic as functions
- ✅ Use pattern matching for control flow
- ✅ Handle collections and optional values
- ✅ Build multi-stage assessment pipelines
- ✅ Create comprehensive test suites

## Next Steps: Advanced Course

Ready for production-grade legal systems? The [Advanced Course](../advanced-course-ai/README.md) covers:

**Development & Workflows:**
- **Module A1:** Professional development workflow with VSCode and Git
- **Module A2:** AI-assisted ingestion from legal documents
- **Module A7:** Regression testing and change control
- **Module A8:** Multi-file architecture for large projects

**Advanced Language Features:**
- **Module A3:** Temporal logic for multi-temporal reasoning [WIP]
- **Module A5:** Rebuttable presumptions with TYPICALLY keyword [WIP]

**Integration & Deployment:**
- **Module A4:** Decision service APIs with @desc export
- **Module A6:** JSON integration with external systems (includes Swagger endpoints)

**Migration & Legacy Systems:**
- **Module A9:** Importing Oracle Policy Modeling (OPM/OIA) files

See the [Advanced Course README](../advanced-course-ai/README.md) for detailed learning paths and capstone project.

---

**🎓 Learning Paths:**
- Full Stack Legal Engineer (all modules, 4-6 weeks)
- API Integration Specialist (A1, A4, A6, 1-2 weeks)
- Legal Rule Migration Expert (A1, A2, A9, 2-3 weeks)
- Quality Assurance Specialist (A1, A6, A7, 2 weeks)
- Enterprise Architect (A1, A6, A7, A8, 3-4 weeks)

## Getting Help

- Documentation bugs? Open an issue on GitHub
- Questions about L4? Join the community discussions
- Need examples? Check `jl4/examples/` directory

## Contributing

Found a typo or want to improve these materials? Contributions welcome!

## License

These educational materials are part of the L4 IDE project. See the main LICENSE file for details.

---

**Let's make law computable!** 🚀
