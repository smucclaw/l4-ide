# Tutorial Ideas and Suggestions

This document tracks ideas for new tutorials to serve different audiences and use cases. Contributions welcome!

## Priority Tutorials

### For Legal Professionals

- [ ] **Modeling a Simple Contract from Scratch**

  - Audience: Lawyers, legal drafters
  - Goal: Translate a real contract into L4
  - Topics: Basic declarations, conditions, obligations
  - Estimated time: 2-3 hours

- [ ] **Encoding Regulatory Compliance Rules**

  - Audience: Compliance officers, legal professionals
  - Goal: Model compliance requirements in L4
  - Topics: Rules, exceptions, reporting requirements
  - Estimated time: 2 hours

- [ ] **Creating Decision Tables for Eligibility Criteria**

  - Audience: Legal professionals, policy makers
  - Goal: Build decision logic for eligibility determination
  - Topics: Boolean logic, decision trees, tabular rules
  - Estimated time: 1.5 hours

- [ ] **Working with Date-Based Deadlines**

  - Audience: Legal professionals
  - Goal: Model time-sensitive obligations
  - Topics: daydate library, temporal logic, WITHIN
  - Estimated time: 1 hour

- [ ] **Understanding Legal Ambiguity in L4**
  - Audience: Legal drafters
  - Goal: Recognize and handle ambiguous legal text
  - Topics: MAYBE types, default logic, edge cases
  - Estimated time: 1.5 hours

---

### For Developers

- [ ] **Integrating L4 Decision Services with REST APIs**

  - Audience: Backend developers
  - Goal: Call L4 decisions from web applications
  - Topics: HTTP requests, JSON, API design
  - Estimated time: 2 hours

- [ ] **Building Web Forms from L4 Schemas**

  - Audience: Frontend developers
  - Goal: Auto-generate forms from L4 type definitions
  - Topics: Type introspection, form generation
  - Estimated time: 2.5 hours

- [ ] **Batch Processing L4 Evaluations**

  - Audience: Data engineers, developers
  - Goal: Process multiple cases efficiently
  - Topics: YAML input, batch CLI usage, scripting
  - Estimated time: 1 hour

- [ ] **Setting Up CI/CD for L4 Projects**

  - Audience: DevOps engineers, developers
  - Goal: Automate testing and deployment
  - Topics: GitHub Actions, testing, deployment
  - Estimated time: 1.5 hours

- [ ] **Error Handling in L4 Decision Services**

  - Audience: Backend developers
  - Goal: Handle edge cases and errors gracefully
  - Topics: MAYBE, EITHER, error messages
  - Estimated time: 1 hour

- [ ] **Optimizing L4 Performance**
  - Audience: Performance engineers
  - Goal: Make L4 evaluations faster
  - Topics: Profiling, optimization strategies
  - Estimated time: 2 hours

---

### For Legal Engineers

- [ ] **Translating Legislation to L4 (Deep Dive)**

  - Audience: Legal engineers
  - Goal: Systematic approach to encoding statutes
  - Topics: Statute structure, cross-references, amendments
  - Estimated time: 3 hours

- [ ] **Handling Ambiguity in Legal Text**

  - Audience: Legal engineers, legal professionals
  - Goal: Strategies for dealing with unclear language
  - Topics: Default logic, rebuttable presumptions, annotations
  - Estimated time: 2 hours

- [ ] **Multi-Temporal Rule Modeling**

  - Audience: Advanced legal engineers
  - Goal: Model rules that change over time
  - Topics: Temporal logic, amendments, effective dates
  - Estimated time: 2.5 hours

- [ ] **Contract State Machine Patterns**

  - Audience: Legal engineers
  - Goal: Model contracts as state machines
  - Topics: States, transitions, events
  - Estimated time: 2 hours

- [ ] **Using AI to Bootstrap L4 from Documents**

  - Audience: Legal engineers
  - Goal: Leverage LLMs to generate initial L4 code
  - Topics: LLM integration, prompt engineering, review process
  - Estimated time: 2 hours

- [ ] **Version Control for Legal Rules**
  - Audience: Legal engineers, legal teams
  - Goal: Manage changes to L4 codebases
  - Topics: Git workflows, branching, reviewing
  - Estimated time: 1.5 hours

---

### For Data Scientists

- [ ] **Using L4 for Explainable AI in Legal Decisions**

  - Audience: Data scientists, ML engineers
  - Goal: Combine L4 with ML for transparent decisions
  - Topics: Hybrid reasoning, audit trails, explanations
  - Estimated time: 2.5 hours

- [ ] **Integrating L4 with Python Data Pipelines**

  - Audience: Data scientists, analysts
  - Goal: Use L4 in data processing workflows
  - Topics: Python interop, data transformations
  - Estimated time: 2 hours

- [ ] **Analyzing Legal Rules with L4**
  - Audience: Legal analytics professionals
  - Goal: Extract insights from L4-encoded rules
  - Topics: Querying, statistics, visualization
  - Estimated time: 2 hours

---

### Cross-Functional Audiences

- [ ] **L4 for Policy Makers: From Rules to Code**

  - Audience: Government, policy makers
  - Goal: Understand how policy becomes executable
  - Topics: Rule encoding, testing policy, simulation
  - Estimated time: 1.5 hours

- [ ] **L4 for Insurance: Claims Processing Rules**

  - Audience: Insurance professionals
  - Goal: Automate claims adjudication
  - Topics: Eligibility, coverage, exclusions
  - Estimated time: 2 hours

- [ ] **L4 for Banking: KYC/AML Compliance**

  - Audience: Banking compliance teams
  - Goal: Encode know-your-customer rules
  - Topics: Risk assessment, regulatory rules
  - Estimated time: 2 hours

- [ ] **L4 for Tax: Calculating Tax Liability**
  - Audience: Tax professionals, accountants
  - Goal: Model tax rules in L4
  - Topics: Deductions, credits, brackets
  - Estimated time: 2.5 hours

---

## Tutorial Templates

### Quick Start Template (30-60 min)

- Problem statement
- Prerequisites check
- Step-by-step with screenshots
- Working code example
- Verification steps
- Next steps

### Deep Dive Template (2-3 hours)

- Context and motivation
- Conceptual overview
- Detailed walkthrough
- Multiple examples
- Common pitfalls
- Advanced variations
- Further reading

### Domain-Specific Template

- Domain context (insurance, banking, etc.)
- Real-world scenario
- L4 modeling approach
- Complete working example
- Testing and validation
- Deployment considerations

---

## Contributing Tutorial Ideas

Have an idea for a tutorial? Submit it here!

### How to Suggest

1. Open a [GitHub Issue](https://github.com/smucclaw/l4-ide/issues) with:

   - Tutorial title
   - Target audience
   - Learning goals
   - Estimated time
   - Key topics to cover

2. Label the issue as `documentation` and `tutorial-idea`

3. Discuss with maintainers to refine scope

### How to Write a Tutorial

1. Use one of the templates above
2. Create working `.l4` examples in `/docs/examples/tutorials/`
3. Verify examples work with `cabal run jl4-cli`
4. Write tutorial content in appropriate subfolder
5. Link examples from tutorial
6. Submit pull request

Contributions welcome via GitHub pull requests.

---

## Recently Completed

_(Tutorials will be moved here as they are completed)_

- [x] [LLM Getting Started](llm-integration/llm-getting-started.md)
- [x] [Legislative Ingestion](llm-integration/legislative-ingestion.md)
- [x] [Your First L4 File](getting-started/first-l4-file.md)
- [x] [Encoding Legislation](getting-started/encoding-legislation.md)

---

## Notes for Tutorial Authors

### Audience Matters

- Tailor language and examples to audience expertise
- Define terms that may be unfamiliar
- Use domain-appropriate examples

### Working Code is Essential

- Every tutorial must have working examples
- Examples should be copy-paste ready
- Verify before publishing

### Time Estimates

- Be realistic about time required
- Include time for reading, coding, and experimentation
- Consider breaks for longer tutorials

### Learning Outcomes

- State clear learning objectives upfront
- Validate that tutorial achieves objectives
- Provide self-assessment questions

---

## Inspiration

Looking for tutorial ideas? Check:

- [GitHub Issues](https://github.com/smucclaw/l4-ide/issues) - User questions
- [Discussion Forum](https://github.com/smucclaw/l4-ide/discussions) - Common topics
- [Course Modules](../courses/README.md) - Expand on course topics
- Real-world L4 projects - What would have helped?
