# L4 Advanced Course

**For developers ready to build production-grade legal systems**

Welcome to the L4 Advanced Course! This course assumes you've completed the [Foundation Course](../foundation-course-ai/) and are ready to tackle real-world legal system development.

## What You'll Learn

This course covers the full stack of production L4 development:

- **Professional development workflows** with VSCode and git
- **AI-assisted rule ingestion** from legal documents
- **Temporal logic** for multi-temporal reasoning
- **Decision service APIs** for enterprise integration
- **Rebuttable presumptions** with default values
- **JSON integration** with external systems
- **Regression testing** and change control
- **Multi-file architecture** for large projects
- **Legacy system migration** from Oracle Policy Modeling
- **LLM-powered semantic parsing** for natural language interfaces [planned]

By the end of this course, you'll be able to build enterprise-grade legal systems that integrate with modern software stacks.

## Prerequisites

Before starting this course, you should:

1. **Complete the Foundation Course** — Understand L4 syntax, types, functions, and control flow
2. **Have programming experience** — Comfortable with command-line tools, git, and APIs
3. **Understand legal basics** — Familiar with how rules, regulations, and policies work
4. **Have a real use case** — Best learned by applying to actual legal problems

## Course Modules

### Module A1: L4 Development Workflow in VSCode

**What you'll learn:**

- Setting up a professional L4 development environment
- Using the L4 VSCode extension for syntax highlighting and type checking
- Organizing multi-file projects
- Git workflows for L4 code
- Testing and debugging strategies

**Key takeaway:** Transform from writing L4 in a text editor to using a full IDE with autocomplete, error checking, and visualization.

[Start Module A1 →](module-a1-vscode-workflow.md)

---

### Module A2: AI-Assisted Ingestion & Model Refinement

**What you'll learn:**

- Using LLMs (ChatGPT, Claude) to extract rules from legal documents
- Prompt engineering for accurate extraction
- Iterative refinement workflow: extract → draft → refine → test
- Handling ambiguities and discretionary powers
- Generating test cases with AI assistance

**Key takeaway:** Accelerate rule encoding by 10x using AI to transform PDFs and Word documents into L4 code.

[Start Module A2 →](module-a2-ai-ingestion.md)

---

### Module A3: Temporal Logic in L4 [WIP]

**What you'll learn:**

- Four temporal dimensions: valid time, system time, rule version time, encoding time
- Using the `daydate` library for date arithmetic
- The EVAL construct for multi-temporal queries (under development)
- Retroactive rule evaluation
- Temporal snapshots and time travel

**Key takeaway:** Build systems that can answer "What was true on date X?" and "When did this rule change?"

**Status:** Work in progress — EVAL construct is being actively developed

[Start Module A3 →](module-a3-temporal-logic.md)

---

### Module A4: Decision Service & API Integration

**What you'll learn:**

- Using `@desc export` annotations to expose L4 functions as APIs
- Automatic API deployment from the Web IDE
- REST endpoint patterns for decision services
- JSON request/response handling
- Swagger/OpenAPI documentation
- Batch processing and error handling

**Key takeaway:** Turn L4 code into production APIs with a single annotation.

[Start Module A4 →](module-a4-decision-service.md)

---

### Module A5: Rebuttable Presumptions with TYPICALLY [ROADMAP]

**What you'll learn:**

- The TYPICALLY keyword for default values
- Legal concept of rebuttable presumptions
- Reducing question burden in interactive applications
- Type checking rules for TYPICALLY
- Runtime semantics and evaluation

**Key takeaway:** Encode legal defaults explicitly rather than hardcoding assumptions.

**Status:** On roadmap — TYPICALLY keyword is not yet implemented. An initial implementation was attempted in December 2025 but was reverted due to technical issues. See [TYPICALLY-DEFAULTS-SPEC.md](../todo/TYPICALLY-DEFAULTS-SPEC.md) for details. This module describes the planned design and serves as a preview of what's coming.

[Start Module A5 →](module-a5-rebuttable-presumptions.md)

---

### Module A6: JSON Integration (Input & Output)

**What you'll learn:**

- Mapping L4 types to JSON schemas
- Handling JSON input in decision service calls
- Generating JSON output from L4 functions
- Using JSONDECODE and JSONENCODE for type-safe JSON processing
- Making HTTP requests with FETCH (GET) and POST operators
- Accessing environment variables with ENV keyword
- Schema evolution and versioning
- Integration patterns: REST bridge, database sync, batch processing, event-driven
- API discovery with Swagger endpoints (/swagger-ui, /swagger)
- Performance optimization

**Key takeaway:** Seamlessly integrate L4 with databases, web apps, external APIs, and enterprise systems via JSON and HTTP.

[Start Module A6 →](module-a6-json-integration.md)

---

### Module A7: Regression Testing & Change Control

**What you'll learn:**

- Building test suites: unit, integration, regression
- Golden master testing for change detection
- Automated change impact analysis
- Git workflows for legal code
- Continuous integration with GitHub Actions
- Pre-commit hooks and automated testing
- Rollback procedures and version control
- Documenting breaking changes

**Key takeaway:** Ensure that rule changes don't break existing functionality using software engineering best practices.

[Start Module A7 →](module-a7-regression-testing.md)

---

### Module A8: Multi-File Pipelines & System Architecture

**What you'll learn:**

- When to split code into multiple files
- Import and module dependency management
- Module design patterns: type-first, validation layer, orchestration, API export
- Avoiding circular dependencies
- Team collaboration patterns and code ownership
- Testing strategies for multi-file projects
- System architecture patterns: microservices, plugin architecture, multi-tenant
- Performance considerations with lazy evaluation
- Documentation and deployment strategies

**Key takeaway:** Architect large-scale L4 projects that multiple teams can work on simultaneously.

[Start Module A8 →](module-a8-multi-file-architecture.md)

---

### Module A9: Importing OPM/OIA Files

**What you'll learn:**

- Understanding Oracle Policy Modeling file formats
- Using the opm2l4 command-line translator
- Translation mapping from OPM to L4
- Reviewing and refining imported code
- Testing translated rules against OPM golden masters
- Migration strategies: big bang, incremental, hybrid, new development
- Handling three-valued logic and type coercion
- Integrating OPM-translated modules with existing L4

**Key takeaway:** Migrate existing Oracle Policy Modeling investments to L4 without rewriting from scratch.

[Start Module A9 →](module-a9-opm-import.md)

---

### Module A10: LLM-Powered Semantic Parser / Chatbot [PLANNED]

**What you'll learn:**

- Building an LLM-powered interface to the L4 decision service
- Semantic parsing: converting natural language queries to function calls
- Function discovery and argument extraction with prompt engineering
- Displaying evaluation traces and visualizations to users
- Structured interaction patterns vs. free-form chatbots
- Error handling, confidence thresholds, and fallback strategies
- Integration with OpenRouter, OpenAI, or Anthropic APIs

**Key takeaway:** Create an intelligent natural language interface where users can ask questions in plain English and get structured answers from L4 reasoners.

**Status:** Planned module. Related materials exist:
- [README-AI-QUERYING.md](../../README-AI-QUERYING.md) - Shows how L4 can call LLMs (opposite direction)
- [proposal-jl4-nl-query-mar-24-2025.md](../proposal-jl4-nl-query-mar-24-2025.md) - Design proposal for semantic parser approach
- Module A4 (Decision Service APIs) and Module A6 (JSON Integration) provide foundational knowledge

**Contributions welcome!** This would be a valuable addition to the course.

---

## Learning Paths

### Path 1: Full Stack Legal Engineer (All Modules)

Complete all modules in sequence for comprehensive mastery.

**Timeline:** 4-6 weeks (Module A10 adds 1 week when available)
**Best for:** Developers building enterprise legal systems from scratch

---

### Path 2: API Integration Specialist (A1, A4, A6, A10)

Focus on integrating L4 with existing systems and user interfaces.

**Timeline:** 1-2 weeks (+ 1 week for A10 when available)
**Best for:** Backend developers integrating L4 into existing applications, building chatbots and conversational interfaces

---

### Path 3: Legal Rule Migration Expert (A1, A2, A9)

Specialize in ingesting and migrating legal rules.

**Timeline:** 2-3 weeks
**Best for:** Developers migrating from legacy systems or digitizing legal documents

---

### Path 4: Quality Assurance Specialist (A1, A6, A7)

Focus on testing and quality control for legal code.

**Timeline:** 2 weeks
**Best for:** QA engineers ensuring legal system reliability

---

### Path 5: Enterprise Architect (A1, A6, A7, A8)

Master large-scale system design and team collaboration.

**Timeline:** 3-4 weeks
**Best for:** Tech leads and architects designing multi-team L4 projects

---

## Capstone Project

After completing the course, build a complete system that demonstrates your skills:

### Project: WorkPass Authority API

Build a production-ready API for immigration eligibility assessment:

**Requirements:**

- Multi-file architecture (types, validation, eligibility, operations, API)
- Decision service with @desc export annotations
- JSON integration with external HR systems
- Comprehensive test suite (unit, integration, regression)
- CI/CD pipeline with GitHub Actions
- Swagger documentation
- Error handling and logging

**Deliverables:**

1. L4 source code (well-organized, documented)
2. Test suite with 90%+ coverage
3. API documentation
4. Deployment guide
5. Change log with semantic versioning

**Time estimate:** 20-40 hours

---

## Development Environment Setup

### Required Tools

1. **L4 Compiler**

   ```bash
   cd l4-ide
   cabal build all
   cabal install jl4-cli
   ```

2. **VSCode with L4 Extension**

   ```bash
   code --install-extension l4-lang.l4-vscode
   ```

3. **Git** for version control

   ```bash
   git config --global user.name "Your Name"
   git config --global user.email "your.email@example.com"
   ```

4. **Node.js** (for opm2l4 and tooling)

   ```bash
   # Install Node.js 18+
   node --version  # Should be v18.0.0 or higher
   ```

5. **Optional: Docker** (for decision service deployment)
   ```bash
   docker --version
   ```

### Recommended VSCode Extensions

- **L4** — Syntax highlighting and type checking
- **GitLens** — Enhanced git integration
- **REST Client** — Test API endpoints
- **Markdown All in One** — Documentation writing
- **Error Lens** — Inline error messages

---

## Getting Help

### Community Resources

- **GitHub Discussions** — Ask questions and share projects
- **Discord Server** — Real-time chat with the L4 community
- **Stack Overflow** — Tag questions with [l4] and [legal-tech]

### Documentation

- **L4 Language Reference** — Comprehensive syntax guide
- **API Documentation** — Decision service API reference
- **Example Projects** — Real-world L4 codebases

### Support

- **GitHub Issues** — Report bugs and request features
- **Email Support** — support@legalese.com

---

## Course Materials

All course materials are open source and available on GitHub:

```bash
git clone https://github.com/smucclaw/l4-ide
cd l4-ide/doc/advanced-course-ai
```

Each module includes:

- Conceptual explanations
- Code examples
- Hands-on exercises
- Best practices
- Common pitfalls

---

## Assessment and Certification

### Module Exercises

Each module includes hands-on exercises to reinforce learning. Complete all exercises to solidify your understanding.

### Capstone Project

The capstone project demonstrates your ability to build production-grade L4 systems. Submit your project for community review and feedback.

### Certification (Coming Soon)

Earn an L4 Advanced Developer Certificate by:

1. Completing all module exercises
2. Building and submitting the capstone project
3. Passing a practical assessment

---

## What's Next?

After completing this course, you can:

### Contribute to L4 Development

- Submit pull requests to the L4 compiler
- Write VSCode extension features
- Create libraries for specific legal domains
- Improve documentation and tutorials

### Build Production Systems

- Deploy L4 decision services to production
- Integrate L4 with enterprise systems
- Build industry-specific applications
- Offer L4 development services

### Join the Research Community

- Explore formal verification techniques
- Develop domain-specific language extensions
- Research legal AI and explainability
- Publish papers on computational law

### Teach Others

- Create your own L4 tutorials
- Host workshops and training sessions
- Mentor new L4 developers
- Build educational materials

---

## Course Authors

This course was developed by the L4 core team with contributions from:

- **Meng Wong** — L4 language designer, SMU Centre for Computational Law
- **The L4 Community** — Contributors, users, and early adopters

---

## License

This course is licensed under Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0).

You are free to:

- **Share** — Copy and redistribute the material
- **Adapt** — Remix, transform, and build upon the material

Under the following terms:

- **Attribution** — Give appropriate credit
- **ShareAlike** — Distribute under the same license

---

## Changelog

### Version 1.1 (2025-12-17)

- Added Module A10 (LLM-Powered Semantic Parser / Chatbot) as planned module
- Updated learning paths to include A10
- Noted related materials (README-AI-QUERYING.md, proposal document)

### Version 1.0 (2024-12-01)

- Initial release with 9 modules
- Foundation Course prerequisites
- Capstone project guidelines
- Learning paths for different roles

---

**Ready to build production-grade legal systems?**

[Start with Module A1: L4 Development Workflow →](module-a1-vscode-workflow.md)
