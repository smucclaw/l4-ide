# L4 Marketecture: From Legal Text to Production Systems

## The Central Question

**"After I've converted my rules to L4, what can I usefully do with them?"**

## The Answer: Everything

L4 is not just a programming language—it's a **legal computation platform** that automatically generates multiple artifacts from a single source of truth.

```
                    ┌─────────────────────────────────────────┐
                    │                                         │
                    │         LEGAL RULES IN L4               │
                    │    (Single Source of Truth)             │
                    │                                         │
                    │  - Type-checked                         │
                    │  - Version-controlled                   │
                    │  - Testable                             │
                    │  - Verifiable                           │
                    │                                         │
                    └──────────────┬──────────────────────────┘
                                   │
                    ┌──────────────┴──────────────┐
                    │                             │
           ┌────────▼─────────┐         ┌────────▼─────────┐
           │                  │         │                  │
           │  DEVELOPMENT     │         │  PRODUCTION      │
           │  OUTPUTS         │         │  OUTPUTS         │
           │                  │         │                  │
           └────────┬─────────┘         └────────┬─────────┘
                    │                             │
        ┌───────────┼───────────┐                 │
        │           │           │                 │
        ▼           ▼           ▼                 │
    ┌─────┐    ┌─────┐    ┌─────┐                │
    │ IDE │    │Tests│    │Docs │                │
    │Tools│    │     │    │     │                │
    └─────┘    └─────┘    └─────┘                │
                                                  │
                    ┌─────────────────────────────┼──────────────────────┐
                    │                             │                      │
                    ▼                             ▼                      ▼
            ┌───────────────┐           ┌─────────────────┐    ┌──────────────┐
            │  REST API     │           │  WEB APPS       │    │ INTEGRATIONS │
            │  (Decision    │           │  (Consumer UI)  │    │ (Enterprise) │
            │   Service)    │           │                 │    │              │
            └───────┬───────┘           └────────┬────────┘    └──────┬───────┘
                    │                            │                     │
        ┌───────────┼────────────┐               │          ┌──────────┼────────┐
        │           │            │               │          │          │        │
        ▼           ▼            ▼               ▼          ▼          ▼        ▼
    ┌─────┐    ┌──────┐    ┌────────┐      ┌────────┐  ┌─────┐  ┌──────┐  ┌──────┐
    │Batch│    │Trace │    │Swagger │      │Wizard  │  │Chat │  │SQL   │  │Event │
    │Eval │    │Debug │    │  UI    │      │  UI    │  │ bot │  │Bridge│  │Stream│
    └─────┘    └──────┘    └────────┘      └────────┘  └─────┘  └──────┘  └──────┘
```

---

## Input: Creating L4 Code

### 1. **AI-Assisted Ingestion** 🤖

Upload legal documents (PDF, Word, web pages) and use LLM assistance to extract rules into L4.

**Tools:**

- Claude Code Plugin (cloud-based validation)
- Module A2 workflow (prompt templates)
- LLM Integration Library (multi-provider)

**Output:** First-draft L4 code with types and functions

---

### 2. **Manual Authoring** ✍️

Write L4 directly in professional IDEs with full language support.

**Tools:**

- VSCode Extension (syntax highlighting, type checking, visualization)
- LSP Server (hover info, diagnostics, jump-to-definition)
- Web IDE (browser-based editor at jl4.legalese.com)
- REPL (interactive exploration)

**Output:** Production-grade L4 code

---

## Development Outputs: Building Confidence

### 3. **Type Checking & Verification** ✅

Catch errors before deployment—ambiguities, contradictions, type mismatches.

**Capabilities:**

- Bidirectional type inference
- Exhaustiveness checking (pattern matching)
- TDNR (Type-Directed Name Resolution)
- Formal verification (SAT/SMT integration, roadmap)

**Output:** Compile-time guarantees of correctness

---

### 4. **Visualization & Understanding** 📊

See your logic as interactive diagrams, not just text.

**Tools:**

- Ladder Diagrams (interactive decision trees with AND/OR/NOT logic)
- GraphViz Traces (evaluation step-by-step with timestamps)
- Query Planning (relevance analysis showing which variables matter)

**Output:** Visual documentation and debugging aids

---

### 5. **Testing & Regression Suites** 🧪

Test rules against scenarios before production deployment.

**Capabilities:**

- `#EVAL` for unit tests
- `#ASSERT` for validation
- Golden file testing (snapshot testing)
- Batch evaluation with test fixtures

**Output:** Confidence that rules behave as intended

---

### 6. **Documentation Generation** 📚

Auto-generate human-readable documentation from code.

**Capabilities:**

- `@desc` annotations extracted as documentation
- JSON Schema generation (type definitions)
- Swagger/OpenAPI specs (API documentation)
- Natural language generation (roadmap)

**Output:** Always-up-to-date documentation

---

## Production Outputs: Deploying Value

### 7. **REST API (Decision Service)** 🌐

Expose L4 functions as HTTP endpoints with zero additional code.

**Features:**

- Single `@export` annotation creates API
- Automatic JSON schema validation
- Batch processing (parallel evaluation)
- GraphViz traces included in responses
- Module precompilation (10-100x speedup)
- Swagger UI at `/swagger-ui`

**Use Cases:**

- Microservices architecture
- Mobile app backends
- Chatbot integration
- Enterprise system integration

**Example:**

```bash
# Single evaluation
curl -X POST 'http://api.example.com/functions/eligibility/evaluation' \
  -H 'Content-Type: application/json' \
  -d '{"age": 25, "education": "degree", "salary": 6000}'

# Batch processing
curl -X POST 'http://api.example.com/functions/eligibility/batch' \
  -H 'Content-Type: application/json' \
  -d '{"cases": [{"age": 25, "salary": 6000}, {"age": 45, "salary": 8000}]}'
```

---

### 8. **Consumer Web Apps (l4-wizard)** 🧙

Auto-generate interactive questionnaires with intelligent question ordering.

**Features:**

- Progressive disclosure (query planning)
- Visual feedback (questions gray out when irrelevant)
- Schema-driven inputs (boolean, number, enum, text)
- Interactive ladder diagram
- Real-time evaluation
- Shareable URLs with pre-filled answers

**Use Cases:**

- Public-facing eligibility checkers
- Compliance assessment tools
- Interactive policy explainers
- Self-service legal guidance

**Example:**

```
https://wizard.example.com/?fn=may_purchase_alcohol
→ User answers age, marital status, beverage type
→ System asks only relevant questions
→ Visual feedback shows decision logic
→ Final result with explanation
```

---

### 9. **AI Chatbot Interface** 💬

Natural language interface for non-technical users.

**Features:**

- Semantic parsing (natural language → function calls)
- Function discovery (LLM finds relevant rules)
- Argument extraction (LLM interprets user input)
- Trace visualization in conversation
- Structured interaction patterns

**Use Cases:**

- Customer support automation
- Legal advice chatbots
- Regulatory compliance assistants
- Employee self-service portals

**Example Conversation:**

```
User: "Can I purchase alcohol if I'm 25 and married?"
Bot:  Checking eligibility rules...
      ✓ Age requirement met (25 ≥ 21)
      ? Are you purchasing beer only? Or does your spouse approve?
User: "My spouse approves"
Bot:  ✅ Yes, you may purchase alcohol.
      [View decision trace] [Share this result]
```

**Status:** Core infrastructure complete (LLM integration library, decision service API, query planning). Semantic parser implementation is Module A10 (planned). Related materials in `doc/tutorial/llm-getting-started.md` and `doc/proposal-jl4-nl-query-mar-24-2025.md`.

---

### 10. **Enterprise System Integration** 🏢

Connect L4 to existing databases, ERP systems, and business processes.

**Integration Patterns:**

#### **REST Bridge**

- L4 Decision Service ↔ Enterprise API Gateway
- JSON request/response with schema validation

#### **Database Sync**

- L4 queries pull data from SQL/NoSQL
- Decision results written back to OLTP systems

#### **Event Stream Processing**

- Kafka/RabbitMQ events trigger L4 evaluation
- Results published back to event bus

#### **Batch Processing**

- Nightly jobs evaluate thousands of cases
- Results loaded into data warehouse

**Capabilities:**

- `FETCH` (HTTP GET) and `POST` operators
- `JSONDECODE` / `JSONENCODE` (type-safe JSON)
- `ENV` keyword (access environment variables)
- Batch evaluation API (parallel processing)

**Use Cases:**

- Underwriting automation (insurance)
- Credit decisioning (banking)
- Compliance monitoring (regulatory)
- Benefits eligibility (government)

---

## Real-World Value Delivered

### **Government Regulatory Compliance**

- Encoded secondary legislation
- Auto-generated web wizards
- **Result:** Discovered race condition (double bind) via formal verification

### **Insurance Policy Analysis**

- Formalized contracts from major providers
- Found ambiguities in payout formulas
- **Result:** Prevented millions in claims leakage

### **Legislative Drafting**

- Rules-as-code initiatives with government legal offices
- Legislation written in machine-verifiable form
- **Result:** Faster iteration, fewer contradictions

### **Commercial Agreements**

- Complex fee schedules and payment terms
- SQL-like APIs for enterprise integration
- **Result:** Real-time pricing calculations, audit trails

---

## The "Whole Product" Vision

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                 │
│  INPUT                    L4 CODE              OUTPUT           │
│                                                                 │
│  Legal Text    ─────►   Single Source   ─────►  Everything     │
│  (PDF/Word)             of Truth                You Need        │
│                                                                 │
│  - Contracts            - Typed                 - APIs          │
│  - Regulations          - Tested                - Web Apps      │
│  - Policies             - Versioned             - Chatbots      │
│  - Legislation          - Verifiable            - Integrations  │
│                                                 - Documentation │
│                                                 - Tests         │
│                                                 - Visualizations│
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Comparison: L4 vs Traditional Approaches

| Capability              | Traditional Legal Tech            | L4 Platform                                        |
| ----------------------- | --------------------------------- | -------------------------------------------------- |
| **Source of Truth**     | Word documents, scattered systems | Single L4 codebase                                 |
| **Testing**             | Manual review, production bugs    | Automated test suites, pre-deployment verification |
| **Ambiguity Detection** | Litigation discovers it           | Type checker catches it                            |
| **API Development**     | Months of backend coding          | `@export` annotation                               |
| **UI Development**      | Custom React/Angular apps         | Auto-generated wizard                              |
| **Documentation**       | Out-of-sync Word docs             | Auto-generated from code                           |
| **Explainability**      | Black box decisions               | GraphViz traces, audit trails                      |
| **Change Management**   | Risk of breaking changes          | Regression tests, type safety                      |
| **Integration**         | Custom APIs per system            | Standard REST, JSON schemas                        |
| **Formal Verification** | Not available                     | SAT/SMT integration (roadmap)                      |

---

## Deployment Options

### **Option 1: Cloud-Hosted Decision Service**

- Deploy to AWS/Azure/GCP
- Auto-scaling with load balancer
- Monitoring with Prometheus/Grafana
- Example: `https://jl4.legalese.com`

### **Option 2: On-Premises (Docker)**

- Docker Compose for full stack
- Internal network deployment
- Air-gapped environments supported

### **Option 3: Embedded (Library)**

- Link L4 evaluator as Haskell library
- Embed in larger applications
- Zero network latency

### **Option 4: Serverless (AWS Lambda)**

- Function-as-a-Service deployment
- Pay-per-evaluation pricing
- Automatic scaling

---

## Getting Started: Three Paths

### **Path 1: Legal Engineer** (Writing L4)

1. Complete Foundation Course (7 modules)
2. Use AI ingestion to convert first document
3. Refine in VSCode with LSP support
4. Deploy to decision service with `@export`
5. Share l4-wizard link with end-users

**Timeline:** 1-2 weeks to first production deployment

---

### **Path 2: Backend Developer** (Integrating L4)

1. Read Advanced Course Module A4 (Decision Service)
2. Call REST API from your application
3. Use Swagger UI to explore endpoints
4. Integrate batch evaluation into existing workflows
5. Monitor with traces and analytics

**Timeline:** 2-3 days to first integration

---

### **Path 3: End-User** (Using L4 Apps)

1. Open l4-wizard URL (e.g., `?fn=eligibility`)
2. Answer questions (only relevant ones asked)
3. View decision with visual explanation
4. Share result URL with others
5. Ask chatbot for natural language clarification

**Timeline:** 5 minutes to first decision

---

## Next Steps

### **For Your All-Hands Meeting**

1. **Show the marketecture diagram** (this document)
2. **Demo each output** (API, wizard, chatbot, visualization)
3. **Highlight real-world impact** (government, insurance, legislative)
4. **Call to action** (start with Foundation Course)

### **For New Users**

1. **Read this document** to understand capabilities
2. **Watch 5-minute demo video** (to be recorded)
3. **Complete quickstart tutorial** (doc/foundation-course-ai/quickstart.md)
4. **Join community discussions** (GitHub, Discord)

### **For Contributors**

1. **Review open issues** (48 issues, prioritized in CLAUDE.md)
2. **Implement missing pieces** (chatbot UI, Docker Compose, templates)
3. **Improve documentation** (videos, examples, patterns)
4. **Build integrations** (language-specific SDKs, connectors)

---

## Conclusion

**L4 is not just a language—it's a complete platform** for turning legal rules into production systems. From AI-assisted ingestion to REST APIs to consumer web apps to chatbot interfaces, L4 delivers **everything you need** from a single source of truth.

**The answer to "what can I do with L4?"** is simple: **Everything you currently build with months of custom code, but automatically generated from type-checked, tested, verifiable legal specifications.**

---

**Questions? Issues? Contributions?**

- GitHub: https://github.com/smucclaw/l4-ide
- Documentation: https://jl4.legalese.com
- Email: hello@legalese.com
