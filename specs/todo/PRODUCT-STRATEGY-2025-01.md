# Product Strategy & Prioritization - January 2025

**Date:** January 9, 2026  
**Context:** Sprint planning session  
**Status:** 📋 ACTIVE ROADMAP  
**Related:** `doc/MARKETECTURE.md`, `CLAUDE.md`, `AGENTS.md`

## Executive Summary

L4 has achieved remarkable technical maturity in the last 3 months (l4-wizard, prohibition semantics, LLM integration, plugin distribution, cross-platform builds). The system now offers a compelling "whole product" experience from legal text → L4 code → REST API → end-user interface.

**Critical insight:** New users consistently ask **"After I convert my rules to L4, what can I do with them?"** This question drove the creation of `doc/MARKETECTURE.md` to articulate the complete value proposition.

**Strategic gaps** remain in three areas:

1. **Onboarding friction** - Setup is complex, documentation incomplete
2. **Error experience** - Messages cryptic, diagnostics missing common pitfalls
3. **"Whole product" polish** - Missing connective tissue between components

This document prioritizes upcoming work based on user impact and demo readiness.

---

## Current State Assessment

### ✅ What's Working Exceptionally Well

**Core Language (9/10)**

- Mature type system with bidirectional checking, TDNR, algebraic types
- Lazy evaluation with trace generation
- Query planning with relevance analysis
- Prohibition semantics (MUST NOT, BREACH) complete
- LLM integration library (multi-provider)
- Cross-platform builds (Windows, macOS, Linux)

**Developer Tooling (8/10)**

- LSP server with hover support and diagnostics
- VSCode extension with ladder visualization
- REPL with interactive exploration (`:trace`, `:qp`, `:decides`)
- CLI with GraphViz output control
- Web IDE at jl4.legalese.com

**Decision Service APIs (9/10)**

- `@export` annotation for zero-code API exposure
- Swagger/OpenAPI auto-generated docs
- Batch processing with parallel evaluation
- GraphViz traces (DOT/PNG/SVG) for explainability
- Module precompilation (10-100x speedup)
- JSON Schema generation

**Consumer Experience (8/10)**

- l4-wizard with progressive disclosure UX
- Query-plan-driven question ordering
- Interactive ladder diagrams (clickable nodes)
- Schema-driven input controls
- Real-time evaluation feedback

**Documentation (8/10)**

- Foundation Course: 7 modules, 12,700+ lines
- Advanced Course: 11 modules covering enterprise integration
- Hands-on examples (WorkPass Authority domain)
- Multiple learning paths (Full Stack, API Integration, QA, Architecture, Contracts)

### ⚠️ Critical Gaps (Impact × Urgency)

**1. Onboarding Friction (HIGH IMPACT × HIGH URGENCY)**

_Problem:_ New users face 30+ minute setup before "Hello World"

_Evidence:_

- `doc/foundation-course-ai/quickstart.md` exists but needs better integration with main README
- No single-command setup script
- Requires manual: Haskell toolchain → npm packages → jl4-lsp install → VSCode extension VSIX
- Five separate terminals without `dev-start.sh --run` flag
- No health-check script to verify services running

_User Impact:_

- Legal professionals give up before seeing value
- Developers expect "works out of the box" experience
- Demo attendees may struggle to follow along

_Priority:_ **P0 - Pre-Sprint**

---

**2. Error Experience (HIGH IMPACT × MEDIUM URGENCY)**

_Problem:_ Cryptic errors discourage continued use

_Evidence:_

- [Issue #313](https://github.com/smucclaw/l4-ide/issues/313): Hover shows `x10` instead of actual variable name `x`
- [Issue #708](https://github.com/smucclaw/l4-ide/issues/708): WHERE indentation mistakes cause silent mis-attachment
- No LSP diagnostic for common mixfix mistakes
- GraphViz traces excellent but lack "click to expand" for large trees
- Test suite golden file behavior confusing ("fail first time, pass second")

_User Impact:_

- Users get stuck on first error and abandon
- WHERE mis-attachment is a "silent footgun"
- Difficult to debug without deep L4 knowledge

_Priority:_ **P0 - Pre-Sprint** (at least fix [#313](https://github.com/smucclaw/l4-ide/issues/313) for demos)

---

**3. "Whole Product" Polish (MEDIUM IMPACT × MEDIUM URGENCY)**

_Problem:_ Components work independently but lack connective tissue

_Evidence:_

- l4-wizard requires manual `?fn=function-name` URL parameter
- No landing page showing available functions
- No "share this decision" button (URL with pre-filled answers)
- No "export ladder as PNG" button
- AI ingestion documented (Module A2) but no web-based tool
- No demo video showing end-to-end workflow
- No template gallery (insurance, rental, employment examples)

_User Impact:_

- Users don't discover capabilities organically
- Hard to share results with stakeholders
- No "wow moment" in first 5 minutes

_Priority:_ **P1 - Post-Sprint Week 1-2**

---

**4. Deployment Automation (MEDIUM IMPACT × LOW URGENCY)**

_Problem:_ Infrastructure exists but lacks automation triggers

_Evidence:_

- [Issue #628](https://github.com/smucclaw/l4-ide/issues/628): No auto-deploy on merge to main
- [Issue #629](https://github.com/smucclaw/l4-ide/issues/629): CI builds but doesn't attach artifacts to releases (CLOSED - 80% done)
- Manual `nixos-rebuild` required for deployment
- No Docker Compose for quick production setup
- No health monitoring or error tracking

_User Impact:_

- Slow iteration cycles for dev environment
- Hard for new contributors to test changes
- No production observability

_Priority:_ **P2 - Post-Sprint Week 3-4**

---

## Priority Matrix

```
                     │
        HIGH IMPACT  │  1. Onboarding     │  2. Error
                     │     Friction       │     Experience
                     │     (P0)           │     (P0)
        ─────────────┼────────────────────┼─────────────────
                     │                    │
        MED IMPACT   │  3. "Whole         │  4. Deployment
                     │     Product"       │     Automation
                     │     Polish         │     (P2)
                     │     (P1)           │
        ─────────────┼────────────────────┼─────────────────
                     │  HIGH URGENCY      │  MED/LOW URGENCY
```

---

## Roadmap: Sprint Preparation (Pre-Demo)

### Week of Jan 9-16, 2026

**Goal:** Ensure smooth demo and minimize attendee setup friction

**Status Update (Jan 10, 2026):** 2 of 6 tasks complete, 11 hours of work remaining

#### P0 Tasks (Must Complete)

**1. Complete Quickstart Documentation** (4 hours)

- Status: `doc/foundation-course-ai/quickstart.md` exists, references updated
- Owner: Documentation lead
- Acceptance:
  - Copy-paste commands for macOS, Linux, Windows
  - "Hello World in 5 minutes" section
  - Link from main README.md
  - Tested on fresh machine
- Files: `doc/foundation-course-ai/quickstart.md`

**2. Add Dev Health-Check Script** (2 hours)

- Status: Missing
- Owner: Infrastructure
- Acceptance:
  - `./dev-healthcheck.sh` verifies all services responding
  - Returns exit code 0 if healthy, 1 if issues
  - Shows status table (LSP, decision-service, websessions, jl4-web, l4-wizard)
  - Documents in `dev-start.sh` header
- Files: `dev-healthcheck.sh`

**3. Fix [Issue #313](https://github.com/smucclaw/l4-ide/issues/313) - Hover Inference Variables** (6 hours)

- Status: Open since 2024
- Owner: LSP team
- Acceptance:
  - Hover shows user-defined names (`x`, `employee`) not inference vars (`x10`, `x42`)
  - Doesn't break existing hover functionality
  - Golden tests updated
- Files: `jl4-lsp/src/LSP/L4/Hover.hs`

**4. Record 5-Minute Demo Video** (3 hours)

- Status: Missing
- Owner: Product/PM
- Acceptance:
  - Shows: Legal PDF → AI ingestion → VSCode → API → l4-wizard → decision
  - Uploaded to YouTube (unlisted)
  - Embedded in README.md and doc/MARKETECTURE.md
  - Closed captions
- Deliverable: YouTube link

**5. Add l4-wizard Landing Page** (4 hours)

- Status: Requires `?fn=` parameter
- Owner: Frontend
- Acceptance:
  - Root URL shows function list with descriptions
  - Pulled from `/functions` endpoint
  - Click function → load wizard with bindings form
  - "Try Example" button with pre-filled values
- Files: `ts-apps/l4-wizard/src/routes/+page.svelte`

**6. Rehearse Demo Flow** (2 hours)

- Status: Not started
- Owner: PM/Demo lead
- Acceptance:
  - Run through 5-stage demo 3x without errors
  - Backup recordings for each segment
  - Pre-launch all services with `dev-start.sh full --run`
  - Test URLs accessible

#### P1 Tasks (Nice to Have)

**7. Add "Share Decision" Button** (3 hours)

- Status: Missing
- Owner: Frontend
- Files: `ts-apps/l4-wizard/src/lib/components/OutcomeBanner.svelte`
- Feature: Generate URL with bindings in query string

**8. Add "Export Ladder as PNG" Button** (2 hours)

- Status: Missing
- Owner: Frontend
- Files: `ts-apps/l4-wizard/src/lib/components/LadderDiagram.svelte`
- Feature: Client-side canvas render → download PNG

---

## Roadmap: Post-Sprint (4-Week Period)

### Week 1: Onboarding & Discoverability (Jan 16-23)

**Theme:** Reduce time-to-first-value from 30 minutes to 5 minutes

**Deliverables:**

1. **One-Click Setup Scripts** (8 hours)
   - `./setup.sh` installs everything (GHCup, cabal, npm, jl4-lsp)
   - Platform detection (macOS/Linux/Windows)
   - Interactive prompts with defaults
   - Idempotent (safe to re-run)

2. **Docker Compose for Full Stack** (6 hours)
   - Single `docker-compose up` launches all services
   - LSP, decision-service, websessions, jl4-web, l4-wizard
   - Persistent volumes for database
   - Health checks

3. **Template Gallery** (10 hours)
   - 5-10 pre-built examples (insurance, rental, employment, parking, citizenship)
   - Each with: L4 code + YAML test data + README
   - Deployed to l4-wizard with "Try This Example" buttons
   - Location: `jl4/templates/`

4. **Common Mistakes Cheat Sheet** (4 hours)
   - Top 20 errors with solutions
   - WHERE vs LET confusion
   - Mixfix precedence
   - Type coercion patterns
   - Location: `doc/COMMON-MISTAKES.md`

**Success Metric:** New user sees working example in 5 minutes

---

### Week 2: Error Experience & Diagnostics (Jan 23-30)

**Theme:** Turn cryptic errors into actionable guidance

**Deliverables:**

1. **LSP: WHERE Indentation Diagnostic** (8 hours)
   - Implements [Issue #708](https://github.com/smucclaw/l4-ide/issues/708)
   - Warning: "WHERE clause may attach to wrong expression due to indentation"
   - Suggests fix: "Add blank line or indent WHERE under target expression"
   - Files: `jl4-lsp/src/LSP/L4/Diagnostics.hs`

2. **LSP: Common Mixfix Error Hints** (6 hours)
   - Detect: operator used before defined
   - Detect: ambiguous mixfix parse
   - Suggest: "Did you mean `operator _` (postfix)?"
   - Files: `jl4-lsp/src/LSP/L4/Diagnostics.hs`

3. **Improved Golden Test Documentation** (3 hours)
   - Explain "fail first time, pass second" behavior
   - Add `--accept-golden` flag for intentional updates
   - Location: `doc/dev/TESTING.md`

4. **Interactive GraphViz Traces** (10 hours)
   - Click node → expand subtree
   - Hover node → show expression text
   - Filter: hide irrelevant branches
   - Export subset as PNG
   - Files: `ts-shared/l4-ladder-visualizer/`

**Success Metric:** User resolves first error without external help

---

### Week 3: Deployment Automation (Jan 30 - Feb 6)

**Theme:** From manual deploy to continuous delivery

**Deliverables:**

1. **Auto-Deploy to Dev on Main Merge** (6 hours)
   - Implements [Issue #628](https://github.com/smucclaw/l4-ide/issues/628)
   - GitHub Actions workflow triggers `nixos-rebuild` on dev server
   - SSH key authentication
   - Slack notification on success/failure
   - Files: `.github/workflows/deploy-dev.yml`

2. **Release Artifact Automation** (8 hours)
   - Extends [Issue #629](https://github.com/smucclaw/l4-ide/issues/629) work (CLOSED - 80% done)
   - Attach static binaries to GitHub releases
   - Windows: jl4-lsp.exe, jl4-cli.exe
   - Linux: jl4-lsp, jl4-cli (x86_64, aarch64)
   - macOS: jl4-lsp, jl4-cli (Intel, Apple Silicon)
   - Files: `.github/workflows/*-release.yml`

3. **Production Monitoring Setup** (10 hours)
   - Health check endpoints: `/health`, `/metrics`
   - Prometheus metrics export
   - Grafana dashboard (requests/sec, latency, errors)
   - Sentry error tracking integration
   - Files: `jl4-decision-service/src/Server.hs`, `nix/monitoring.nix`

4. **dev-stop.sh Script** (1 hour)
   - Mentioned in `dev-start.sh` but doesn't exist
   - Reads PIDs from `JL4_PIDFILE`
   - Graceful shutdown with SIGTERM, then SIGKILL if needed
   - Files: `dev-stop.sh`

**Success Metric:** Deploy to dev in <5 minutes, zero manual steps

---

### Week 4: Documentation & Integration (Feb 6-13)

**Theme:** From "here's the code" to "here's how to use it"

**Deliverables:**

1. **Pattern Library** (12 hours)
   - 15-20 common legal constructs as templates
   - Eligibility rules, fee calculations, temporal conditions
   - Multi-party obligations, breach scenarios
   - Copy-paste ready with annotations
   - Location: `doc/patterns/`

2. **Quick Integration Guide** (6 hours)
   - "15 minutes to first API call"
   - curl examples for all endpoints
   - Postman collection download
   - Language-specific quickstarts (TypeScript, Python, Java)
   - Location: `doc/integration/QUICKSTART.md`

3. **End-User Guide for l4-wizard** (8 hours)
   - Targeted at non-developers using L4 apps
   - "Understanding Decision Explanations"
   - "How to Read Ladder Diagrams"
   - FAQ about specific rules
   - Location: `doc/end-user/`

4. **Architecture Decision Records (ADRs)** (6 hours)
   - Document major design choices
   - Why lazy evaluation? Why layout-sensitive syntax?
   - Why TDNR? Why query planning?
   - Template for future ADRs
   - Location: `doc/adr/`

**Success Metric:** Developer integrates L4 API in 15 minutes

---

## Open Issues Triaged

### ✅ Closed (Completed in Recent Work)

- [**#634**](https://github.com/smucclaw/l4-ide/issues/634) - BREACH syntax ([PR #726](https://github.com/smucclaw/l4-ide/pull/726)) ✅
- [**#629**](https://github.com/smucclaw/l4-ide/issues/629) - CI/CD builds (CLOSED by user - 80% done, artifacts remaining) ⏳

### 📝 Updated with Progress Comments

- [**#628**](https://github.com/smucclaw/l4-ide/issues/628) - Auto-deploy (infrastructure exists, trigger missing)

### 🔴 High Priority (P0-P1)

- [**#313**](https://github.com/smucclaw/l4-ide/issues/313) - Hover shows inference variables (P0 - fix pre-demo)
- [**#708**](https://github.com/smucclaw/l4-ide/issues/708) - LSP warn on WHERE indentation (P1 - Week 2)
- [**#630**](https://github.com/smucclaw/l4-ide/issues/630) - Visualizer expansion improvements (P1 - investigate)

### 🟡 Medium Priority (P2)

- [**#502**](https://github.com/smucclaw/l4-ide/issues/502) - Viz deployment docs for non-Lir consumers
- [**#510**](https://github.com/smucclaw/l4-ide/issues/510) - Render NLG output in webview
- [**#418**](https://github.com/smucclaw/l4-ide/issues/418) - Folding support for § sections

### 🟢 Low Priority (Backlog)

- [**#645**](https://github.com/smucclaw/l4-ide/issues/645) - Auto-default MAYBE attributes
- [**#641**](https://github.com/smucclaw/l4-ide/issues/641) - Dictionary (map) type implementation
- [**#640**](https://github.com/smucclaw/l4-ide/issues/640) - Semantics of counterfactual and "subject to"
- [**#638**](https://github.com/smucclaw/l4-ide/issues/638) - Interactive boolean minimization
- [**#541**](https://github.com/smucclaw/l4-ide/issues/541) - Untimed obligations
- [**#540**](https://github.com/smucclaw/l4-ide/issues/540) - Meta-rule priorities
- [**#539**](https://github.com/smucclaw/l4-ide/issues/539) - Formal verification integration
- [**#491**](https://github.com/smucclaw/l4-ide/issues/491) - Permutation parsing
- [**#490**](https://github.com/smucclaw/l4-ide/issues/490) - Model `UPON Event`
- [**#420**](https://github.com/smucclaw/l4-ide/issues/420) - Default values for named arguments
- [**#410**](https://github.com/smucclaw/l4-ide/issues/410) - Unnamed arguments

### ❓ Needs Review

- [**#635**](https://github.com/smucclaw/l4-ide/issues/635) - "Critical decision service improvements" (too vague, review details)

---

## Demo Script (30 Minutes)

### Act 1: The Problem (5 min)

**Message:** Legal documents are ambiguous and untestable

- Show dense insurance policy or regulation
- Highlight cross-references, ambiguities, edge cases
- "What if we could treat contracts as code?"

### Act 2: AI-Assisted Ingestion (5 min)

**Message:** LLMs accelerate rule extraction 10x

- Demo: Paste PDF excerpt into Claude Code plugin
- Show prompt template from Module A2
- Extract types and functions automatically
- Emphasize: "AI-assisted, not fully automated"

### Act 3: Developer Experience (7 min)

**Message:** Professional tooling for legal engineering

- Open L4 file in VSCode
- Syntax highlighting, type checking, hover info
- REPL: `:decides`, `:qp`, `:trace`
- Ladder diagram visualization
- Run test suite: `cabal test all`

### Act 4: API Generation (3 min)

**Message:** From legal code to REST API in seconds

- Add `@export` annotation
- Show Swagger UI at `/swagger-ui`
- Make API call with curl
- Show GraphViz trace in response

### Act 5: End-User Experience (7 min)

**Message:** Auto-generated UIs with smart question ordering

- Open l4-wizard: `?fn=may_purchase_alcohol`
- Progressive disclosure (questions gray out)
- Interactive ladder diagram (click nodes)
- Real-time evaluation
- Final decision with trace

### Act 6: The Future (3 min)

**Message:** Roadmap and call to action

- Chatbot interface (Module A10)
- Formal verification (SAT/SMT)
- Template gallery and pattern library
- "Start writing L4 today - Foundation Course available"

**Backup Plan:** Pre-record each segment in case of technical difficulties

---

## Success Metrics

### Pre-Demo (Week of Jan 9)

- [ ] Demo rehearsed 3x without errors
- [ ] Quickstart.md complete and tested
- [ ] Issue #313 fixed (hover shows real names)
- [ ] 5-minute demo video uploaded
- [ ] l4-wizard landing page deployed
- [ ] Health-check script working

### Post-Demo Week 1 (Onboarding)

- [ ] New user setup time: 30 min → 5 min
- [ ] Docker Compose launches all services
- [ ] Template gallery has 5+ examples
- [ ] Common Mistakes cheat sheet published

### Post-Demo Week 2 (Error Experience)

- [ ] WHERE indentation diagnostic implemented
- [ ] Mixfix error hints added
- [ ] Interactive GraphViz traces deployed
- [ ] Golden test behavior documented

### Post-Demo Week 3 (Deployment)

- [ ] Auto-deploy to dev on main merge
- [ ] Release artifacts attached to GitHub releases
- [ ] Monitoring dashboard live
- [ ] dev-stop.sh implemented

### Post-Demo Week 4 (Documentation)

- [ ] Pattern library has 15+ examples
- [ ] Quick Integration Guide published
- [ ] End-User Guide complete
- [ ] 5 ADRs written

---

## Risk Register

### Risk 1: Setup Complexity Discourages Adoption

**Impact:** High | **Likelihood:** High  
**Mitigation:**

- P0: Complete doc/foundation-course-ai/quickstart.md with copy-paste commands
- P0: Record 5-minute video walkthrough
- P1: Docker Compose for one-command setup
- P1: One-click setup script

### Risk 2: Users Get Stuck on First Error

**Impact:** High | **Likelihood:** Medium  
**Mitigation:**

- P0: Fix hover inference variables (#313)
- P1: Add WHERE indentation diagnostic (#708)
- P1: Common Mistakes cheat sheet
- P1: Improve LSP diagnostics

### Risk 3: Demo Technical Difficulties

**Impact:** High | **Likelihood:** Low  
**Mitigation:**

- Rehearse demo 3x beforehand
- Pre-launch services with `dev-start.sh full --run`
- Backup recordings for each segment
- Test all URLs 24 hours before

### Risk 4: AI Ingestion Overpromised

**Impact:** Medium | **Likelihood:** Low  
**Mitigation:**

- Emphasize "AI-assisted" not "fully automated"
- Show prompt engineering (Module A2 templates)
- Demo refinement loop
- Set expectations: "LLM gives you 80%, you refine the 20%"

### Risk 5: Post-Sprint Momentum Loss

**Impact:** Medium | **Likelihood:** Medium  
**Mitigation:**

- Clear 4-week plan with owners
- Weekly check-ins on progress
- Ship incremental improvements (don't wait for perfect)
- Celebrate wins publicly (Slack, GitHub discussions)

---

## Decision Points

### Resolved

1. **Create marketecture document?** ✅ YES
   - Decision: `doc/MARKETECTURE.md` created
   - Rationale: Answers "what can I do with L4?" upfront
   - Impact: Clear value proposition for all stakeholders

2. **Move BREACH spec to done?** ✅ YES
   - Decision: Moved to `doc/dev/specs/done/`
   - Rationale: PR #726 fully implemented feature
   - Impact: Accurate status tracking

3. **Close issue #634?** ✅ YES
   - Decision: Closed with implementation details
   - Rationale: BREACH syntax fully working
   - Impact: Accurate issue tracking

4. **Close issue #629?** ✅ YES (user decision)
   - Decision: User closed directly
   - Note: ~80% done, release artifacts still needed

5. **Update issue #628?** ✅ YES
   - Decision: Added progress comment, kept open
   - Rationale: Infrastructure exists, automation missing
   - Impact: Tracks remaining work

### Pending

1. **Invest in chatbot UI (Module A10)?**
   - Current: Infrastructure complete, UI planned
   - Options:
     - A. Build now (pre-demo)
     - B. Build post-demo (Week 2-3)
     - C. Defer to Q1 2026
   - Recommend: **Option B** - Show architecture, build after onboarding fixed
   - Trade-off: Demo shows potential, post-work delivers polish

2. **Docker vs NixOS for deployment?**
   - Current: NixOS for prod, no Docker option
   - Options:
     - A. Add Docker Compose (broader adoption)
     - B. NixOS only (consistent with current)
     - C. Both (maintenance burden)
   - Recommend: **Option A** - Docker for dev/demo, NixOS for prod
   - Trade-off: More work but lowers barrier to entry

3. **Web-based ingestion tool?**
   - Current: Claude Code plugin (requires license)
   - Options:
     - A. Build web tool (paste PDF → extract L4)
     - B. Enhance docs for existing workflow
     - C. Wait for Module A2 completion
   - Recommend: **Option B** - Docs first, web tool if demand
   - Trade-off: Avoids premature optimization

4. **Template gallery curation?**
   - Current: Many examples in `jl4/experiments/`
   - Options:
     - A. Curate 5 best, add READMEs
     - B. Generate from Foundation Course examples
     - C. Community contributions via GitHub
   - Recommend: **Option A + B** - Curate 5, expand from course
   - Trade-off: Quality over quantity

---

## Stakeholder Communication

### For Leadership

**Message:** L4 is production-ready but needs polish for broad adoption

**Highlights:**

- Core language mature (9/10)
- Decision service APIs excellent (9/10)
- Consumer UX strong (8/10)
- Gaps: Onboarding friction, error messages, connective tissue

**Ask:** Support 4-week sprint for polish

### For Developers

**Message:** Contributing is easier than ever

**Highlights:**

- 48 open issues triaged by priority
- Clear roadmap with owners and timelines
- Foundation + Advanced courses (12,700 lines)
- Pattern library coming (Week 4)

**Ask:** Pick P1-P2 issues aligned with your interests

### For End-Users (Legal Professionals)

**Message:** L4 delivers value today, getting easier daily

**Highlights:**

- AI-assisted ingestion (Module A2)
- Auto-generated APIs and UIs
- Real-world impact (government, insurance, legislative)
- Video walkthroughs coming

**Ask:** Try Foundation Course, give feedback on pain points

### For Partners/Integrators

**Message:** Integration is straightforward and well-documented

**Highlights:**

- REST API with Swagger docs
- Batch processing for scale
- JSON schemas for validation
- Quick Integration Guide coming (Week 4)

**Ask:** Share integration patterns for others

---

## Appendix: Reference Links

### Documentation

- Marketecture: `doc/MARKETECTURE.md`
- Foundation Course: `doc/foundation-course-ai/`
- Advanced Course: `doc/advanced-course-ai/`
- Developer Setup: `doc/dev/setup.md`
- Deployment: `doc/dev/deployment/`

### Code Repositories

- Main Repo: https://github.com/smucclaw/l4-ide
- Issues: https://github.com/smucclaw/l4-ide/issues
- PRs: https://github.com/smucclaw/l4-ide/pulls

### Production Services

- Web IDE: https://jl4.legalese.com
- Decision Service: https://jl4.legalese.com/decision/swagger-ui/
- LSP WebSocket: wss://jl4.legalese.com/lsp

### Related Specs

- BREACH Implementation: `doc/dev/specs/done/PROHIBITION-BREACH-SPEC.md`
- LLM Integration: `doc/dev/specs/todo/LLM-INTEGRATION-SPEC.md`
- Query Planning: `doc/dev/specs/todo/SYMBEVAL-QUERY-PLANNING-STATUS.md`
- Web App Generator: `doc/dev/specs/todo/WEB-APP-GENERATOR-SPEC.md`

---

## Changelog

**2026-01-09:** Initial version based on PM/product strategy session

- Analyzed recent PRs (l4-wizard, prohibition, LLM, plugin)
- Triaged 48 open issues
- Created 4-week post-sprint roadmap
- Defined success metrics and risk mitigation

---

## Approval & Sign-Off

**Reviewed By:** [Name, Role, Date]  
**Approved By:** [Name, Role, Date]  
**Next Review:** 2026-02-13 (end of 4-week sprint)

---

**Questions? Feedback? Issues?**

- Open GitHub issue tagged `product-strategy`
- Discuss in #l4-roadmap Slack channel
- Email: strategy@legalese.com
