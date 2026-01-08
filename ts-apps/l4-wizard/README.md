# L4 Wizard

**Consumer-facing web application** that automatically generates interactive decision guidance interfaces from L4 specifications.

## Overview

L4 Wizard transforms L4 decision functions into user-friendly questionnaires with intelligent question ordering and visual feedback. It's designed for end-users who need to make decisions based on legal rules, not for developers writing L4 code.

## Features

- **Automatic UI Generation**: Provide a function name, get a complete decision interface
- **Query Planning**: Smart question ordering based on relevance analysis
- **Visual Feedback**: Questions become irrelevant and gray out as you answer
- **Interactive Ladder Diagram**: Clickable decision tree showing AND/OR/NOT logic
- **Real-time Evaluation**: See results update as you answer questions
- **Schema-driven Inputs**: Automatic input controls (boolean, number, text, enum)
- **GraphViz Traces**: View evaluation traces for debugging

## Quick Start

### Prerequisites

1. Decision service running on `http://localhost:8001` with L4 functions loaded
2. Node.js >= 20

### Running

```bash
# From web-app root
cd ts-apps/l4-wizard
npm install
npm run dev
```

Navigate to `http://localhost:5174/?fn=<function-name>` where `<function-name>` is the name of an L4 function exposed via `@export`.

**Example:**

```bash
# If you have decision service running with 04-alcohol-purchase.l4:
http://localhost:5174/?fn=may%20purchase%20alcohol
```

## Architecture

```
User Browser
    ↓
l4-wizard (Svelte 5 + TypeScript)
    ↓
Decision Service API (http://localhost:8001)
    ├─ GET /functions               (list available functions)
    ├─ GET /functions/{id}          (metadata: parameters, schema)
    ├─ POST /functions/{id}/query-plan (relevance analysis)
    └─ POST /functions/{id}/evaluation (compute results)
```

## URL Parameters

- `?fn=<function-name>` - Function to load and interact with
- Multiple functions can be specified (future enhancement)

## Development

**Tech Stack:**

- Svelte 5 (runes syntax)
- TypeScript
- Vite
- TailwindCSS
- `@repo/decision-service-types` (shared types)

**Project Structure:**

```
src/
  lib/
    components/
      Wizard.svelte           - Main orchestrator
      ParameterGrid.svelte    - Question cards
      ParameterCard.svelte    - Individual question
      LadderDiagram.svelte    - Interactive decision tree
      GraphvizDiagram.svelte  - Evaluation trace viewer
      OutcomeBanner.svelte    - Result display
      inputs/                 - Schema-driven input controls
    decision-service.ts       - API client
    ladder.ts                 - Ladder evaluation logic
    types.ts                  - Local type definitions
  routes/
    +page.svelte              - Entry point
```

## Query Planning

The wizard uses the decision service's `/query-plan` endpoint to:

1. **Determine next question**: Which parameter should be asked next?
2. **Detect irrelevance**: Which parameters don't matter given current answers?
3. **Short-circuit evaluation**: Stop asking questions when outcome is determined

This creates a dynamic, conversational flow rather than a static form.

## Ladder Diagram

The interactive ladder diagram visualizes the decision logic:

- **Green nodes**: Variable is true
- **Red nodes**: Variable is false
- **Gray nodes**: Variable is unknown
- **NOT badges**: Inline with their operands (unary operator)
- **Clickable**: Click nodes to cycle through values (undefined → true → false)

## Example Usage

### Alcohol Purchase Rules

Given this L4 function:

```l4
DECIDE `may purchase alcohol` IF
           you are 21+ years old
      AND     you are unmarried
           OR your spouse approved
           OR buying only beer
  OR       you are under 21
      AND  your parent approved
           OR you are legally emancipated
```

The wizard will:

1. Ask for age first (most discriminating question)
2. If 21+: gray out parental questions (irrelevant)
3. If buying beer: gray out marital questions (irrelevant)
4. Show live updates in the ladder diagram
5. Display final result when determined

## Configuration

The app connects to `http://localhost:8001` by default. This is configurable in the Wizard component's `serviceUrl` prop.

## Building for Production

```bash
npm run build
# Output: build/ directory with static files
# Serve with any static file server or nginx
```

## Related Documentation

- [WEB-APP-GENERATOR-SPEC.md](../../doc/dev/specs/todo/WEB-APP-GENERATOR-SPEC.md) - Full specification
- [SYMBEVAL-QUERY-PLANNING-STATUS.md](../../doc/dev/specs/todo/SYMBEVAL-QUERY-PLANNING-STATUS.md) - Query planning implementation
- [Decision Service API](../../jl4-decision-service/README.md)

## Differences from jl4-web

| Feature            | jl4-web (IDE)                  | l4-wizard                      |
| ------------------ | ------------------------------ | ------------------------------ |
| **Purpose**        | Write/edit L4 code             | Use L4 rules to make decisions |
| **User**           | Developers                     | End-users                      |
| **Input**          | L4 source code                 | Form answers                   |
| **Output**         | Saved programs, visualizations | Decision results               |
| **Editor**         | Monaco (LSP-enabled)           | None                           |
| **Websessions**    | Saves programs                 | Not used                       |
| **Query Planning** | Visualizes ladder              | Drives UX flow                 |
