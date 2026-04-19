# Legalese AI Tab — Implementation Plan

A new first-position sidebar tab in the VSCode extension that runs an agent
loop against `ai.legalese.cloud`. Tools execute locally (file ops, LSP,
local MCP servers including the built-in `l4-rules` one); the ai-proxy only
handles LLM round-trips (client-declared tool passthrough shipped in
ai-proxy 0429443).

Visuals inline with message flow, Continue/Claude Code style — not a
floating "tool card" panel. The existing ai-chat repo is a functional
reference only; the implementation and layout here are fresh.

---

## 1. Guiding principles

- **Strict process boundary.** Webview is pure UI, does no I/O. Extension
  host is the only side that touches disk, LSP, MCP, or the network.
- **Side-effecting tools are client-side.** File edits, LSP calls, MCP
  invocations, and anything that touches the user's machine run in the
  extension host — never on the proxy. Every side effect is visible
  to the user through the approval UI and the diff tab. (The proxy
  _does_ run its own stateless server-side tools, like `search_l4_docs`,
  for things that need baked-in L4-version context. Those have no side
  effects and surface as `tool_activity` events only.)
- **Visuals inline.** Tool calls appear in-line with the assistant message
  that spawned them — not in a separate panel.
- **OpenAI-compatible, nothing bespoke.** The wire protocol with the proxy
  is standard OpenAI chat completions with `tools` and `tool_calls`. No
  L4-specific RPC.

---

## 2. Architecture

```
┌───────────────────────────── Webview (Svelte 5) ──────────────────────────┐
│                                                                            │
│   ai-chat.svelte.ts (runes store)                                          │
│        │                                                                    │
│   chat.svelte ── message-list ── message.svelte                            │
│        │              │                │                                    │
│        │              │                ├── markdown-renderer (marked+shiki+katex)│
│        │              │                ├── thinking-block (collapsible)     │
│        │              │                ├── tool-activity-inline (dedup)     │
│        │              │                ├── file-tool-call (cmd+click diff)  │
│        │              │                └── rule-tool-call (expand input/output)│
│        │                                                                    │
│   chat-input.svelte ── attachments, new-conv / history / settings buttons  │
│   conversation-history.svelte (overlay)                                    │
│   settings-panel.svelte (permissions + MCP servers)                        │
│                                                                            │
│   extension-bridge.ts (postMessage wrapper)                                │
└─────────────────────────────────┬──────────────────────────────────────────┘
                                  │ postMessage (typed events)
┌─────────────────────────────────┴──────────────────────────────────────────┐
│                        Extension host (Node.js)                             │
│                                                                              │
│   chat-service.ts ────── ai-proxy HTTP client (SSE parser)                  │
│        │                                                                     │
│        ├── tool-registry.ts ── builtins + MCP tool lists → OpenAI `tools[]`│
│        ├── tool-dispatcher.ts ── routes tool_call by name prefix             │
│        │                                                                     │
│   fs-tools.ts (create / read / edit / delete)                                │
│   lsp-tool.ts (jl4 LSP diagnostics via stdio JSON-RPC)                       │
│   mcp-clients.ts (l4-rules localhost HTTP + user stdio/HTTP MCP servers)    │
│                                                                              │
│   permissions.ts (Never / Ask / Always gate, per tool category)             │
│                                                                              │
│   conversation-store.ts (JSON files in globalStorage)                       │
│   title-generator.ts (summize request)                                       │
│   diff-tabs.ts (vscode.diff + virtual TextDocumentContentProvider)          │
└──────────────────────────────────────────────────────────────────────────────┘
```

---

## 3. UI specification

### 3.1 Tab layout

Tab order: Legalese AI · Docs · Inspector · Deploy · Deployments. **Docs
remains the default tab on extension boot** — the AI tab is first in the
strip (most prominent) but not auto-selected.

```
┌─────────────────────────────────────────────┐
│ [Legalese AI] [Docs] [Inspector] [Deploy]  │  ← tabs (new tab first, Docs default)
├─────────────────────────────────────────────┤
│                                              │
│  ░░ message scroll region ░░                │
│                                              │
│  • User: "add a discount rule"              │
│  • Assistant: (thinking ▸)                  │
│  •          Server used search_l4_docs (2x) │
│  •          Edit contracts/discount.l4 ↗    │  ← cmd+click opens diff
│  •          Evaluating rule discount  [▸]   │
│  •          "I've added…"                   │
│                                              │
├─────────────────────────────────────────────┤
│ ┌─────────────────────────────────────────┐│
│ │ Ask anything about your L4…             ││  ← textarea, 1–6 rows
│ │                                          ││
│ │─────────────────────────────────────────││  ← thin separator
│ │ [＋] [⏱ history] [⚙]       [📎] [↑]   ││  ← action bar
│ └─────────────────────────────────────────┘│
│  Connected to Legalese Cloud · acme         │  ← existing footer status
└─────────────────────────────────────────────┘
```

**Action bar:**

- `＋` — new conversation (autosave current if dirty)
- `⏱` — toggle conversation history overlay
- `⚙` — open AI chat settings overlay
- `📎` — file picker (text + PDF); chips inside the textarea's lower margin
- `↑` — submit (Enter also submits, Shift+Enter for newline)

**The divider doubles as a token usage gauge.** The thin separator line
between the textarea and the action bar starts light gray. As the org's
daily token usage grows, the line fills from left→right with a slightly
brighter fill. Width proportion = `usedToday / dailyTokenLimit`. When
the ratio passes ~0.9 the fill shades toward amber; past 1.0 (which
means the next request will 429 if `blockOnOverage` is on) it shades
red. No text, no percentage — quiet ambient signal.

**Polling.** While the AI tab is the active tab AND has an open
conversation, the extension polls an existing Legalese Cloud endpoint
(`/service/health` already returns `blockOnOverage`; extend it or pick
the existing endpoint carrying `ai.dailyTokenLimit` + today's tally) on
a 30s interval. No polling when the tab is not visible or when there's
no conversation loaded. Each successful chat completion also refreshes
the counter immediately (piggybacking on the `metadata` event's usage
if we surface it, or with a single ad-hoc fetch post-turn).

### 3.1a `@` mentions in the input

Typing `@` in the textarea opens an autocomplete popup over workspace
context:

- `@foo.l4` — files (fuzzy-matched, workspace-rooted)
- `@ruleName` — L4 symbols from the workspace-exports index (same
  source the bootstrap system message uses)
- `@selection` — whatever is currently selected in the active editor

Accepted mentions render as chips inline in the textarea. On submit, the
extension expands each chip into the right content-injection form:

- File mention → file contents added as a text block in the user message
  with a `<file path="foo.l4">…</file>` wrapper so the model can cite.
- Symbol mention → just the export declaration (not the whole defining
  file), which keeps token cost low.
- `@selection` → the selected text with its filename + line range.

This is table-stakes for modern chat-coding UX. Without it every
context hand-off is a tool-call round-trip. With it, the user guides
attention in one gesture.

### 3.1b Empty state (authenticated, no conversation)

When the user opens the tab signed-in but without a loaded conversation,
the message region shows a centered "Get started" block with three
outlined buttons (gray-on-gray, matching the sidebar's existing quiet
style). Each button seeds a first turn — no prompt engineering required
from the user.

```
Get started

┌───────────────────────────────────────────────────────────┐
│  Turn my policy document into L4 for use as MCP           │
└───────────────────────────────────────────────────────────┘
┌───────────────────────────────────────────────────────────┐
│  Convert my spreadsheet into REST APIs I can use as       │
│  webhooks                                                  │
└───────────────────────────────────────────────────────────┘
┌───────────────────────────────────────────────────────────┐
│  Help me convert a legal text to L4 and create example    │
│  scenarios challenging the rules                           │
└───────────────────────────────────────────────────────────┘
```

Click behaviors:

| Button                                                                                | Action                                                                                                                                                                                                                                                                                |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Turn my policy document into L4 for use as MCP                                        | Opens an OS file picker (text / PDF). Selected file becomes an attachment. Textarea pre-fills: _"Turn this policy document into precise and easily auditable L4 rules for use as an MCP tool with the goal to allow an AI agent to check compliance."_ User can edit or press submit. |
| Convert my spreadsheet into REST APIs I can use as webhooks                           | Opens file picker (`.csv`, `.xlsx`, `.tsv`). Textarea pre-fills: _"Convert this spreadsheet into REST API endpoints I can use as webhooks."_                                                                                                                                          |
| Help me convert a legal text to L4 and create example scenarios challenging the rules | No file picker. Textarea pre-fills: _"Help me convert a legal text to L4 and create example scenarios challenging the rules."_ — the user then pastes or `@`-mentions the text.                                                                                                       |

The buttons disappear as soon as the user starts typing or selects a
conversation from history — they're only for the zero-state.

### 3.2 Message variants

| Role / kind               | Visual                                                                                                                                                                                                                              |
| ------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| User                      | Right-aligned, muted background, attachment + mention chips.                                                                                                                                                                        |
| Assistant text            | Left-aligned, full-width, markdown-rendered.                                                                                                                                                                                        |
| Thinking                  | Italic "Thinking…" row; click to expand. Collapsed by default. (Phase 3)                                                                                                                                                            |
| Server `tool_activity`    | One-liner: `🔍 Searching documentation for "DECIDE"` → `✓ Found 3 doc sections`. Dedupe by message string within a round.                                                                                                           |
| `fs__create` / `fs__edit` | `📝 Edit contracts/discount.l4` — filename `cmd+click` → `vscode.diff` between on-disk and proposed content.                                                                                                                        |
| `fs__read`                | `👁 Read contracts/discount.l4`. No diff.                                                                                                                                                                                           |
| `fs__delete`              | `🗑 Delete contracts/discount.l4`. No diff.                                                                                                                                                                                         |
| `lsp__diagnostics`        | `🩺 Check diagnostics in foo.l4 — 2 errors`. Click expands to the diagnostic list.                                                                                                                                                  |
| `l4__evaluate`            | `🧪 Evaluate discount(age:40, income:50000)` + chevron. Expanded: inputs JSON + result + trace (Inspector value renderer).                                                                                                          |
| `meta__ask_user`          | Distinct question card above the in-progress assistant text: "The rule says 'within a reasonable time' — configurable or default to 30 days?" with input box (or option buttons if `options` supplied). Answering resumes the turn. |
| `l4-rules__*`             | `⚙ Evaluating rule discount` + chevron. Expanded: inputs JSON tree + output (Inspector value renderer).                                                                                                                            |
| Generic MCP               | `🔌 {server}/{tool}` with generic JSON input/output expand.                                                                                                                                                                         |
| Approval pending          | Inline: `Allow Edit contracts/discount.l4? [Allow] [Deny] [Always allow edits]`                                                                                                                                                     |

All tool rows sit inline with the assistant turn they belong to — no
floating cards.

### 3.3 Conversation history

Overlay over the message region (not modal). Rows grouped by date:
auto-title · relative time · delete icon. Click to load.

**Concurrent conversations are architecturally supported.** The state
store keys streams by `conversationId` so two turns on different
conversations can run simultaneously. The user can switch away from a
streaming conversation — events for the background one continue to
accumulate in the store; when they switch back, they see the up-to-date
state. The currently-streaming conversation is marked with a small
spinner in the history list.

### 3.4 Unauthenticated state

If the user isn't signed in to Legalese Cloud, the tab still renders but
the textarea + action bar are disabled and the chat area shows a single
centered CTA:

> **Sign in to Legalese Cloud to start composing rules with AI.**

The sign-in button triggers the existing flow at
[auth.ts:141](src/auth.ts#L141). Once the session arrives via the
`l4-vscode://` URI callback, the AI tab subscribes to the auth state and
enables input automatically — no tab reload needed.

### 3.5 Settings panel

Overlay. Two sections:

```
Tool permissions
  Create files          [ Never | Ask | Always ]
  Read files            [ Never | Ask | Always ]
  Edit files            [ Never | Ask | Always ]
  Delete files          [ Never | Ask | Always ]
  Evaluate L4 (LSP)     [ Never | Ask | Always ]
  Run deployed rules    [ Never | Ask | Always ]

MCP servers
  ▸ l4-rules  (built-in, localhost HTTP)           [Always ▾]
  ▸ github    (stdio: npx -y @mcp/server-github)   [Ask ▾]   [✏️] [🗑]
  [+ Add MCP server]
```

Settings write through to VSCode configuration (`legaleseAi.*`) so they're
scriptable and sync via VSCode settings sync.

---

## 4. Webview ↔ extension protocol

Typed discriminated union over `postMessage`.

**Webview → extension:**

```ts
type ChatRequest =
  | {
      kind: 'chat/start'
      conversationId?: string
      text: string
      attachments: Attachment[]
      mentions: Mention[]
    }
  | { kind: 'chat/abort'; conversationId: string }
  | {
      kind: 'chat/approveTool'
      callId: string
      decision: 'allow' | 'deny' | 'alwaysAllow'
    }
  | { kind: 'chat/answerQuestion'; callId: string; answer: string } // meta__ask_user
  | { kind: 'conversation/list' }
  | { kind: 'conversation/load'; id: string }
  | { kind: 'conversation/delete'; id: string }
  | { kind: 'conversation/new' }
  | { kind: 'mention/search'; query: string } // @-mention autocomplete
  | { kind: 'settings/get' }
  | { kind: 'settings/update'; patch: Partial<AiSettings> }
  | { kind: 'file/openDiff'; path: string; proposedContent: string }
  | { kind: 'usage/subscribe' }
  | { kind: 'usage/unsubscribe' } // poll only when tab visible
```

**Extension → webview (streamed):**

```ts
type ChatEvent =
  | { kind: 'chat/started'; conversationId: string; model: string }
  | { kind: 'chat/textDelta'; conversationId: string; text: string }
  | { kind: 'chat/thinkingDelta'; conversationId: string; text: string }
  | {
      kind: 'chat/toolActivity'
      conversationId: string
      tool: string
      status: 'running' | 'done' | 'error'
      message: string
    }
  | {
      kind: 'chat/toolCall'
      conversationId: string
      callId: string
      name: string
      args: unknown
      status: 'pending-approval' | 'running' | 'done' | 'error'
      result?: unknown
      errorMessage?: string
    }
  | {
      kind: 'chat/done'
      conversationId: string
      finishReason: 'stop' | 'tool_calls' | 'length' | 'error'
      usage?: { promptTokens: number; completionTokens: number }
    }
  | { kind: 'chat/error'; conversationId: string; message: string }
  | { kind: 'conversation/list'; items: ConversationSummary[] }
  | { kind: 'conversation/loaded'; conversation: Conversation }
  | { kind: 'mention/results'; query: string; items: MentionCandidate[] }
  | {
      kind: 'usage/update'
      used: number
      limit: number
      blockOnOverage: boolean
    }
  | { kind: 'settings/value'; settings: AiSettings }
```

Each event carries a `conversationId` so the webview can ignore stale
events after abort/switch.

---

## 5. Tool system

### 5.1 Naming convention

All client-side tool names use the double-underscore namespace separator,
matching MCP convention. No inconsistency between builtin and MCP tools.

- Built-in categories: `fs__*`, `lsp__*`, `l4__*`, `meta__*`
- MCP tools: `{serverId}__{toolName}` (e.g. `l4-rules__calculate-tax`)

The dispatcher splits on the first `__` to route calls.

### 5.2 Built-in tools

| Tool               | Args                   | Behavior                                                                                                                                                                                                                                |
| ------------------ | ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `fs__read_file`    | `{path}`               | UTF-8 file contents.                                                                                                                                                                                                                    |
| `fs__create_file`  | `{path, content}`      | Creates file; fails if exists.                                                                                                                                                                                                          |
| `fs__edit_file`    | `{path, old, new}`     | **String-anchored find/replace.** Replaces the first (and must be unique) occurrence of `old` with `new`. Fails loudly if `old` is not found or matches multiple spots. Model calls it once per edit.                                   |
| `fs__delete_file`  | `{path}`               | Deletes file.                                                                                                                                                                                                                           |
| `lsp__diagnostics` | `{path}`               | didOpen + pull-diagnostics against jl4 LSP; returns diagnostics.                                                                                                                                                                        |
| `l4__evaluate`     | `{ruleName, inputs}`   | Evaluates a rule on supplied inputs via the LSP's evaluation endpoint; returns `{result, trace}`.                                                                                                                                       |
| `meta__ask_user`   | `{question, options?}` | **Pauses the turn** and surfaces the question to the user as a distinct UI affordance. The user's answer flows back as the tool result on the follow-up turn. Crucial for long autonomous loops where the model hits genuine ambiguity. |

String-anchored edits — not line ranges — because the model's view of the
file drifts over multiple rounds (prior edits, user edits in the real
editor). Line numbers rot; unique text anchors fail fast and loudly.
Pattern used by Cursor, Claude Code, Aider; worth matching.

All paths resolve against workspace root; absolute paths outside the
workspace are rejected. The `fs__` / `lsp__` / `l4__` / `meta__` prefixes
stay safely clear of any future server-side tool (ai-proxy's
`tool_name_disallowed` guard catches collisions anyway).

### 5.3 MCP tools

Use [`@modelcontextprotocol/sdk`](https://github.com/modelcontextprotocol/typescript-sdk)
for all MCP client work — don't roll our own stdio/HTTP handshake.

- **`l4-rules`** — built-in; the existing
  [mcp-proxy.ts](src/mcp-proxy.ts) serves it at
  `http://127.0.0.1:{port}/mcp`. The extension connects as an MCP
  _client_ (yes, into its own process) so the permission model and
  dispatcher treat it identically to any third-party server.
- **User-configured** — stdio (`command`, `args`, `env`) or HTTP from
  `legaleseAi.mcpServers` settings.

### 5.4 Permission model

Every tool call passes through `permissions.check(category, context)`:

- **Never** — synthetic result `{error: "User denied: {category}"}` fed
  back to the model. Nothing runs.
- **Always** — run immediately.
- **Ask** — emit `toolCall` event with `status: "pending-approval"`.
  Inline approval UI. User picks Allow / Deny / Always allow. "Always
  allow" bumps the setting to Always.

**Braver defaults to avoid approval fatigue.** If the user asked the AI
to modify the workspace, stopping for a confirmation on every `fs__edit`
is friction theater — they'll flip everything to "always" in the first
session. Permission gates have value for blocking _unexpected_ actions,
not redoing work the user already asked for. The turn-end review card
(§8.6) provides visibility; it's not a blind trust model.

| Category           | Default | Rationale                                                                                                              |
| ------------------ | ------- | ---------------------------------------------------------------------------------------------------------------------- |
| `fs.read`          | Always  | Read-only.                                                                                                             |
| `fs.create`        | Always  | Reviewable in the turn-end card + diff.                                                                                |
| `fs.edit`          | Always  | Same.                                                                                                                  |
| `fs.delete`        | **Ask** | Genuinely destructive; worth a confirmation.                                                                           |
| `lsp.evaluate`     | Always  | Pure, no side effects.                                                                                                 |
| `l4.evaluate`      | Always  | Pure.                                                                                                                  |
| `mcp.l4Rules`      | Always  | Pure reads of deployed rules.                                                                                          |
| `mcp.<new-server>` | **Ask** | Newly-added third-party MCP; user hasn't vetted it yet. Becomes "always" after first accept unless user flips it back. |
| `meta.ask_user`    | Always  | It's literally asking the user — gating it makes no sense.                                                             |

Persisted in VSCode settings (survives sessions, syncs via settings
sync).

### 5.5 Client-tool parallelism

The proxy may return multiple `tool_calls` in one round. Dispatch all in
parallel (respecting permissions), collect results, POST the batch back
in one follow-up. Partial denials are fine — LLMs adapt.

---

## 6. Rendering details

### 6.1 Markdown

- `marked` + custom renderer that pipes ` ```l4 ` blocks through the
  existing `@repo/l4-highlight` package (already used by the Inspector
  and Docs tabs — see [inspector-panel.svelte:13](../webview/src/lib/components/inspector-panel.svelte#L13)).
  Non-L4 code blocks fall through to Shiki with its default grammars.
- `marked-katex-extension` for `$…$` and `$$…$$`.
- Sanitizer: no raw HTML, no `javascript:` URIs (model output is untrusted).
- Tables via GFM.

**Streaming strategy — render block-by-block, not token-by-token.**
Re-parsing the whole message on every token delta flickers (partial code
fences, half-formed lists, unclosed tables). Instead:

1. As tokens arrive, accumulate into a buffer.
2. Whenever the buffer contains a **closed** block-level markdown
   construct (a paragraph ending with `\n\n`, a fenced code block with
   its closing ```, a complete list item, a table row, etc.), cut it
   off, parse + render it into a committed DOM element, and drop it
   from the buffer.
3. Render the remaining (in-flight) buffer as plain text with a
   blinking cursor at the end.
4. Committed blocks never re-render. Only the trailing text node
   changes as tokens arrive.

This matches how the best chat tools avoid flicker. Implementation is
maybe 100 lines of `marked` lexer plumbing.

### 6.2 Thinking blocks

Requires an **ai-proxy addition** (not yet shipped): pass Claude extended
thinking through as a custom SSE event (`event: thinking_delta`). On the
webview side, thinking is collapsed by default; expanding shows the
reasoning text. Non-reasoning providers simply omit the event.

### 6.3 Tool-activity dedupe

Server `tool_activity` events coalesce per turn, keyed by the exact
`message` string. Same message → update existing row's status.
Distinct message → new row.

### 6.4 File tool diff

For `fs_create_file` / `fs_edit_file`, the webview holds the proposed
content. `cmd+click` on the filename:

1. Webview emits `file/openDiff { path, proposedContent }`.
2. Extension registers a `TextDocumentContentProvider` under scheme
   `l4-ai-proposed:`, returning the proposed content for that path.
3. Extension runs
   `vscode.commands.executeCommand("vscode.diff", currentUri, proposedUri, "Legalese AI — foo.l4")`.

After approval and apply, the row flips to `✓ Edited foo.l4`; cmd+click
still opens the same diff (now current == proposed). Fine — useful for
retrospective review.

### 6.5 Rule tool expand

Name stripped of `l4-rules__` prefix and pretty-printed. Expanded view:

- **Inputs** — JSON tree matching the nested-properties pattern at
  [tool-card.svelte:48-86](../webview/src/lib/components/tool-card.svelte#L48-L86).
- **Output** — reuse the Inspector's "Track result" value renderer
  (exact component name TBD — verify before implementing).

### 6.6 Virtualization

Skip for v1. If users report slowdowns on long chats, add
`svelte-virtual-list` later.

---

## 7. Storage

### 7.1 Conversations

Local JSON files at `{context.globalStorageUri}/ai/conversations/{id}.json`.

**Schema:** thin envelope around the standard OpenAI chat message
format. No custom versioning — OpenAI's message shape is the de-facto
standard (OpenAI, AI SDK, Anthropic, every major provider maintains
compatibility) and it evolves additively. We don't need our own version
number; if OpenAI adds a field, we ignore it on read and carry it on
write.

```json
{
  "id": "conv_...",
  "orgId": "acme",
  "userId": "user_...",
  "model": "legalese-compose-4",
  "title": "Add discount rule",
  "createdAt": "...",
  "lastActiveAt": "...",
  "messages": [
    /* standard OpenAI chat messages: role, content, tool_calls,
       tool_call_id, name — with our UI-specific metadata on
       assistant messages under a `_meta` field the server ignores
       (pending-approval flags, review-card summaries, thinking text) */
  ]
}
```

Server owns the full state via `conversationId` and handles compaction
(sliding window + summary) for us — the local file is the render log for
UI restoration, not the LLM context. On resume we send only the new user
turn (delta-mode, auto-detected). If the server returns 404 (TTL
expired), start a new conversation and replay local history as messages.

Because the server already compacts, we don't trim the local file. It
stays unbounded and the history panel paginates on display.

**Local tracking is the only way to reopen a conversation in Phase 1.**
The history overlay reads this directory — conversations not tracked
locally (lost `globalStorageUri`, different machine) can't be reopened,
even though they still exist server-side. A later "list my
conversations" API over the server's storage would fix this, but isn't
v1. Users swapping machines keep a bare awareness of this limitation.

**Deletion renames the file out of the way; no `unlink`, no restore
UI.** Locally: `{id}.json` → `{id}.deleted-{ts}.json`. Server-side:
`DELETE /v1/conversations/{id}` does the same rename on EFS. A future
cleanup job (not v1) reaps renamed files past some TTL. From the user's
perspective it's gone; no "recently deleted" panel. The rename is just
for operational safety — accidental drops stay recoverable from disk
until the cleanup reaps them.

### 7.2 Title generation

On first assistant `chat/done`, fire-and-forget:

```json
POST /v1/chat/completions
{
  "model": "legalese-summize-4",
  "messages": [{
    "role": "user",
    "content": "Generate a 4-6 word title for a chat that begins: \"{first_user_message}\". Respond with just the title, no quotes."
  }]
}
```

Write the result back to the conversation file. On failure, fall back to
a truncated form of the first user message.

### 7.3 Settings

All under `legaleseAi.*` in VSCode configuration. package.json contributions:

```jsonc
{
  "legaleseAi.permissions.readFiles": {
    "enum": ["never", "ask", "always"],
    "default": "always",
  },
  "legaleseAi.permissions.createFiles": { "default": "always" },
  "legaleseAi.permissions.editFiles": { "default": "always" },
  "legaleseAi.permissions.deleteFiles": { "default": "ask" },
  "legaleseAi.permissions.evaluateL4": { "default": "always" },
  "legaleseAi.permissions.evaluateRule": { "default": "always" },
  "legaleseAi.permissions.runDeployedRules": { "default": "always" },
  "legaleseAi.mcpServers": {
    "type": "object",
    "additionalProperties": { "type": "object" },
  },
}
```

Webview reads via `settings/get`, writes via `settings/update`. Extension
persists with `ConfigurationTarget.Global`.

---

## 8. Editor & workspace context

A general chat tab is not enough for policy→L4 drafting. The AI needs to
know _what the user is looking at_ and _what already exists in the
project_ on every turn, or the user pays the tax of re-explaining.

### 8.1 Editor context injected per turn

Before POSTing `/v1/chat/completions`, the extension collects a
`system`-role message:

```
<editor-context>
activeFile: contracts/discount.l4
cursorLine: 42
selection: |
  DECIDE x IS eligible IF age > 18
openFiles:
  - contracts/discount.l4
  - contracts/common.l4
</editor-context>
```

**Important: prompt-cache ordering.** The ai-proxy's compose pipeline
relies on provider-side caching of the 22k L4 system prompt prefix for
cost — a ~90% discount on cached input tokens that evaporates if any
variable content sits in front of it. The extension therefore sends
editor/workspace context as **an additional system message** in
`body.messages`, which the proxy appends AFTER its cached prefix. Never
stuff dynamic content into the first system slot or into the body's
`system` field.

The proxy already handles additional system messages transparently — no
proxy change needed. We just need to be disciplined about ordering.

### 8.2 Workspace bootstrap

On conversation start, prepend a second `system` block listing the
project's L4 exports, their types, and deployed-rule status (reuse what
the Deployments tab already surfaces). Think of it as a `ls -l` for the
project's public interface:

```
<workspace-exports>
- eligible(age: Integer, income: Money) -> Boolean  [@export, deployed]
- computeDiscount(order: Order) -> Money             [@export]
- isResident(person: Person) -> Boolean              [local]
</workspace-exports>
```

Stops the AI from re-deriving what's already been defined, and stops it
inventing function names that don't exist.

### 8.3 Source citation discipline

The system prompt (ideally a drafting-specialized variant; see §13 Q8)
instructs the model to annotate every generated rule with a `@desc`
citing the source clause:

```l4
@desc "Section 5(a) of the Consumer Protection Act 2008"
@export
GIVEN (age IS A Integer)
DECIDE isEligible IF age > 18
```

A follow-up feature (v2): two-way navigation. Click an `@desc` in the L4
file → locate the cited clause in the source document pane. Click a
clause in the source document → jump to the L4 rule citing it. This is
the feature that turns the tool from "AI coding assistant" into "policy
encoding platform." Not v1, but the citation habit must start v1 so the
navigation layer has data to work with.

### 8.4 Test-case evaluation tool

`lsp_diagnostics` catches _compile_ errors. It does not catch _semantic_
errors — the rule compiles, returns `true` for a case that should be
`false`. For a drafting copilot that's inadequate.

Add a client-side tool:

| Tool          | Args                         | Behavior                                                                                                                         |
| ------------- | ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| `l4_evaluate` | `{ruleName, inputs: object}` | Evaluates a rule on supplied inputs via the LSP's evaluation endpoint (same path the Inspector uses); returns `{result, trace}`. |

The model uses this to:

1. Derive test cases from the source text ("if a 40-year-old with $50k
   income should qualify, verify the rule returns `true` for those
   inputs").
2. Run them.
3. If any fail, iterate on the rule.

Permission category: `l4.evaluate` (default: Always — pure, no side effects).

### 8.5 Code-action entry points

Right-click in the editor on a selection → "Ask Legalese AI about this"
command opens the AI tab with the selected text pre-quoted as the first
user message. Without this the tab is an island.

Bonus: code-lens above every `@export`-annotated function: "Explain" ·
"Test" · "Refactor" — each prefills the chat with a relevant prompt.

### 8.6 Turn-end review card

After the AI completes a turn that touched ≥2 files, render a review
card at the bottom of the assistant message:

```
Changes in this turn:
  • contracts/discount.l4 (edited, +12 / -3)
  • tests/discount-spec.l4 (created, +24)
  [Show combined diff]
```

"Show combined diff" opens a multi-file diff view. No in-app revert
button in v1 — the user reverts via git, which matches the "no rollback"
stance on abort semantics (see §13). The review card is pure display.

---

## 9. Attachments

Simplest path: pass files straight through as Anthropic-native content
blocks. No OCR, no text extraction.

- **Text files** (`.l4`, `.txt`, `.md`, etc.): read as UTF-8, embed as a
  `text` block prefixed with the filename.
- **PDFs**: base64 + Anthropic `document` block. Claude reads both the
  text layer and the rendered pages (handles scanned PDFs via its vision
  capability). Native; no OCR dep on our side.
- **Word (`.docx`)**: Anthropic doesn't natively accept `.docx`. Two
  options, pick the simpler one when we get there:
  1. Convert to PDF in the extension (e.g. via LibreOffice headless) —
     adds a heavy dep.
  2. Show a UX nudge: "Word files aren't supported directly — save as
     PDF and attach that instead." My recommendation: (2) for v1.

**Requires ai-proxy change** — currently only `content: string` is
accepted on chat messages; need array-content with text + document parts
passed through to the provider. See §10.

Permissions don't apply to attachments (user explicitly attached).

---

## 10. Required ai-proxy changes (post the client-tool commit)

1. **Remove `maxToolRounds` cap on compose.** The daily token quota is
   the only safety valve the agent loop needs. Keep the field in the
   `Pipeline` type (summize uses 0); stop decrementing in the compose
   loop → replace the bounded `for` with an open `while` that exits on
   "model called no tools."
2. **Multi-modal content on user messages.** Accept array content with
   text + Anthropic `document` parts; pass through to the provider.
   Required for PDF attachments.
3. **Thinking pass-through** — new `event: thinking_delta` SSE frame
   streamed when the provider emits reasoning parts.
4. **Conversation deletion by rename** — `DELETE /v1/conversations/{id}`
   renames `{id}.json` → `{id}.deleted-{ts}.json` on EFS. No `unlink`,
   no restore UI. A later cleanup job reaps renamed files past TTL.
5. **Record creator user ID; store per-user.** Today:
   `/efs/conversations/{org}/{conversationId}.json`. New layout:
   `/efs/conversations/{org}/{userId}/{conversationId}.json`. Pulls the
   `userId` from the sealed session (`wos_*`) or falls back to a
   reserved `api-key-{keyId}` subfolder for `sk_*` callers. Enables
   future "list my conversations" API per user without scanning
   org-wide, and records provenance. The `userId` also lands inside the
   conversation file for resilience against future path changes.

Phase mapping: (1) + (4) + (5) in Phase 1. (2) + (3) in Phase 3.

---

## 11. File / module layout

### Webview (`ts-apps/webview/`)

```
src/lib/components/ai/
  chat.svelte                 # top-level container
  chat-input.svelte           # textarea + action bar + @-mention + usage-line
  empty-state.svelte          # "Get started" with 3 seed-prompt buttons
  error-bubble.svelte         # inline error rendering
  submit-or-stop.svelte       # morphs ↑ ↔ ■ during streaming
  mention-popup.svelte        # autocomplete over files + exports + selection
  usage-line.svelte           # the gradient divider gauge
  conversation-history.svelte
  settings-panel.svelte
  message-list.svelte         # sticky-to-bottom scroll + jump-to-latest
  message-user.svelte
  message-assistant.svelte    # block-by-block streaming markdown
  streaming-markdown.svelte   # the incremental renderer
  thinking-block.svelte       # Phase 3
  tool-activity-row.svelte
  tool-call-file.svelte       # fs__create / fs__edit / fs__delete / fs__read
  tool-call-lsp.svelte        # lsp__diagnostics
  tool-call-l4-eval.svelte    # l4__evaluate
  tool-call-ask-user.svelte   # meta__ask_user — renders the question card
  tool-call-rule.svelte       # l4-rules__*
  tool-call-generic.svelte    # fallback for unknown MCP tools
  tool-approval.svelte
  review-card.svelte          # turn-end multi-file summary
  copy-button.svelte          # hover-reveal on messages + code blocks
  markdown.svelte             # marked + @repo/l4-highlight + shiki + katex
  json-tree.svelte

src/lib/stores/
  ai-chat.svelte.ts           # per-conversation-id keyed stream state
  ai-auth.svelte.ts           # signed-in flag, session expiry handling
  ai-drafts.svelte.ts         # per-conversation textarea persistence
  ai-settings.svelte.ts
  ai-usage.svelte.ts          # 30s poller for token usage

src/lib/ai/
  extension-bridge.ts
  stream-types.ts
  incremental-markdown.ts     # block-boundary lexer + render helper
```

### Extension host (`ts-apps/vscode/src/`)

```
ai/
  chat-service.ts
  ai-proxy-client.ts          # SSE parser
  editor-context.ts           # per-turn <editor-context> builder
  workspace-bootstrap.ts      # first-turn <workspace-exports> builder
  tool-registry.ts            # merges builtins + MCP into OpenAI tools[]
  tool-dispatcher.ts          # routes tool_call by `__`-prefix
  permissions.ts              # Never/Ask/Always + settings IO
  conversation-store.ts       # JSON files, rename-on-delete
  title-generator.ts          # summize request
  diff-tabs.ts                # virtual scheme + vscode.diff
  attachments.ts              # text + PDF → multimodal parts (Phase 3)
  usage-poller.ts             # 30s /health fetch when tab visible
  code-actions.ts             # right-click "Ask Legalese AI"
  webview-bridge.ts           # event buffer for hidden webviews; auth-status
  logger.ts                   # Legalese AI output channel
  tools/
    fs.ts                     # string-anchored edit
    lsp.ts                    # lsp__diagnostics
    l4-evaluate.ts            # l4__evaluate
    ask-user.ts               # meta__ask_user
  mcp/
    client-manager.ts         # reads legaleseAi.mcpServers, wraps MCP SDK
    l4-rules-client.ts        # connect to existing localhost MCP server
```

---

## 12. Phasing

Three shippable milestones. Each phase leaves the tab in a usable state.

### Phase 1 — Chat foundation (~8 days)

**Goal:** the tab exists, authenticated users can have streaming
conversations with the ai-proxy, editor/workspace context flows in, the
token gauge tells them how much runway they have, and runaway turns can
be stopped cleanly.

**Webview**

- First-position tab registered in the sidebar; Docs stays as default on
  boot.
- Unauthenticated state: disabled input + CTA "Sign in to Legalese Cloud
  to start composing rules with AI." triggering [auth.ts:141](src/auth.ts#L141).
- Empty state (authed, no conversation): "Get started" block with three
  outlined buttons that seed the first turn (§3.1b).
- Chat container, message list, user + assistant message components.
- Chat input box with textarea, attachment button stub (Phase 3), action
  bar (`＋`, `⏱`, `⚙`, `↑`), `@` mention autocomplete popup wired to
  files + workspace-export symbols + `@selection`.
- **Abort button** — `↑` submit button morphs into `■` stop while
  streaming; click sends `chat/abort`.
- **Error rendering** — `error-bubble.svelte` renders inline on:
  HTTP 401 (prompt re-sign-in, trigger auth flow), 429 (show quota
  message), network drops mid-stream (show partial message + "interrupted"
  marker), generic 5xx (terse message + retry button).
- **Streaming markdown (block-by-block)** — committed block-level
  elements never re-render; trailing in-flight buffer renders as plain
  text with blinking cursor until its enclosing block closes (§6.1).
- **Scroll behavior** — sticky-to-bottom during streaming when user is
  near bottom; releases when user scrolls up; "Jump to latest" button
  appears when scrolled away.
- **Copy buttons** — hover-reveal Copy on assistant messages and
  individual code blocks.
- **Draft persistence** — input textarea content persists per
  conversation across VSCode reloads (keyed in webview state).
- Usage-line divider gauge (light gray → brighter fill → amber past
  0.9 → red past 1.0) polling the existing Legalese Cloud health
  endpoint every 30s while tab is visible and a conversation is loaded.
- Conversation history overlay with subtle per-row spinner for currently
  streaming conversations.
- Settings panel button deferred (Phase 2 introduces content; hiding it
  in Phase 1 avoids a dead click).
- Markdown rendering via `@repo/l4-highlight` for L4 code blocks, Shiki
  for everything else.
- Concurrent-conversation-keyed store — streams from multiple
  conversations coexist; switching tabs or hiding the webview doesn't
  cancel, the extension host buffers events and replays on webview
  re-show.
- Title generation via summize call after first assistant turn.

**Extension host**

- `chat-service.ts` + `ai-proxy-client.ts` SSE parser.
- `conversation-store.ts` (JSON in globalStorage, standard OpenAI
  message shape with thin envelope, delete-by-rename).
- `title-generator.ts` (single summize call, non-blocking).
- `editor-context.ts` — produces the per-turn `<editor-context>` system
  message.
- `workspace-bootstrap.ts` — produces the first-turn `<workspace-exports>`
  system message from the existing exports index. Gracefully handles
  empty workspace / no-L4-files (emits the section with an explicit
  "(no L4 exports defined yet)" marker so the model knows).
- Both context messages sent AFTER the ai-proxy's cached prefix (as
  extra system messages in `body.messages`, not prepended).
- `@` mention expansion on send (chip → real content).
- `auth-status` event to webview so it can enable/disable input and
  detect session expiry mid-chat.
- Webview-hide/show buffer: events for a backgrounded webview accumulate
  in the host, replay as a batch on `WebviewView.onDidChangeVisibility`.
- Output-channel logging (`Legalese AI` channel) — request sent, chunks
  received, errors. First line of defense when users report issues.

**Ai-proxy**

- Remove `maxToolRounds` cap on compose (§10.1).
- Add `DELETE /v1/conversations/{id}` rename-delete endpoint (§10.4).
- Per-user subfolder layout + user ID in the conversation JSON (§10.5).

**Deliberately out of scope in Phase 1:** tools, approvals, attachments,
thinking, math, MCP, settings panel content. Without tools it's a pure
chat; still useful for Q&A, rule ideation, and regulatory discussion.

---

### Phase 2 — Agent tooling (~6 days)

**Goal:** the AI can read, draft, edit, evaluate, and ask — autonomously
when permissions allow. MCP tools from the existing l4-rules server and
third-party configured servers become available.

**Tools** (all names `category__name`)

- `fs__read_file`, `fs__create_file`, `fs__edit_file` (string-anchored
  find/replace), `fs__delete_file`.
- `lsp__diagnostics`.
- `l4__evaluate` (execute a rule on inputs; return result + trace).
- `meta__ask_user` (pauses the turn, surfaces structured question).
- MCP integration via `@modelcontextprotocol/sdk`:
  - `l4-rules` as a built-in MCP server.
  - Third-party servers from `legaleseAi.mcpServers` setting.

**Permissions** — braver defaults per §5.4. Settings panel permissions
section becomes interactive. Inline approval UI banner; "Always allow"
option bumps the setting.

**Visuals**

- Tool-activity dedupe row for server-side `tool_activity` events.
- File tool rows (`fs__create`, `fs__edit`) with `cmd+click` → diff tab
  via virtual `l4-ai-proposed:` scheme.
- `lsp__diagnostics` row, click to expand diagnostic list.
- `l4__evaluate` row, click to expand inputs + trace (reuse Inspector
  value renderer).
- Rule tool row for l4-rules MCP: `⚙ Evaluating rule {name}` + expand
  card with inputs (JSON tree) and output (Inspector value renderer).
- Generic MCP fallback row for unknown-category tools.
- `meta__ask_user` rendered as a distinct input-prompt card above the
  assistant's in-progress text.
- Turn-end review card (multi-file summary + "Show combined diff").
- Right-click editor code action: "Ask Legalese AI about this" opens
  the tab with the selected text pre-quoted.

**Extension host additions**

- `tool-registry.ts`, `tool-dispatcher.ts`, `permissions.ts`.
- `tools/fs.ts`, `tools/lsp.ts`, `tools/l4-evaluate.ts`, `tools/ask-user.ts`.
- `mcp/client-manager.ts`, `mcp/l4-rules-client.ts`, `mcp/stdio-client.ts`
  (thin wrappers around the SDK).
- `diff-tabs.ts` — `TextDocumentContentProvider` + `vscode.diff`.

**Ai-proxy** — no changes this phase.

---

### Phase 3 — Multimodal & polish (~3 days)

**Goal:** users can attach PDFs and text files; reasoning visibility;
math rendering; final feature cleanup.

**Webview**

- Attachment chips inside input textarea. File picker filters to text
  - PDF; Word triggers a nudge to save as PDF.
- Thinking block component — collapsed-by-default; renders
  `thinkingDelta` events as italic gray text when expanded.
- Katex math rendering in markdown (`$…$`, `$$…$$`).

**Extension host**

- `attachments.ts` — read text as UTF-8, PDFs as base64; assemble
  Anthropic-style array content on user messages.

**Ai-proxy**

- Multi-modal user-message content pass-through (§10.2).
- Thinking/reasoning pass-through as `event: thinking_delta` SSE (§10.3).

---

## 13. Decisions locked in

- **L4 syntax highlighting** — `@repo/l4-highlight`, same as Inspector
  and Docs tabs.
- **Model selection** — fixed `legalese-compose-4`; no picker, no
  drafting-variant pipeline.
- **Default tab on boot** — Docs, not AI tab.
- **Unauthenticated state** — disabled input + single sign-in CTA.
- **No rollback on abort** — once a tool ran, the side effect stands;
  revert via git.
- **Concurrent conversations** — architecturally supported; per-id
  keyed stream store.
- **No privacy notice in-tab** — covered at Legalese Cloud signup.
- **PDFs pass through natively to Claude** — no OCR; Word → "save as
  PDF" nudge.
- **No tool-round cap** — daily token quota is the only safety valve.
- **Delete conversations by rename** — `{id}.json` →
  `{id}.deleted-{ts}.json` on both sides. No `unlink`, no restore UI,
  no "recently deleted" panel. A later cleanup job reaps the renamed
  files.
- **Tool naming** — `category__name` everywhere (`fs__edit_file`,
  `l4-rules__calculate_tax`).
- **String-anchored edits** — `fs__edit_file({path, old, new})`, not
  line ranges.
- **Prompt-cache ordering** — editor/workspace context goes in an extra
  system message in `body.messages`, never prepended to the proxy's
  cached prefix.
- **MCP client** — `@modelcontextprotocol/sdk`; we don't hand-roll
  stdio transports.

## 14. Open questions to resolve during later phases

- **Inspector value renderer** — exact component name. Verify before
  Phase 2 rule-visual work.
- **Regulation-as-document** — first-class workspace citizen vs. plain
  attached file? Needed for two-way citation navigation (§8.3 v2).
  Design when that feature is scoped.
