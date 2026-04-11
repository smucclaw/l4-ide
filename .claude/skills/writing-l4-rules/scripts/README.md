# L4 Skill Scripts

Support scripts for the `writing-l4-rules` skill.

## validate-lsp.mjs

A minimal Node-based LSP client that drives `jl4-lsp` over stdio to validate
`.l4` files. Use this when you need to validate files outside the VS Code
extension — for example, in CI, in an Agent SDK sandbox, or when validating
a test file you are not about to edit.

Inside the VS Code extension, you usually do not need this at all: every
`Edit` / `Write` on a `.l4` file already triggers `jl4-lsp` and returns
diagnostics via the IDE hook. Use `validate-lsp.mjs` when that passive flow
is unavailable or insufficient.

### Usage

```bash
node scripts/validate-lsp.mjs path/to/file.l4 [more.l4 ...]
```

For each file, the script:

1. Spawns `jl4-lsp` (must be on `PATH` — the VS Code extension installs it)
2. Sends an LSP `initialize` → `initialized` → `textDocument/didOpen` sequence
3. Waits for `publishDiagnostics` notifications
4. Prints every diagnostic with severity and line number, including
   Info-level `assertion satisfied` / `assertion failed` from `#ASSERT`,
   and `Result: ...` from `#EVAL` / `#TRACE`
5. Exits with `0` if there are no errors, `1` otherwise

Imports resolve relative to each file's directory, exactly as the VS Code
extension does them.

### Requirements

- Node ≥ 18 (uses `node:child_process`, `node:fs`, `node:url`)
- `jl4-lsp` on `PATH`. The VS Code extension ships a platform-specific
  binary under `<extension>/bin/<platform>-<arch>/jl4-lsp`; point your
  `PATH` there, or install the binary elsewhere on `PATH`.

### When to use this vs. IDE diagnostics

| Scenario                                                  | Preferred path          |
| --------------------------------------------------------- | ----------------------- |
| Authoring a rule file, iterating                          | Passive IDE diagnostics |
| Re-checking a test file after editing the rule it imports | `validate-lsp.mjs`      |
| Running a whole test suite and counting results           | `validate-lsp.mjs`      |
| Outside the VS Code extension (CI, Agent SDK, sandbox)    | `validate-lsp.mjs`      |
