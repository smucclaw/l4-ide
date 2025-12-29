# `jl4-repl`

Interactive Read–Eval–Print Loop (REPL) for L4, built on the same parsing + typechecking + evaluation pipeline as the LSP.

## Run

```bash
cabal run jl4-repl -- path/to/file.l4
```

Or start empty, then `:load`:

```bash
cabal run jl4-repl
```

## Common Commands

- `:help` — show built-in help
- `:load <file>` / `:reload` — load or reload the current file
- `:type <expr>` — show the inferred type for an expression
- `:info <name>` / `:env` — inspect names in scope

## Query Planning (Elicitation)

- `:decides` — list `DECIDE` declarations in the loaded file
- `:queryplan [DECIDE] [k=v ...]` (alias `:qp`) — print query-plan ranking under partial boolean bindings

Bindings are `key=true|false` (also accepts `t/f`, `yes/no`, `1/0`).

Examples:

```text
:decides
:qp compute_qualifies walks=true eats=false
:qp vermin_and_rodent i.some_field=true
```

## Tracing

- `:trace <expr>` / `:tr <expr>` — show evaluation trace as GraphViz DOT
- `:traceascii <expr>` / `:tra <expr>` — show evaluation trace as an ASCII tree
- `:tracefile <path|off>` — save DOT traces to `<path>-NN.dot` (or restore stdout with `off`)
