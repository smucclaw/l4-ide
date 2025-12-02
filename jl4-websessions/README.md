# JL4 Web Sessions

`jl4-websessions` hosts the browser-based playground that lets users upload, edit, and run L4 programs against the Decision Service backend.

## Exported entry points

When a session uploads an `.l4` module it relies on the same annotation conventions as the decision service:

- Annotate every function that should be callable from the UI with `@export …`.
- Use `@export default …` when the module has a “main” entry point. The UI (and the backend it talks to) invokes this decide clause whenever the user runs the session without picking a specific function name. If no default exists, the first exported function is used.

Inline `@desc` annotations for parameters are still supported and show up in the generated forms.

See `jl4-decision-service/README.md` for more detail on how annotation-based metadata is processed.
