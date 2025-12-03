# JL4 Web Sessions

`jl4-websessions` hosts the browser-based playground that lets users upload, edit, and run L4 programs against the Decision Service backend.

## Exported entry points

When a session uploads an `.l4` module it relies on the same annotation conventions as the decision service:

- Annotate every function that should be callable from the UI with `@export …`.
- Use `@export default …` when the module has a “main” entry point. The UI (and the backend it talks to) invokes this decide clause whenever the user runs the session without picking a specific function name. If no default exists, the first exported function is used.

Inline `@desc` annotations for parameters are still supported and show up in the generated forms.

See `jl4-decision-service/README.md` for more detail on how annotation-based metadata is processed.

## Session persistence & UUID auto-save

Every time a user uploads or edits an L4 program the server saves it to SQLite under a randomly generated UUID. That UUID becomes both the shareable link for the session and the function name used when the program is pushed to the decision service (if a backend URL is configured). When someone loads a session by UUID, the decision service will:

- expose every `@export …` decide clause under `/functions/{uuid}:{name}`; and
- run the decide marked `@export default …` if no specific function name is provided.

Because sessions are permalinked by UUID, you can bookmark or send the URL to collaborators, and the backend will auto-load the saved module and its exported entry point without additional setup.
