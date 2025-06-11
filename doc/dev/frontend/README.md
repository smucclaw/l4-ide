# Notes on the frontend

This page is written for software developers who are helping to develop the L4 frontend.

The 'frontend' includes:

- the VSCode and Monaco/jl4-web extensions
- [the Ladder visualizer](../../../ts-shared/l4-ladder-visualizer)
- as well as auxiliary or helper packages like [jl4-client-rpc](../../../ts-shared/jl4-client-rpc/)

## Where to find stuff

The substantive Typescript / frontend stuff is in either `ts-apps` or `ts-shared`.

There's also misc config files and dev infra at the top level of the repo, e.g.

- [turbo.json for our Turbo monorepo](../../../turbo.json)
- [VSCode tasks](../../../.vscode)
- the top level package.json and package.lock (see Turborepo docs for how monorepos are structured if you are wondering why there are 'nested' package.jsons)

## (A partial list of the key) design decisions and conventions

- Tech stack: The frontend work in this repo uses
  - Typescript and Svelte 5
  - Tailwind 4 (not clear if this was the right choice at the time)
  - and of course, Turborepo for the TS monorepo, with `npm` (`pnpm` would have required more work to use it with `vsce`)
- We use the 'Layout-IR' data structure as an intermediate data structure for more complex UIs. The core layout IR library is currently in [the Ladder visualizer](../../../ts-shared/l4-ladder-visualizer), but it should be factored out to a package in ts-shared at some point.
- [The Ladder visualizer](../../../ts-shared/l4-ladder-visualizer) uses the algebraic-graphs formalism when constructing graphs; this allows us to construct and manipulate graphs in a high-level, safe way. This has, in the admittedly-biased opinion of the person who made this decision, been a huge productivity win.
- Re the client-side RPC, see [the README in jl4-client-rpc](../../../ts-shared/jl4-client-rpc/README.md).
