# Notes on the frontend

This page is written for software developers who are helping to develop the L4 frontend.

The 'frontend' includes:

- the VSCode and Monaco/jl4-web extensions
- [the Ladder visualizer](../../../ts-shared/l4-ladder-visualizer)
- as well as auxiliary or helper packages like [jl4-client-rpc](../../../ts-shared/jl4-client-rpc/)

## How to use the visualizer library in another web app

See the relevant section of the [the Ladder visualizer README](../../../ts-shared/l4-ladder-visualizer/README.md).

## Where to find stuff

The substantive Typescript / frontend stuff is in either `ts-apps` or `ts-shared`.

You will also want to look at [the Viz / Ladder backend](../../../jl4-lsp/src/LSP/L4/Viz/) and potentially also Handlers.hs.

There's also misc config files and dev infra at the top level of the repo, e.g.

- [turbo.json for our Turbo monorepo](../../../turbo.json)
- [VSCode tasks](../../../.vscode)
- the top level package.json and package-lock.json (see Turborepo docs for how monorepos are structured if you are wondering why there are 'nested' package.jsons)

## (A partial list of the key) design decisions and conventions

- Tech stack: The frontend work in this repo uses
  - Typescript and Svelte 5
  - Tailwind 4 (not clear if this was the right choice at the time)
  - Shadcn Svelte / BitsUI for UI components
  - and of course, Turborepo for the TS monorepo, with `npm` (`pnpm` would have required more work to use it with `vsce`)
- We use the 'Layout-IR' data structure as an intermediate data structure for more complex UIs. The core layout IR library is currently in [the Ladder visualizer](../../../ts-shared/l4-ladder-visualizer), but it should be factored out to a package in ts-shared at some point.
- [The Ladder visualizer](../../../ts-shared/l4-ladder-visualizer) uses the algebraic-graphs formalism when constructing graphs; this allows us to construct and manipulate graphs in a high-level, safe way. This has, in the admittedly-biased opinion of the person who made this decision, been a huge productivity win.
- Re the client-side RPC, see [the README in jl4-client-rpc](../../../ts-shared/jl4-client-rpc/README.md).

## Misc notes on infra

If you are actively developing on the frontend, the following should be helpful:

- There's still some relatively low-hanging fruit when it comes to making the TS build faster.
  - See the relevant part of the Turborepo docs: https://turborepo.com/docs/crafting-your-repository/configuring-tasks#dependent-tasks-that-can-be-run-in-parallel
  - (You might also need to use turbo commands, e.g., `turbo run lint test build check`, instead of npm, though I'm not totally sure.)

## Other frontend dev docs

- The READMEs of the various sub-packges
- [How to debug the NoIntermediateBundlingNodeDag](./no-intermediate-bundling-node-dag.md)
- There's also a reasonable amount of comments / docs, especially on key invariants, in the codebase itself; I have also tried to structure the code and types so as to make the underlying software design explicit (though of course I might not always have succeeded in doing so)
