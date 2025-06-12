# README

## Key commands

```bash
npm run dev --open
```

You might need to build from the root dir when you've made changes that touch multiple packages.

## How to use the visualizer library in another web app

### Easiest way: just make another app in this monorepo

The easiest thing to do is to make another Svelte app in the `ts-apps` dir of this monorepo,
with configs similar to the those of the other apps there (e.g. the vscode webview or jl4-web). And then just import the relevant lib(s)
from that app, in the same way that those other apps do (see, e.g., [the vscode webview +page.svelte](../../ts-apps/webview/src/routes/+page.svelte)).

It might also help to look at [the demo route in the visualizer package](../../ts-shared/l4-ladder-visualizer/src/routes/+page.svelte), especially if you plan to use stock or hard-coded examples.

### Other options

- [Make the visualizer package an 'external' package, in Turborepo's parlance, and deploy it to npm](https://turborepo.com/docs/guides/publishing-libraries).
  - Given the current setup / API, this will probably also require publishing the core Lir package, since the API currently requires consumers to set up and pass in the LirNode.
  - If you want to make it possible for consumers to opt out of using the Lir framework, you will need to extend the visualizer API accordingly. This is easy --- shouldn't require more than an hour of work; I just hadn't done it because the frontend architecture so far had been structured around using the Lir framework for our state synchronization needs, and because the priority had been on developing more features.
- If you also want to be able to use it _without_ using Svelte,
  - one option here might be to compile it to a 'custom element' or 'web component': https://svelte.dev/docs/svelte/custom-elements But do read the 'Caveats and Limitations' section of that link.
  - There might also be other options: this is not something I know a lot about.

## Notes on dependency-related things / errors / decisions

### Make sure that SF nodes don't try to render and measure themselves in a non-browser environment (e.g. if SSR is enabled)

### esbuild x vite

`esbuild` was added as a direct dependency, with a pinned version,
because I was running into https://github.com/evanw/esbuild/issues/3800
after upgrading `vite`.

## Useful prior art

### Svelte components for VSCode webviews

https://github.com/bscotch/stitch/tree/develop/packages/vscode/webviews

### Layout x xyflow

- An elkjs example featuring subflows that seems easier to understand than the sample code: https://codesandbox.io/p/sandbox/elkjs-layout-subflows-9og9hl?file=%2Flayout.js
