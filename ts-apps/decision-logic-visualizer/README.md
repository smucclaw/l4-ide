# README

## Key commands

```bash
npm run dev -- --open
```

## Notes on dependency-related things / errors / decisions

### esbuild x vite

`esbuild` was added as a direct dependency, with a pinned version,
because I was running into https://github.com/evanw/esbuild/issues/3800
after upgrading `vite`.

## Useful prior art

### Svelte components for VSCode webviews

https://github.com/bscotch/stitch/tree/develop/packages/vscode/webviews

### Layout x xyflow

- An elkjs example featuring subflows that seems easier to understand than the sample code: https://codesandbox.io/p/sandbox/elkjs-layout-subflows-9og9hl?file=%2Flayout.js
