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
