# Developer's Guide to Building the L4 Toolchain

These instructions are intended for L4 internal developers with experience in Typescript and/or Haskell.

## Requirements:

- Haskell
  - [GHCup](https://www.haskell.org/ghcup/)
  - ghc 9.6.6
  - cabal 3.10 (or newer)
- npm >= 10.9.2
  - installed via `corepack` or `nvm` or your package manager
  - See the README in `ts-apps/vscode` for more details on working with the VSCode extension.
- VS Code
  - If running `code` from the command line does nothing, see https://code.visualstudio.com/docs/setup/mac#_launch-vs-code-from-the-command-line

## Build:

```sh
cabal update
cabal build all
npm install && npm run build
cabal install exe:jl4-lsp --overwrite-policy=always
# Make sure the installation directory (usually `~/.cabal/bin/`) is on the `$PATH`
```

## Tests

```sh
cabal test test:jl4-test
```

## Run

After a successful build above, quit any running VS Code instances, and launch one from the current directory:

```sh
code .
```

Press `F5` from within VSCode. The JL4 extension should launch in a new VS Code window.

## Examples

Open folder [./jl4/examples](./jl4/examples) to see the Language Server in action

## Syntax Highlighting

![Syntax Highlighting Example](./doc/images/doc-screenshot-1-.png)

## Visualizer

Click on "visualize" to see a rendering of the decision logic.
