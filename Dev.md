# Developer's Guide to Building the L4 Toolchain

These instructions are intended for L4 internal developers with experience in Typescript and/or Haskell.

## Quick build and install

```sh
npm install && npm run build
cabal update
cabal install exe:jl4-lsp --overwrite-policy=always
# Make sure the installation directory (usually `~/.cabal/bin/`) is on the `$PATH`
```

## Requirements

- Haskell
  - [GHCup](https://www.haskell.org/ghcup/)
  - ghc 9.6.6
  - cabal 3.10 (or newer)
- npm >= 10.9.2
  - installed via `corepack` or `nvm` or your package manager
  - See the README in `ts-apps/vscode` for more details on working with the VSCode extension.
- VS Code
  - If running `code` from the command line does nothing, see https://code.visualstudio.com/docs/setup/mac#_launch-vs-code-from-the-command-line

After ghcup is installed, run `ghcup tui` and set `ghc` to version `9.6.6`; press `i` to install and then `s` to set that as the default.

If you run into difficulty later with `npm`, you may benefit from first running

```
nvm install --lts
```

And if this is your first time doing any kind of development on your system, on a Mac, you will need to install the Xcode command line tools:

```
xcode-select --install
```

Other prerequisites include xz / liblzma which pkgconfig needs to know about:

```
sudo apt install pkg-config liblzma-dev
```

Under Nix you can run nix-shell in the current directory to pick up the above packages; it will read `shell.nix`.


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

![Syntax Highlighting Example](./doc/images/doc-screenshot-1.png)

## Visualizer

Click on "visualize" to see a rendering of the decision logic.
