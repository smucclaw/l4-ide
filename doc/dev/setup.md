# Developer's Guide to Building the L4 Toolchain

These instructions are intended for L4 internal developers with experience in Typescript and/or Haskell.

**Platform-specific guides:**
- [Windows Build Guide](./windows-build.md) - Detailed Windows setup and release process

## Quick build and install

```sh
npm ci && npm run build
cabal update
cabal install exe:jl4-lsp --overwrite-policy=always
# Make sure the installation directory (usually `~/.cabal/bin/`) is on the `$PATH`
```

### npm ci vs npm install

**Use `npm ci` for regular development:**

- Installs exactly what's in package-lock.json
- Faster and matches CI behavior
- Never modifies package-lock.json
- Fails if package.json and lock file are out of sync (catches real problems)

**Only use `npm install` when:**

- Adding a new package: `npm install some-package`
- Updating dependencies intentionally
- The package-lock.json needs regeneration

If `npm ci` fails with sync errors, investigate before running `npm install` - something is actually wrong.

## Requirements

- Haskell
  - [GHCup](https://www.haskell.org/ghcup/)
  - ghc 9.8.4
  - cabal 3.10 (or newer)
- npm >= 10.9.2
  - installed via `corepack` or `nvm` or your package manager
  - See the README in `ts-apps/vscode` for more details on working with the VSCode extension.
- VS Code
  - If running `code` from the command line does nothing, see https://code.visualstudio.com/docs/setup/mac#_launch-vs-code-from-the-command-line
- GraphViz (`dot`) + xdot
  - Required for `jl4-cli --graphviz`, REPL `:trace` / `:tracefile`, decision-service PNG/SVG endpoints, and interactive viewing with `xdot`
  - `brew install graphviz xdot` on macOS, `apt-get install graphviz xdot` on Debian/Ubuntu (or your package manager of choice)
  - The Nix shell (`nix/shell.nix`), flake-based hosts (`nix/configuration.nix`), and the Docker `jl4-decision-service` image all install `graphviz`/`xdot` automatically so containerized builds can render traces, too.
- on your system
  - pkgconfig
  - xz (or liblzma dev libraries)

After ghcup is installed, run `ghcup tui` and set `ghc` to version `9.8.4`; press `i` to install and then `s` to set that as the default.

If you run into difficulty later with `npm`, you may benefit from first running

```
nvm install --lts
```

And if this is your first time doing any kind of development on your system, on a Mac, you will need to install the Xcode command line tools:

```
xcode-select --install
```

Other prerequisites which pkgconfig needs to know about:

```
sudo apt install pkg-config liblzma-dev libgmp-dev
```

Under Nix you can run `nix-shell nix/shell.nix` in the current directory to pick up the above packages (GraphViz included).

Alternatively, if you use direnv, the included `.envrc` file will automatically load the nix flake environment when you `cd` into the repository (on NixOS systems only - it's harmless on other platforms).

## Tests

```sh
cabal test test:jl4-test
```

### Test performance note

- The post-temporals `jl4-test` runtime roughly doubled because of new Excel/temporal acceptance files (`ok/excel-date/*.l4`, `ok/temporal-acceptance.l4`, `jl4-core/libraries/temporal-prelude.l4`). Each runs three golden checks and repeatedly loads the large `excel-date`/`daydate` libraries.
- TDNR-focused cases (`ok/tdnr.l4`, `not-ok/tc/tdnr-ambiguous.l4`, `ok/type-coercion.l4`) show no slowdown; some got faster.
- If you need faster iterations while working on typechecker changes, temporarily filter tests, e.g. `cabal test jl4-test --test-options='--match "tdnr|type-coercion"'`, or batch the heavy Excel files in a single run to reuse the Shake cache.

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

## Running the web IDE locally

The general overview is as follows:

- the webpage is served as a javascript bundle

  - it directly connects to the language server provided by the `jl4-lsp` binary via websocket
  - it directly connects to the session persistance service provided by `jl4-websessions` via http

- the decision service can serve examples from `--sourcePaths` but also by directly contacting the
  `jl4-websessions` service via http

- to set up the CRUD service for saving files:
  `cabal run exe:jl4-websessions -- 5008 test.db`
- to set up the language server to talk to the frontend via websocket:
  `cabal run exe:jl4-lsp -- ws --port 5007`
- to run the frontend that connects to both:
  `cd ts-apps/jl4-web; npm ci; npm run dev`
- check out `--help` for `jl4-websessions` and `jl4-lsp`
- to run the decision service locally:
  `cabal run jl4-decision-service-exe -- --port 8081 --serverName http://localhost:8081/ --sourcePaths jl4/experiments/britishcitizen5.l4 --sourcePaths jl4/experiments/parking.l4`
  The default is set to connect to the webessions service on port 5007
