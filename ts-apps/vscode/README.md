# jl4 LSP Client

This is the VSCode LSP client for `jl4` programs.
It includes a webview extension designed to visualize L4 programs as ladder diagrams.

## Running the Extension

From the root directory:

```bash
npm ci # if you haven't installed already
code .
# In VSCode, press 'F5'
```

Pressing 'F5' will launch an `Extension Development Host` Editor with the extension installed.

Make sure to open the root directory of this project in VSCode, to make sure `.vscode/launch.json` is picked up.

### Installing the Extension

1. Install dependencies and build the project

It is best to install starting from the root directory.

```bash
npm ci
npm run build
```

2. Package the extension

From this (i.e., the `vscode`) directory:

```bash
npm run package
```

(If you get a `npm error Missing script: "package"` error,
that means you aren't in the right directory.)

3. Install the extension
   The extension will be packaged as `l4-vscode-1.0.0`. Open VSCode, select the _Install from VSIX_ option, and install the file.

   > Note: After installation, the extension might initially appear inactive. To activate it, load or write an L4 rule (see details below).

   You can also install the extension from the command line with `code --install-extension *.vsix`.

### Configuring the Language Server

**Platform-specific builds:** If you're using a platform-specific build of the extension (available via GitHub releases), the `jl4-lsp` binary is bundled and no additional configuration is needed.

**Universal builds / Development:** The extension expects to find `jl4-lsp` on the `$PATH`. Install it via:

```sh
$ cabal install exe:jl4-lsp --overwrite-policy=always
```

and make sure `cabal`'s `/bin` directory is part of your `$PATH`.

Alternatively, you can specify the location of the binary via `.vscode/settings.json` like this:

```json
{
  "jl4.serverExecutablePath": "<path-to-jl4-lsp>"
}
```

#### Binary Resolution Order

The extension looks for the language server in this order:

1. User-configured path via `jl4.serverExecutablePath` setting
2. Bundled binary at `<extension>/bin/<platform>-<arch>/jl4-lsp[.exe]`
3. `jl4-lsp` on the system PATH

#### Supported Bundled Platforms

- `darwin-arm64` (macOS Apple Silicon)
- `darwin-x64` (macOS Intel)
- `win32-x64` (Windows x64)
- `linux-x64` (Linux x64)
- `linux-arm64` (Linux ARM64)

### Development

It can be faster for development to use a workflow as follows:

```sh
cabal build exe:jl4-lsp
cabal list-bin exe:jl4-lsp
```

and use the obtained path for `"jl4.serverExecutablePath": "<path-to-jl4-lsp>"`.
Running `cabal build exe:jl4-lsp` and restarting the `Extension Development Host` Editor is sufficient to use the latest version of the JL4 Language Server.

## Features and Functionalities

### Ladder Diagram

The extension uses a VSCode webview to render L4 rules as ladder diagrams, displaying them in a panel next to the editor. This setup enables the extension to update the visualisation dynamically, reflecting any changes made to the rules in real time.

### Displaying the Diagram

After opening a .l4 file, to visualize an eligible rule as a ladder diagram, click on the "Visualize" or "Simplify and visualize" codelens above the L4 expression you want to visualize.

## More build / config notes

### lib, DOM

Jan 20 2025, tsconfig.json: `"DOM"` had to be added to the value for `lib` to avoid issues like the following

```bash
┌ l4-vscode#build > cache miss, executing a18c902b9c9c2a55
│
│
│ > l4-vscode@1.0.0 build
│ > tsc -b tsconfig.json && npm run esbuild-base
│
│ ../../node_modules/@types/d3-drag/index.d.ts:14:38 - error TS2304: Cannot find name 'Element'.
│
│ 14 export type DraggedElementBaseType = Element;
│                                         ~~~~~~~
│
│ ../../node_modules/@types/d3-drag/index.d.ts:19:36 - error TS2304: Cannot find name 'HTMLElement'.
│
│ 19 export type DragContainerElement = HTMLElement | SVGSVGElement | SVGGElement; // HTMLElement inclu
│ des HTMLCanvasElement
```
