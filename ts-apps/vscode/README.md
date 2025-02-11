# jl4 LSP Client

This is a prototype of a VSCode LSP client for `jl4` programs.
It includes a webview extension designed to visualize L4 programs as ladder diagrams.

## Running the Extension

From the root `mattwaddington` directory:

```bash
npm install # if you haven't installed already
code .
# In VSCode, press 'F5'
```

Pressing 'F5' will launch an `Extension Development Host` Editor with the extension installed.

Make sure to open the root directory of this project in VSCode, to make sure `.vscode/launch.json` is picked up.

### Installing the Extension

1. Install dependencies and build the project

It is best to install starting from the root directory.

```bash
npm install
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
   The extension will be packaged as `jl4-lsp-client-0.0.1`. Open VSCode, select the _Install from VSIX_ option, and install the file.

   > Note: After installation, the extension might initially appear inactive. To activate it, load or write an L4 rule (see details below).

   You can also install the extension from the command line with `code --install-extension *.vsix`.

### Configuring the Language Server

The extension expects to find `jl4-lsp` on the `$PATH`. If you didn't install the extension before, you can do this via:

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

The ladder diagrams are powered by an npm library developed by Jules and Zeming, available [here](https://github.com/JuliaPoo/ladder-diagram). For use in this extension, the library is bundled using esbuild to generate a standalone JavaScript file. To update the bundled file in the future, you can run:

```bash
npx esbuild node_modules/ladder-diagram/js/ladder.js --bundle --format=iife --global-name=LadderDiagram --outfile=media/ladder-diagram.min.js
```

This produces a file named `ladder-diagram.min.js`, located in the `media` folder, which is referenced by the extension.

### Displaying the Diagram

To visualize your rule as a ladder diagram:

- Save the file containing your rule.

- Alternatively, use the "Update Diagram" button in the bottom-right status bar of VSCode (near the notification bell or Prettier controls). Clicking this button generates the diagram in a new panel.
  ![alt text](screenshots/update-viz.png)

## More build / config notes

Jan 20 2025, tsconfig.json: `"DOM"` had to be added to the value for `lib` to avoid issues like the following

```bash
┌ jl4-lsp-client#build > cache miss, executing a18c902b9c9c2a55
│
│
│ > jl4-lsp-client@0.0.1 build
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
