# jl4 LSP Client

This is a prototype of a VSCode LSP client for `jl4` programs.
It includes a webview extension designed to visualize L4 programs as ladder diagrams.

## Running the Extension

1. Build the project

It is best to build starting from the parent `typescript-monorepo` directory: please
see the README there, especially the use of `corepack`.

   ```bash
   npm install
   npm run build
   ```

2. Package the extension

   ```bash
   npm package
   ```

   During this process, you may be prompted to create the package without a license. Confirm by responding `y` to proceed.

3. Install the extension
   The extension will be packaged as `jl4-lsp-client-0.0.1`. Open VSCode, select the _Install from VSIX_ option, and install the file.
   > Note: After installation, the extension might initially appear inactive. To activate it, load or write an L4 rule (see details below).

   You can also install the extension from the command line with `code --install-extension *.vsix`.

Alternatively:

Pressing `F5` in VSCode while `vscode/src/extension.ts` is open in your buffer.
Make sure to open the root directory of this project in VSCode, to make sure `.vscode/launch.json` is picked up.

### Configuring the Language Server

The extension expects to find `l4-lsp` on the `$PATH`. If you didn't install the extension before, you can do this via:

```sh
$ cabal install exe:l4-lsp
```

and make sure `cabal`'s `/bin` directory is part of your `$PATH`.

At last, you can specify the location of the binary via `.vscode/settings.json` like this:

```json
{
  "jl4.serverExecutablePath": "<path-to-l4-lsp>"
}
```

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
