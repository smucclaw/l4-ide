import esbuild from 'esbuild'

/*
The previous esbuild package.json script that
this is meant to extend
(setting aside the ts vs mts difference):

    "esbuild-base": "esbuild ./src/extension.ts --bundle --minify --outfile=out/extension.js --external:vscode --format=cjs --platform=node",
*/

esbuild
  .build({
    entryPoints: ['./src/extension.mts'],
    bundle: true,
    minify: true,
    sourcemap: true,
    platform: 'node',
    target: 'es2020', // to match the target in tsconfig.json
    external: ['vscode'],
    format: 'cjs',
    outfile: 'out/extension.js',
    loader: {
      '.html': 'text',
      // Load .html files as text.
      // Required for the html loading in the webview part of webview-panel.ts
    },
  })
  .then(() => {
    console.log('Esbuild-bundling of VSCode extension succeeded!')
  })
  .catch(() => process.exit(1))
