import path from 'path';
import * as vscode from 'vscode';
import { RuleNode } from './ruleToJson';

export function showViz(context: vscode.ExtensionContext, ruleJson?: RuleNode) {
  let panel: vscode.WebviewPanel | undefined;

  if (!panel) {
    panel = vscode.window.createWebviewPanel(
      'viz',
      'Visualisation',
      vscode.ViewColumn.Beside,
      {
        enableScripts: true,
        retainContextWhenHidden: true
      }
    );

    const ladderDiagramScriptPath = vscode.Uri.file(path.join(context.extensionPath, 'media', 'ladder-diagram.min.js'));
    const ladderDiagramScriptUri = panel.webview.asWebviewUri(ladderDiagramScriptPath);

    panel.webview.html = getWebviewContent(context, panel, ladderDiagramScriptUri, ruleJson || DefaultRule() );

    panel.onDidDispose(() => {
      panel = undefined;
    });
  }
}

function getWebviewContent(
  context: vscode.ExtensionContext,
  panel: vscode.WebviewPanel,
  scriptUri: vscode.Uri,
  ruleJson: RuleNode): string {

  const webviewCssUri = panel.webview.asWebviewUri(
    vscode.Uri.joinPath(
      context.extensionUri,
      'node_modules',
      'ladder-diagram',
      'css',
      'ladder.css'
    )
  );

  return `
    <!DOCTYPE html>
    <html lang="en">
    <head>
      <meta charset="UTF-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0">
      <title>Rule Ladder Diagram</title>
      <link rel="stylesheet" href="${webviewCssUri.toString()}">
      <style>
        body {
          margin: 0;
          padding: 20px;
          font-family: Arial, sans-serif;
        }
        #ladder-container {
          width: 100%;
          height: 100vh;
        }
      </style>
      <script src="${scriptUri.toString()}"></script>
    </head>
    <body>
      <div id="ladder-container" style="width: 100%; height: 100%;"></div>
      <hr />
      <!-- <div id="json">${JSON.stringify(ruleJson, null, 2)}</pre></div> -->
      <script>
        // const LadderDiagram = LadderDiagram.LadderDiagram;
        console.log("ladder:", LadderDiagram.LadderDiagram);
        const BoolVar = LadderDiagram.BoolVar;
        const AllQuantifier = LadderDiagram.AllQuantifier;
        const AnyQuantifier = LadderDiagram.AnyQuantifier;

        // JavaScript functions for the ladder diagram
        function q2circuit(q) {
          if (q.andOr.tag === 'Leaf') {
            const utf =
              q.mark.value === 'undefined' ? 'U' :
              q.mark.value === 'true' ? 'T' :
              q.mark.value === 'false' ? 'F' : null;

            return new BoolVar(
              q.andOr.contents,
              false,
              q.mark.source === 'default' ? utf : null,
              q.mark.source === 'user' ? utf : null
            );
          }

          const Construct = q.andOr.tag === 'All' ?
            AllQuantifier :
            AnyQuantifier;

          return new Construct(
            q.andOr.children.map(c => q2circuit(c))
          );
        }

        function renderDiagram() {
          const ruleJson = ${JSON.stringify(ruleJson)};
          const circuit = q2circuit(ruleJson);
          const ld = new LadderDiagram.LadderDiagram(circuit);
          ld.attach(document.getElementById('ladder-container'));
        }

        renderDiagram();
      </script>
    </body>
  </html>
  `;
}

function DefaultRule(): RuleNode {
  return {
    andOr: { tag: 'All', children: [] },
    mark: { value: 'undefined', source: 'user' },
    prePost: {},
    shouldView: 'Ask'
  };
}
