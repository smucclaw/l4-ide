import * as vscode from 'vscode'
// import { RuleNode } from './rule-to-json'
import webviewHtml from '../static/webview/index.html'

const STATIC_ASSETS_DIR = 'static'
const WEBVIEW_DIR = 'webview'

const PANEL_TITLE = 'Decision Logic Visualizer'

export function makeVizPanel(
  context: vscode.ExtensionContext
  // , ruleJson?: RuleNode
) {
  const panel = vscode.window.createWebviewPanel(
    'viz',
    PANEL_TITLE,
    vscode.ViewColumn.Beside,
    {
      enableScripts: true,
      retainContextWhenHidden: true,
    }
  )
  panel.webview.html = getWebviewContent(context, panel)
  return panel
}

function getWebviewContent(
  context: vscode.ExtensionContext,
  panel: vscode.WebviewPanel
): string {
  const basePath = vscode.Uri.joinPath(
    context.extensionUri,
    STATIC_ASSETS_DIR,
    WEBVIEW_DIR
  )
  const compatibleBasePath = panel.webview.asWebviewUri(basePath)

  /* Add the <base> tag so that relative paths work
  (TODO: Test that this actually works...)

  References:
    * https://github.com/bscotch/stitch/blob/76f65a626a6ebd825af5b172b5338a8dee6e947d/packages/vscode/src/webview.igor.mts#L64
    * https://medium.com/@ashleyluu87/data-flow-from-vs-code-extension-webview-panel-react-components-2f94b881467e
  */
  const postprocessedWebviewHtml = webviewHtml.replace(
    '<head>',
    `<head><base href="${compatibleBasePath}/">`
  )

  return postprocessedWebviewHtml
}
