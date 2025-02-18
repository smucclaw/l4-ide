import * as vscode from 'vscode'
import webviewHtml from '../static/webview/index.html'
import { Uri } from 'vscode'

/******************************************
                Panel
*******************************************/

export interface PanelConfig {
  viewType: string
  title: string
  position: vscode.ViewColumn
}

export class PanelManager {
  /** Allow only one webview to exist at any given moment. */
  #panel: vscode.WebviewPanel | undefined = undefined

  private frontendIsReadyPromise: Promise<void>
  private frontendIsReadyResolver!: () => void

  constructor(private readonly config: PanelConfig) {
    this.resetWebviewFrontendIsReady()
  }

  getPanel() {
    return this.#panel
  }

  private resetWebviewFrontendIsReady() {
    this.frontendIsReadyPromise = new Promise<void>((resolve) => {
      this.frontendIsReadyResolver = resolve
    })
  }

  markFrontendAsReady() {
    if (this.frontendIsReadyResolver) {
      this.frontendIsReadyResolver()
      // Since the webview can be recreated, allow the Promise to be resolved each time the webview frontend sends the "ready" notification.
    }
  }

  getWebviewFrontendIsReadyPromise(): Promise<void> {
    return this.frontendIsReadyPromise
  }

  initialize(context: vscode.ExtensionContext, ownUri: Uri) {
    if (!this.#panel) {
      this.#panel = vscode.window.createWebviewPanel(
        this.config.viewType,
        // TODO: The title can be `<title prefix> <filename>`
        this.config.title,
        { viewColumn: this.config.position, preserveFocus: true },
        {
          enableScripts: true,
          retainContextWhenHidden: true,
        }
      )
      this.#panel.webview.html = getWebviewContent(context, this.#panel)

      // Reset when the current panel is closed
      this.#panel.onDidDispose(async () => {
        // if the panel dies, we want to reset the visualisation
        // such that the extension doesn't keep bringing up the visualisation
        // after it has been closed
        await vscode.commands.executeCommand(
          'l4.resetvisualization',
          ownUri.toString()
        )
        this.#panel = undefined
        this.resetWebviewFrontendIsReady()
      })
    }
  }

  /**
   * If the panel has already been created, reveal it;
   * make it if not.
   */
  render(context: vscode.ExtensionContext, ownUri: Uri) {
    if (this.#panel) {
      this.#panel.reveal(this.config.position, /* preserveFocus */ true)
    } else {
      this.initialize(context, ownUri)
      // this.#panel.reveal(this.config.position)
    }
  }
}

/******************************************
            getWebviewContent
*******************************************/

const STATIC_ASSETS_DIR = 'static'
const WEBVIEW_DIR = 'webview'

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
