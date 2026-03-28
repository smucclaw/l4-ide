import * as vscode from 'vscode'
import * as fs from 'fs'
import * as path from 'path'
import webviewHtml from '../static/webview/index.html'
import { Uri } from 'vscode'
import { getTokenColors, tokenColorsToCSS } from './theme-colors.js'

/******************************************
                Panel
*******************************************/

export interface PanelConfig {
  viewType: string
  title: string
  position: vscode.ViewColumn
  /** Subpath within the webview static dir. Defaults to root. */
  htmlSubpath?: string
  /** Callback when the panel is disposed */
  onDispose?: (ownUri: string) => Promise<void>
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
      this.#panel.webview.html = getWebviewContent(
        context,
        this.#panel.webview,
        this.config.htmlSubpath
      )

      // Reset when the current panel is closed
      this.#panel.onDidDispose(async () => {
        if (this.config.onDispose) {
          await this.config.onDispose(ownUri.toString())
        } else {
          // Default: reset visualization (legacy behavior for ladder panel)
          await vscode.commands.executeCommand(
            'l4.resetvisualization',
            ownUri.toString()
          )
        }
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

  /**
   * Update the --l4-tok-* CSS custom properties in the webview
   * to match the current VS Code color theme.
   */
  refreshTokenColors() {
    const panel = this.#panel
    if (!panel) return

    const colors = getTokenColors()
    panel.webview.postMessage({
      type: 'l4-token-colors',
      colors,
    })
  }
}

/******************************************
            getWebviewContent
*******************************************/

const STATIC_ASSETS_DIR = 'static'
const WEBVIEW_DIR = 'webview'

/**
 * Build the HTML content for a webview.
 * Accepts a `vscode.Webview` (the common interface shared by both
 * `WebviewPanel.webview` and `WebviewView.webview`) so it works
 * for both panels and the sidebar.
 */
export function getWebviewContent(
  context: vscode.ExtensionContext,
  webview: vscode.Webview,
  htmlSubpath?: string
): string {
  const basePath = vscode.Uri.joinPath(
    context.extensionUri,
    STATIC_ASSETS_DIR,
    WEBVIEW_DIR,
    ...(htmlSubpath ? [htmlSubpath] : [])
  )
  const compatibleBasePath = webview.asWebviewUri(basePath)

  let html: string
  if (htmlSubpath) {
    const htmlPath = path.join(
      context.extensionPath,
      STATIC_ASSETS_DIR,
      WEBVIEW_DIR,
      htmlSubpath,
      'index.html'
    )
    html = fs.readFileSync(htmlPath, 'utf-8')
  } else {
    html = webviewHtml
  }

  const tokenCSS = tokenColorsToCSS(getTokenColors())
  const csp = `<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src ${webview.cspSource} 'unsafe-inline'; script-src ${webview.cspSource} 'unsafe-inline'; img-src ${webview.cspSource} https:; font-src ${webview.cspSource}; connect-src https://legalese.com;">`

  const postprocessedWebviewHtml = html.replace(
    '<head>',
    `<head><base href="${compatibleBasePath}/">${csp}${tokenCSS}`
  )

  return postprocessedWebviewHtml
}
