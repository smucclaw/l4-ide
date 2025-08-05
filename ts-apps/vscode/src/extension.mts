import { ExtensionContext, workspace, window } from 'vscode'
import * as vscode from 'vscode'
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
} from 'vscode-languageclient/node.js'
import { createConverter as createCodeConverter } from 'vscode-languageclient/lib/common/codeConverter.js'
import type { PanelConfig } from './webview-panel.js'
import { PanelManager } from './webview-panel.js'

import { VSCodeL4LanguageClient } from './vscode-l4-language-client.js'
import { BinaryManager } from './binary-manager.js'

import { RenderAsLadderInfo, VersionedDocId } from '@repo/viz-expr'
import { Schema } from 'effect'
import type { WebviewTypeMessageParticipant } from 'vscode-messenger-common'
import { Messenger } from 'vscode-messenger'
import {
  RenderAsLadder,
  WebviewFrontendIsReadyNotification,
  makeLspRelayRequestType,
} from 'jl4-client-rpc'

/***********************************************
     decode for RenderAsLadderInfo
     (aka the payload from lang server)
***********************************************/

const decode = Schema.decodeUnknownSync(RenderAsLadderInfo)

/***************************************
      Language Client
****************************************/

let client: VSCodeL4LanguageClient
const code2ProtocolConverter = createCodeConverter()

/***************************************
       Webview Panel
****************************************/

const PANEL_CONFIG: PanelConfig = {
  viewType: 'l4Viz',
  title: 'Visualize L4',
  position: vscode.ViewColumn.Beside,
}

const vizWebviewFrontend: WebviewTypeMessageParticipant = {
  type: 'webview',
  webviewType: 'l4Viz',
}

/***************************************
      Set up webview messenger
****************************************/

function initializeWebviewMessenger(
  outputChannel: vscode.OutputChannel,
  panelManager: PanelManager
) {
  /** Messenger for VSCode extension to communicate with webview */
  const webviewMessenger = new Messenger({ debugLog: true })

  // Set up listeners
  // -- Listen for whether webview frontend has initialized
  webviewMessenger.onNotification(WebviewFrontendIsReadyNotification, () => {
    panelManager.markFrontendAsReady()
    outputChannel.appendLine(`Ext: got frontend is ready notification!`)
  })

  // -- Listen for LSP client relay requests from webview
  webviewMessenger.onRequest(
    makeLspRelayRequestType<object, unknown>(),
    async (clientReqParams) => {
      outputChannel.appendLine(
        `Ext: Received LSP relay request from webview:\n${JSON.stringify(clientReqParams)}`
      )
      const response = await client.sendRequest(
        clientReqParams.requestType,
        clientReqParams.params
      )
      outputChannel.appendLine(
        `Response from server:\n${JSON.stringify(response)}`
      )
      outputChannel.appendLine(
        '--------------------------------------------------'
      )
      return response
    }
  )

  return webviewMessenger
}

/***************************************
      Activate
****************************************/

export async function activate(context: ExtensionContext) {
  const langId = 'l4'
  const langName = 'jl4 LSP'
  const outputChannel: vscode.OutputChannel = window.createOutputChannel(
    langName,
    langId
  )

  // Initialize binary manager with output channel for better logging
  const binaryManager = new BinaryManager(context, outputChannel)

  // Determine server command
  let serverCmd: string

  // First check for user configuration
  const configuredPath = workspace
    .getConfiguration('jl4')
    .get('serverExecutablePath') as string | undefined

  if (configuredPath) {
    // Use explicitly configured path
    serverCmd = configuredPath
    outputChannel.appendLine(`Using configured server path: ${serverCmd}`)
  } else {
    // Try to get bundled binary path from BinaryManager
    const bundledBinaryPath = await binaryManager.getBinaryPath()

    if (bundledBinaryPath) {
      // Use bundled binary
      serverCmd = bundledBinaryPath
      outputChannel.appendLine(`Using bundled binary: ${serverCmd}`)
    } else {
      // Fallback to system binary
      serverCmd = 'jl4-lsp'
      outputChannel.appendLine(`Using system binary: ${serverCmd}`)

      // Warn the user
      window.showWarningMessage(
        `The jl4-lsp binary was not found bundled with the extension. If the extension fails to activate, please install the binary or configure its path in settings.`
      )
    }
  }

  // Initialize panelManager and webviewMessenger
  const panelManager = new PanelManager(PANEL_CONFIG)
  const webviewMessenger = initializeWebviewMessenger(
    outputChannel,
    panelManager
  )

  // Define server options
  const serverOptions: ServerOptions = {
    run: { command: serverCmd },
    debug: { command: serverCmd },
  }

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: langId, pattern: '**/*' }],
    diagnosticCollectionName: langName,
    revealOutputChannelOn: RevealOutputChannelOn.Never,
    outputChannel,
    outputChannelName: langName,
    middleware: {
      executeCommand: async (command, args, next) => {
        const editor = vscode.window.activeTextEditor
        if (editor) {
          // why do we copy the args via .slice(0)?
          // No clue, the sample does it.
          // Maybe to avoid accidental mutation?
          if (args.length === 0) {
            args = args.slice(0)
            args.push(editor.document.uri.toString())
          }

          outputChannel.appendLine('')
          outputChannel.appendLine(
            '<executeCommand>--------------------------------------------------'
          )
          outputChannel.appendLine(`called with args\n${JSON.stringify(args)}`)
          outputChannel.appendLine('')

          const responseFromLangServer: unknown = await next(command, args)

          if (responseFromLangServer === null) {
            outputChannel.appendLine(
              'language server returned null, doing nothing'
            )
            return
          }

          outputChannel.appendLine('')
          outputChannel.appendLine(
            `Received command response\n${JSON.stringify(responseFromLangServer)}`
          )
          outputChannel.appendLine('')

          const ladderInfo: RenderAsLadderInfo = decode(responseFromLangServer)

          panelManager.render(context, editor.document.uri)
          webviewMessenger.registerWebviewPanel(panelManager.getPanel())
          await panelManager.getWebviewFrontendIsReadyPromise()

          const response = await webviewMessenger.sendRequest(
            RenderAsLadder,
            vizWebviewFrontend,
            ladderInfo
          )
          if (response.$type === 'error') {
            outputChannel.appendLine(`Error in render viz in webview request`)
          } else if (response.$type === 'ok') {
            outputChannel.appendLine(
              `Request to render viz in webview succeeded`
            )
          }
        }

        outputChannel.appendLine(
          '--------------------------------------------------</executeCommand>'
        )
        // TODO: else show pop up to client
      },
      didChange: async (event, next) => {
        // we sent a visualisation command whenever we change any code
        // we do this after invoking the callback to avoid blocking the editor
        // on the command invokation
        await next(event)

        const verDocId: VersionedDocId =
          code2ProtocolConverter.asVersionedTextDocumentIdentifier(
            event.document
          )
        await vscode.commands.executeCommand('l4.visualize', verDocId)
      },
    },
  }

  outputChannel.appendLine(
    `[client] Starting server from the client: ${serverCmd}`
  )

  try {
    // Create the language client and start the client.
    client = new VSCodeL4LanguageClient(
      new LanguageClient(langId, langName, serverOptions, clientOptions)
    )

    // Start the client. This will also launch the server
    await client.start()
    outputChannel.appendLine(`Language server started successfully`)
  } catch (error) {
    outputChannel.appendLine(`Failed to start language server: ${error}`)
    window.showErrorMessage(
      `Failed to start jl4 language server: ${error}. Please check the binary path in settings.`
    )
  }
}

export async function deactivate(): Promise<void> {
  if (!client) {
    return undefined
  }
  await client.dispose()
}
