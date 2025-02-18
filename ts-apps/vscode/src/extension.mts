import { ExtensionContext, workspace, window } from 'vscode'
import * as vscode from 'vscode'
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
} from 'vscode-languageclient/node.js'
import type { WebviewTypeMessageParticipant } from 'vscode-messenger-common'
import { Messenger } from 'vscode-messenger'
import {
  VisualizeDecisionLogicIRInfo,
  WebviewFrontendIsReadyNotification,
  VisualizeDecisionLogicRequest,
} from '@repo/viz-expr'
import { Schema } from 'effect'
// import { cmdViz } from './commands.js'
import type { PanelConfig } from './viz.js'
import { PanelManager } from './viz.js'

/***********************************************
     decode for VisualizeDecisionLogicIRInfo
     (aka the payload from lang server)
***********************************************/

const decode = Schema.decodeUnknownSync(VisualizeDecisionLogicIRInfo)

/***************************************
      Language Client
****************************************/

let client: LanguageClient

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
      Activate
****************************************/

export async function activate(context: ExtensionContext) {
  const langId = 'l4'
  const langName = 'jl4 LSP'
  const outputChannel: vscode.OutputChannel = window.createOutputChannel(
    langName,
    langId
  )

  const serverCmd: string =
    workspace.getConfiguration('jl4').get('serverExecutablePath') ?? 'jl4-lsp'
  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  const serverOptions: ServerOptions = {
    run: { command: serverCmd },
    debug: {
      command: serverCmd,
    },
  }

  const panelManager = new PanelManager(PANEL_CONFIG)

  const webviewMessenger = new Messenger({ debugLog: true })

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

          outputChannel.appendLine('in executeCommand...')
          outputChannel.appendLine(`args are ${args.toString()}`)

          const responseFromLangServer: unknown = await next(command, args)

          if (responseFromLangServer === null) {
            outputChannel.appendLine(
              'language server returned null, doing nothing'
            )
            return
          }

          outputChannel.appendLine(
            `Received command response ${JSON.stringify(responseFromLangServer)}`
          )

          const vizProgramInfo: VisualizeDecisionLogicIRInfo = decode(
            responseFromLangServer
          )

          outputChannel.appendLine(JSON.stringify(vizProgramInfo))

          panelManager.render(context, editor.document.uri)
          webviewMessenger.registerWebviewPanel(panelManager.getPanel())
          webviewMessenger.onNotification(
            WebviewFrontendIsReadyNotification,
            () => {
              panelManager.markFrontendAsReady()
              outputChannel.appendLine(
                `Ext: got frontend is ready notification!`
              )
            }
          )
          await panelManager.getWebviewFrontendIsReadyPromise()

          const response = await webviewMessenger.sendRequest(
            VisualizeDecisionLogicRequest,
            vizWebviewFrontend,
            vizProgramInfo
          )
          if (response.$type === 'error') {
            outputChannel.appendLine(`Error in visualisation request`)
          } else if (response.$type === 'ok') {
            outputChannel.appendLine(`Visualisation request success`)
          }
        }
        // TODO: else show pop up to client
      },
      didChange: async (event, next) => {
        // we sent a visualisation command whenever we change any code
        // we do this after invoking the callback to avoid blocking the editor
        // on the command invokation
        await next(event)
        await vscode.commands.executeCommand(
          'l4.visualize',
          event.document.uri.toString()
        )
      },
    },
  }

  outputChannel.appendLine(
    `[client] Starting server from the client: ${serverCmd}`
  )

  // Create the language client and start the client.
  client = new LanguageClient(langId, langName, serverOptions, clientOptions)
  // Start the client. This will also launch the server
  await client.start()
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined
  }
  return client.stop()
}
