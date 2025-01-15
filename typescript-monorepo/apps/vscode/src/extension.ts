/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { ExtensionContext, workspace, window } from 'vscode'
import * as vscode from 'vscode'
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
} from 'vscode-languageclient/node'
import * as command from './commands'
import { RuleNode } from './ruleToJson'
import { showViz } from './viz'

let client: LanguageClient

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

  // Options to control the language client
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
          args = args.slice(0)
          args.push(editor.document.uri.toString())
          const result: unknown = await next(command, args)
          outputChannel.appendLine(
            `Received command response ${JSON.stringify(result)}`
          )
          const nodeVisualisation: RuleNode[] = result as RuleNode[]
          if (nodeVisualisation.length >= 1) {
            showViz(context, nodeVisualisation[0])
          } else {
            outputChannel.appendLine("Can't visualise, none available")
          }
          return result
        }
        // TODO: else show pop up to client
      },
    },
  }

  outputChannel.appendLine(
    `[client] Starting server from the client: ${serverCmd}`
  )

  // on Button. the button is at the bottom right of the status bar.
  const button = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    100
  )
  button.command = command.showVisualisation
  button.text = 'Update Diagram'
  button.tooltip = 'Show visualisation'
  button.show()

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
