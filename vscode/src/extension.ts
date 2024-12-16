/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import { ExtensionContext, workspace, window } from "vscode";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const langId = "l4";
  const langName = "jl4 LSP";
  const outputChannel: vscode.OutputChannel = window.createOutputChannel(
    langName,
    langId
  );
  // The server is implemented in node
  const serverCmd: string =
    workspace.getConfiguration("jl4").get("serverExecutablePath") ??
    "jl4-lsp";
  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  const serverOptions: ServerOptions = {
    run: { command: serverCmd },
    debug: {
      command: serverCmd,
    },
  };

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: "file", language: langId, pattern: "**/*" }],
    diagnosticCollectionName: langName,
    revealOutputChannelOn: RevealOutputChannelOn.Never,
    outputChannel,
    outputChannelName: langName,
  };

  outputChannel.appendLine(`[client] Starting server from the client: ${serverCmd}`);

  // Create the language client and start the client.
  client = new LanguageClient(langId, langName, serverOptions, clientOptions);
  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
