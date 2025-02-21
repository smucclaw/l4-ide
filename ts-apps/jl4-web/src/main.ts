/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2024 TypeFox and others.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as monaco from "@codingame/monaco-vscode-editor-api";
import { initServices } from "monaco-languageclient/vscode/services";
import { LogLevel } from "@codingame/monaco-vscode-api";
import {
  CloseAction,
  ErrorAction,
  MessageTransports,
  type Middleware,
} from "vscode-languageclient/browser.js";
import { MonacoLanguageClient } from "monaco-languageclient";
import { ConsoleLogger } from "monaco-languageclient/tools";
import {
  WebSocketMessageReader,
  WebSocketMessageWriter,
  toSocket,
} from "vscode-ws-jsonrpc";
import { configureDefaultWorkerFactory } from "monaco-editor-wrapper/workers/workerLoaders";

export const runClient = async () => {
  const logger = new ConsoleLogger(LogLevel.Debug);
  const htmlContainer = document.getElementById("jl4-editor")!;
  await initServices(
    {
      loadThemes: true,
      userConfiguration: {
        json: JSON.stringify({
          "editor.semanticHighlighting.enabled": true,
          "editor.experimental.asyncTokenization": true,
        }),
      },
    },
    {
      htmlContainer,
      logger,
    },
  );

  configureDefaultWorkerFactory(logger);

  // register the jl4 language with Monaco
  monaco.languages.register({
    id: "jl4",
    extensions: [".jl4"],
    aliases: ["JL4", "jl4"],
  });

  monaco.editor.defineTheme("jl4Theme", {
    base: "vs",
    inherit: true,
    rules: [
      { token: "decorator", foreground: "ffbd33" }, // for annotations
    ],
    encodedTokensColors: [],
    colors: {},
  });

  monaco.editor.create(htmlContainer, {
    value: britishCitizen,
    language: "jl4",
    automaticLayout: true,
    wordBasedSuggestions: "off",
    theme: "jl4Theme",
    "semanticHighlighting.enabled": true,
  });

  initWebSocketAndStartClient("ws://localhost:5007", logger);
};

/** parameterized version , support all languageId */
export const initWebSocketAndStartClient = (
  url: string,
  logger: ConsoleLogger,
): WebSocket => {
  const webSocket = new WebSocket(url);
  webSocket.onopen = () => {
    const socket = toSocket(webSocket);
    const reader = new WebSocketMessageReader(socket);
    const writer = new WebSocketMessageWriter(socket);
    const languageClient = createLanguageClient(logger, {
      reader,
      writer,
    });
    languageClient.start();
    reader.onClose(() => languageClient.stop());
  };
  return webSocket;
};

export const createLanguageClient = (
  logger: ConsoleLogger,
  messageTransports: MessageTransports,
): MonacoLanguageClient => {
  return new MonacoLanguageClient({
    name: "JL4 Language Client",
    clientOptions: {
      // use a language id as a document selector
      documentSelector: ["jl4"],
      // disable the default error handler
      errorHandler: {
        error: () => ({ action: ErrorAction.Continue }),
        closed: () => ({ action: CloseAction.DoNotRestart }),
      },
      middleware: mkMiddleware(logger),
    },
    // create a language client connection from the JSON RPC connection on demand
    messageTransports,
  });
};

function mkMiddleware(logger: ConsoleLogger): Middleware {
  return {
    executeCommand: async (command, args, next) => {
      logger.debug(`== trying to execute command ${command}`);
      // FIXME: he we can actually run everything that we also run in the vscode extension
      const response = await next(command, args);
      logger.debug(
        `== received response from language server ${JSON.stringify(response)}`,
      );
    },
  };
}

const britishCitizen = `
§ \`Assumptions\`

ASSUME Person IS A TYPE
ASSUME \`mother of\` IS A FUNCTION FROM Person TO Person
ASSUME \`father of\` IS A FUNCTION FROM Person TO Person

ASSUME \`is born in the United Kingdom after commencement\` IS A FUNCTION FROM Person TO BOOLEAN
ASSUME \`is born in a qualifying territory after the appointed day\` IS A FUNCTION FROM Person TO BOOLEAN
ASSUME \`is settled in the United Kingdom\` IS A FUNCTION FROM Person TO BOOLEAN
ASSUME \`is settled in the qualifying territory in which the person is born\` IS A FUNCTION FROM Person TO BOOLEAN

§ \`Expanded version\`

GIVEN p IS A Person
GIVETH A BOOLEAN
DECIDE \`is a British citizen\` IS
         \`is born in the United Kingdom after commencement\` p
      OR \`is born in a qualifying territory after the appointed day\` p
  AND -- when the person is born ...
            \`is a British citizen\` OF \`father of\` p
         OR \`is a British citizen\` OF \`mother of\` p
      OR    \`is settled in the United Kingdom\` OF \`father of\` p
         OR \`is settled in the United Kingdom\` OF \`mother of\` p
      OR    \`is settled in the qualifying territory in which the person is born\` OF \`father of\` p
         OR \`is settled in the qualifying territory in which the person is born\` OF \`mother of\` p

§ \`Version using a local auxiliary declaration\`

GIVEN p IS A Person
GIVETH A BOOLEAN
DECIDE \`is a British citizen (local)\` IS
         \`is born in the United Kingdom after commencement\` p
      OR \`is born in a qualifying territory after the appointed day\` p
  AND -- when the person is born ...
         \`father or mother\` \`is a British citizen\`
      OR \`father or mother\` \`is settled in the United Kingdom\`
      OR \`father or mother\` \`is settled in the qualifying territory in which the person is born\`
      WHERE
        \`father or mother\` property MEANS
             property OF \`father of\` p
          OR property OF \`mother of\` p

§ \`Version using a global auxiliary declaration\`

\`for father or mother of\` person property MEANS
      property OF \`father of\` person
   OR property OF \`mother of\` person

GIVEN p IS A Person
GIVETH A BOOLEAN
DECIDE \`is a British citizen (variant)\` IS
         \`is born in the United Kingdom after commencement\` p
      OR \`is born in a qualifying territory after the appointed day\` p
  AND -- when the person is born ...
         \`for father or mother of\` p \`is a British citizen (variant)\`
      OR \`for father or mother of\` p \`is settled in the United Kingdom\`
      OR \`for father or mother of\` p \`is settled in the qualifying territory in which the person is born\`
`;
