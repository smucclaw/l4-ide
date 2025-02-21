/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2024 TypeFox and others.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as monaco from '@codingame/monaco-vscode-editor-api';
import { initServices } from 'monaco-languageclient/vscode/services';
import { LogLevel } from '@codingame/monaco-vscode-api';
import { CloseAction, ErrorAction, MessageTransports } from 'vscode-languageclient/browser.js';
import { MonacoLanguageClient } from 'monaco-languageclient';
import { ConsoleLogger } from 'monaco-languageclient/tools';
import { WebSocketMessageReader, WebSocketMessageWriter, toSocket } from 'vscode-ws-jsonrpc';
import { configureDefaultWorkerFactory } from 'monaco-editor-wrapper/workers/workerLoaders';

export const runClient = async () => {
    const logger = new ConsoleLogger(LogLevel.Debug);
    const htmlContainer = document.getElementById('monaco-editor-root')!;
    await initServices({
        userConfiguration: {
            json: JSON.stringify({
                'editor.experimental.asyncTokenization': true
            })
        },
    }, {
        htmlContainer,
        logger
    });

    configureDefaultWorkerFactory(logger);

    // register the jl4 language with Monaco
    monaco.languages.register({
        id: 'jl4',
        extensions: ['.jl4'],
        aliases: ['JL4', 'jl4'],
    });

    // monaco.editor.defineTheme('myCustomTheme', {
    //     base: 'vs',
    //     inherit: true,
    //     rules: [
    //         { token: 'comment'      , foreground: 'aaaaaa', fontStyle: 'italic' },
    //         { token: 'keyword'      , foreground: 'ce63eb' },
    //         { token: 'property'     , foreground: '3e5bbf' },
    //         { token: 'label'        , foreground: '615a60' },
    //         { token: 'type.static'  , fontStyle: 'bold' },
    //         { token: 'class.static' , foreground: 'ff0000', fontStyle: 'bold' }
    //     ],
    //     encodedTokensColors: [],
    //     colors:  {}
    // });

    // monaco.editor.setTheme('myCustomTheme')

    monaco.editor.create(htmlContainer, {
        value: "DECIDE foo IF TRUE",
        language: 'jl4',
        automaticLayout: true,
        wordBasedSuggestions: 'off',
        theme: 'vs-dark',
        'semanticHighlighting.enabled': true,
    });
    initWebSocketAndStartClient('ws://localhost:5007', logger);
};

/** parameterized version , support all languageId */
export const initWebSocketAndStartClient = (url: string, logger: ConsoleLogger): WebSocket => {
    const webSocket = new WebSocket(url);
    webSocket.onopen = () => {
        const socket = toSocket(webSocket);
        const reader = new WebSocketMessageReader(socket);
        const writer = new WebSocketMessageWriter(socket);
        const languageClient = createLanguageClient({
            reader,
            writer
        });
        languageClient.start();
        reader.onClose(() => languageClient.stop());
    };
    return webSocket;
};

export const createLanguageClient = (messageTransports: MessageTransports): MonacoLanguageClient => {
    return new MonacoLanguageClient({
        name: 'JL4 Language Client',
        clientOptions: {
            // use a language id as a document selector
            documentSelector: ['jl4'],
            // disable the default error handler
            errorHandler: {
                error: () => ({ action: ErrorAction.Continue }),
                closed: () => ({ action: CloseAction.DoNotRestart })
            }
        },
        // create a language client connection from the JSON RPC connection on demand
        messageTransports
    });
};
