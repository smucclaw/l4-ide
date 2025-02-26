<script lang="ts">
  import { onMount } from "svelte";
  let editorElement: HTMLDivElement;
  import {
  LadderFlow,
    LirContext,
    LirRegistry,
    VizDeclLirSource,
    type DeclLirNode,
    type LirRootType,
  } from "@repo/decision-logic-visualizer";
  import {
    type VisualizeDecisionLogicIRInfo,
    type FunDecl,
  } from "@repo/viz-expr";
  import { type MessageTransports } from "vscode-languageclient";
  import { type ConsoleLogger } from "monaco-languageclient/tools";

  /**************************
      Set up Lir
  ****************************/

  const registry = new LirRegistry();
  const context = new LirContext();
  const nodeInfo = { registry, context };

  let vizDecl: undefined | FunDecl = $state(undefined);
  let declLirNode: DeclLirNode | undefined = $derived(
    vizDecl && VizDeclLirSource.toLir(nodeInfo, vizDecl)
  );
  let declLabel = $derived(
    declLirNode && (declLirNode as DeclLirNode).getLabel(context)
  );
  $effect(() => {
    if (declLirNode) {
      registry.setRoot(context, "VizDecl" as LirRootType, declLirNode);
    }
  });

  // /**************************
  //       Monadco
  // ****************************/

  onMount(async () => {
    const monaco = await import("@codingame/monaco-vscode-editor-api");
    const { initServices } = await import(
      "monaco-languageclient/vscode/services"
    );
    const { LogLevel } = await import("@codingame/monaco-vscode-api");
    const { CloseAction, ErrorAction } = await import(
      "vscode-languageclient/browser.js"
    );
    const { MonacoLanguageClient } = await import("monaco-languageclient");
    const { WebSocketMessageReader, WebSocketMessageWriter, toSocket } =
      await import("vscode-ws-jsonrpc");
    const { configureDefaultWorkerFactory } = await import(
      "monaco-editor-wrapper/workers/workerLoaders"
    );
    const { ConsoleLogger } = await import("monaco-languageclient/tools");
    const { Schema } = await import("effect");
    const { VisualizeDecisionLogicIRInfo } = await import("@repo/viz-expr");

    const backendUrl =
      import.meta.env.VITE_BACKEND_URL || "ws://localhost:5007";

    const runClient = async () => {
      const logger = new ConsoleLogger(LogLevel.Debug);

      await initServices(
        {
          loadThemes: true,
          userConfiguration: {
            json: JSON.stringify({
              "editor.semanticHighlighting.enabled": true,
              "editor.experimental.asyncTokenization": true,
            }),
          },
          serviceOverrides: {},
        },
        {
          htmlContainer: editorElement,
          logger,
        }
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

      monaco.editor.create(editorElement, {
        value: britishCitizen,
        language: "jl4",
        automaticLayout: true,
        wordBasedSuggestions: "off",
        theme: "jl4Theme",
        "semanticHighlighting.enabled": true,
      });

      initWebSocketAndStartClient(backendUrl, logger);
    };

    /** parameterized version , support all languageId */
    const initWebSocketAndStartClient = (
      url: string,
      logger: ConsoleLogger
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

    const createLanguageClient = (
      logger: ConsoleLogger,
      messageTransports: MessageTransports
    ) => {
      return new MonacoLanguageClient({
        name: "JL4 Language Client",
        clientOptions: {
          // use a language id as a document selector
          documentSelector: ["jl4"],
          // disable the default error handler
          errorHandler: {
            error: () => ({ action: ErrorAction.Continue }),
            closed: () => ({ action: CloseAction.Restart }),
          },
          middleware: mkMiddleware(logger),
        },
        // create a language client connection from the JSON RPC connection on demand
        messageTransports,
      });
    };

    function mkMiddleware(logger: ConsoleLogger) {
      return {
        executeCommand: async (command: any, args: any, next: any) => {
          logger.debug(`== trying to execute command ${command}`);
          // FIXME: he we can actually run everything that we also run in the vscode extension
          const response = await next(command, args);
          logger.debug(
            `== received response from language server ${JSON.stringify(response)}`
          );
          const decode = Schema.decodeUnknownSync(VisualizeDecisionLogicIRInfo);

          const vizProgramInfo: VisualizeDecisionLogicIRInfo = decode(response);

          vizDecl = vizProgramInfo.program;
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
    await runClient();
  });
</script>

<div class="jl4-container">
  <div id="jl4-editor" bind:this={editorElement}></div>
  <div id="jl4-webview">
    <h1>{declLabel}</h1>

    {#if vizDecl && declLirNode}
      {#key declLirNode}
        <div class="flash-on-update visualization-container">
          <LadderFlow {context} node={declLirNode} />
        </div>
      {/key}
    {/if}

    <style>
      @keyframes flash {
        0%,
        90% {
          background-color: hsl(var(--neutral));
        }
        50% {
          background-color: hsl(var(--muted));
        }
      }

      .flash-on-update {
        animation: flash 0.6s;
      }

      h1 {
        margin-top: 10px;
        padding-bottom: 2px;
        font-size: 1.5rem;
        line-height: 1.1rem;
        font-weight: 700;
        text-align: center;
      }
    </style>
  </div>
</div>
