<script lang="ts">
  import { onMount } from "svelte";
  import {
    LadderFlow,
    LirContext,
    LirRegistry,
    VizDeclLirSource,
    type DeclLirNode,
    type LirRootType,
  } from "@repo/decision-logic-visualizer";
  import {
    makeVizInfoDecoder,
    type FunDecl,
    type VisualizeDecisionLogicIRInfo,
  } from "@repo/viz-expr";
  import { type MessageTransports, type Middleware } from "vscode-languageclient";
  import { type ConsoleLogger } from "monaco-languageclient/tools";

  /* eslint-disable-next-line editorElement does not need to be reactive */
  let editorElement: HTMLDivElement;
  let errorMessage: string | undefined = $state(undefined);

  /**************************
      Set up Lir
  ****************************/

  const lirRegistry = new LirRegistry();
  const context = new LirContext();
  const nodeInfo = { registry: lirRegistry, context };

  let vizDecl: undefined | FunDecl = $state(undefined);
  let persistSession: undefined | (() => Promise<void>) = $state(undefined);
  let declLirNode: DeclLirNode | undefined = $derived(
    vizDecl && VizDeclLirSource.toLir(nodeInfo, vizDecl)
  );
  let funName = $derived(
    declLirNode && (declLirNode as DeclLirNode).getFunName(context)
  );
  // TODO: YM has some ideas for how to improve / clean this up
  $effect(() => {
    if (declLirNode) {
      lirRegistry.setRoot(context, "VizDecl" as LirRootType, declLirNode);
    }
  });

  /******************************
      VizInfo Payload Decoder
  *******************************/
  const decodeVizInfo = makeVizInfoDecoder();

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

    const websocketUrl =
      import.meta.env.VITE_SOCKET_URL || 'ws://localhost:5007';

    const sessionUrl =
      import.meta.env.VITE_SESSION_URL || 'http://localhost:5008';

    const runClient = async () => {
      const logger = new ConsoleLogger(LogLevel.Debug);

      await initServices(
        {
          loadThemes: true,
          userConfiguration: {
            json: JSON.stringify({
              "editor.semanticHighlighting.enabled": true,
              "editor.experimental.asyncTokenization": true,
              'editor.lightbulb.enabled': 'on',
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

      const editor = monaco.editor.create(editorElement, {
        value: britishCitizen,
        language: "jl4",
        automaticLayout: true,
        wordBasedSuggestions: "off",
        theme: "jl4Theme",
        "semanticHighlighting.enabled": true,
      });

      const ownUrl: URL = new URL(window.location.href);
      const sessionid: string | null = ownUrl.searchParams.get('id');
      if (sessionid) {
         const response = await fetch(`${sessionUrl}?id=${sessionid}`)
         logger.debug("sent GET for session")
         const prog = await response.json();
         if (prog) {
           editor.setValue(prog);
         }
      }

      persistSession = async () => {
        const bufferContent: string = editor.getValue();
        const ownUrl: URL = new URL(window.location.href);
        const sessionid: string | null = ownUrl.searchParams.get('id');
        if (sessionid) {
          await fetch(sessionUrl, {
            method: 'PUT',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify({jl4program: bufferContent, sessionid: sessionid})
          });
          logger.debug("sent PUT for session")
        } else {
          const response = await fetch(sessionUrl, {
            method: 'POST',
            headers: {'Content-Type': 'application/json'},
            body: JSON.stringify(bufferContent)
          });
          logger.debug("sent POST for session")
          const newSessionId = await response.json()
          if (newSessionId) {
            ownUrl.searchParams.set('id', newSessionId.toString())
            history.pushState(null, '', ownUrl)
          } else {
            console.error(`response was not present`)
          }
        };
      }

      initWebSocketAndStartClient(websocketUrl, logger);
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

    function mkMiddleware(logger: ConsoleLogger): Middleware {
      return {
        executeCommand: async (command: any, args: any, next: any) => {
          logger.debug(`trying to execute command ${command}`);
          const response = await next(command, args);

          logger.debug(
            `received response from language server ${JSON.stringify(response)}`
          );

          const decoded = decodeVizInfo(response);
          // TODO: Can improve this later
          switch (decoded._tag) {
            case "Right":
              if (decoded.right) {
                const vizProgramInfo: VisualizeDecisionLogicIRInfo = decoded.right;
                vizDecl = vizProgramInfo.program;
              }
              break;
            case "Left":
              errorMessage = `Internal error: Failed to decode response. ${decoded?.left}`;
              break;
          }
        },
        didChange: async (event, next) => {
          await next(event)
          if (persistSession) {
            await persistSession()
          }
        }
      };
    }
    await runClient();
  });

  const britishCitizen = `§ \`Assumptions\`

ASSUME Person IS A TYPE
ASSUME \`mother of\` IS A FUNCTION FROM Person TO Person
ASSUME \`father of\` IS A FUNCTION FROM Person TO Person

ASSUME \`is born in the United Kingdom after commencement\` IS A FUNCTION FROM Person TO BOOLEAN
ASSUME \`is born in a qualifying territory after the appointed day\` IS A FUNCTION FROM Person TO BOOLEAN
ASSUME \`is settled in the United Kingdom\` IS A FUNCTION FROM Person TO BOOLEAN
ASSUME \`is settled in the qualifying territory in which the person is born\` IS A FUNCTION FROM Person TO BOOLEAN

§ \`The British Citizen Act\`

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
      OR \`for father or mother of\` p \`is settled in the qualifying territory in which the person is born\``;
</script>

<div class="jl4-container">
  <div id="jl4-editor" bind:this={editorElement}></div>
  <div id="jl4-webview" class="panel">
    <div class="header">
      <h1>{funName}</h1>
    </div>
    {#if vizDecl && declLirNode}
      {#key declLirNode}
        <div
          class="flash-on-update visualization-container slightly-shorter-than-full-viewport-height"
        >
          <LadderFlow {context} node={declLirNode} lir={lirRegistry}/>
        </div>
      {/key}
    {/if}
    {#if errorMessage}
      {errorMessage}
    {/if}
  </div>
</div>

<style>
  @keyframes flash {
    0%,
    90% {
      background-color: hsl(var(--neutral));
    }
    50% {
      background-color: oklch(
        0.951 0.026 236.824
      ); /* Tailwind's --color-sky-100 */
    }
  }

  .panel {
    background-color: white;
  }

  .header {
    padding-top: 3px;
    padding-bottom: 8px;
  }

  .slightly-shorter-than-full-viewport-height {
    height: 95svh;
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
