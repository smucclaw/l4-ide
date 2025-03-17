<script lang="ts">
  import { onMount, onDestroy } from 'svelte'
  import {
    LadderFlow,
    LirContext,
    LirRegistry,
    VizDeclLirSource,
    type DeclLirNode,
    type LirRootType,
  } from '@repo/decision-logic-visualizer'
  import {
    makeVizInfoDecoder,
    type VisualizeDecisionLogicIRInfo,
  } from '@repo/viz-expr'
  import {
    type MessageTransports,
    type Middleware,
  } from 'vscode-languageclient'
  import { type ConsoleLogger } from 'monaco-languageclient/tools'
  import * as vscode from 'vscode'
  import * as monaco from '@codingame/monaco-vscode-editor-api'
  import { debounce } from '$lib/utils'
  import * as Resizable from '$lib/components/ui/resizable/index.js'

  /***********************************
    Persistent-session-related vars
  ************************************/

  // `persistSession` does not need to be reactive
  let persistSession: undefined | (() => Promise<void>) = undefined
  const sessionUrl = import.meta.env.VITE_SESSION_URL || 'http://localhost:5008'

  /***********************************
        UI-related vars
  ************************************/

  /* editorElement does not need to be reactive */
  let editorElement: HTMLDivElement
  let errorMessage: string | undefined = $state(undefined)

  /***********************************
          Set up Lir
  ************************************/

  const lirRegistry = new LirRegistry()
  const context = new LirContext()
  const nodeInfo = { registry: lirRegistry, context }

  let declLirNode: DeclLirNode | undefined = $state(undefined)
  let funName = $derived(
    declLirNode && (declLirNode as DeclLirNode).getFunName(context)
  )

  /******************************
      VizInfo Payload Decoder
  *******************************/

  const decodeVizInfo = makeVizInfoDecoder()

  /**********************************
      Debounced run visualize cmd
  ***********************************/

  const debouncedVisualize = debounce(async (uri: string) => {
    await vscode.commands.executeCommand(
      // TODO: Should probably put the command in the viz-expr package
      'l4.visualize',
      uri
    )
  }, 150)

  // /**************************
  //       Monadco
  // ****************************/

  let editor: monaco.editor.IStandaloneCodeEditor | undefined

  onMount(async () => {
    const { initServices } = await import(
      'monaco-languageclient/vscode/services'
    )
    const { LogLevel } = await import('@codingame/monaco-vscode-api')
    const { CloseAction, ErrorAction } = await import(
      'vscode-languageclient/browser.js'
    )
    const { MonacoLanguageClient } = await import('monaco-languageclient')
    const { WebSocketMessageReader, WebSocketMessageWriter, toSocket } =
      await import('vscode-ws-jsonrpc')
    const { configureDefaultWorkerFactory } = await import(
      'monaco-editor-wrapper/workers/workerLoaders'
    )
    const { ConsoleLogger } = await import('monaco-languageclient/tools')

    const websocketUrl =
      import.meta.env.VITE_SOCKET_URL || 'ws://localhost:5007'

    const runClient = async () => {
      const logger = new ConsoleLogger(LogLevel.Debug)

      await initServices(
        {
          loadThemes: true,
          userConfiguration: {
            json: JSON.stringify({
              'editor.semanticHighlighting.enabled': true,
              'editor.experimental.asyncTokenization': true,
              'editor.lightbulb.enabled': 'on',
            }),
          },
          serviceOverrides: {},
        },
        {
          htmlContainer: editorElement,
          logger,
        }
      )

      configureDefaultWorkerFactory(logger)

      // register the jl4 language with Monaco
      monaco.languages.register({
        id: 'jl4',
        extensions: ['.jl4'],
        aliases: ['JL4', 'jl4'],
      })

      monaco.editor.defineTheme('jl4Theme', {
        base: 'vs',
        inherit: true,
        rules: [
          { token: 'decorator', foreground: 'ffbd33' }, // for annotations
        ],
        encodedTokensColors: [],
        colors: {},
      })

      editor = monaco.editor.create(editorElement, {
        value: britishCitizen,
        language: 'jl4',
        automaticLayout: true,
        wordBasedSuggestions: 'off',
        theme: 'jl4Theme',
        'semanticHighlighting.enabled': true,
      })

      const ownUrl: URL = new URL(window.location.href)
      const sessionid: string | null = ownUrl.searchParams.get('id')
      if (sessionid) {
        const response = await fetch(`${sessionUrl}?id=${sessionid}`)
        logger.debug('sent GET for session')
        const prog = await response.json()
        if (prog) {
          editor.setValue(prog)
        }
      }

      persistSession = async () => {
        if (!editor) return

        const bufferContent: string = editor.getValue()
        const ownUrl: URL = new URL(window.location.href)
        const sessionid: string | null = ownUrl.searchParams.get('id')
        if (sessionid) {
          await fetch(sessionUrl, {
            method: 'PUT',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
              jl4program: bufferContent,
              sessionid: sessionid,
            }),
          })
          logger.debug('sent PUT for session')
        } else {
          const response = await fetch(sessionUrl, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(bufferContent),
          })
          logger.debug('sent POST for session')
          const newSessionId = await response.json()
          if (newSessionId) {
            ownUrl.searchParams.set('id', newSessionId.toString())
            history.pushState(null, '', ownUrl)
          } else {
            console.error(`response was not present`)
          }
        }
      }

      initWebSocketAndStartClient(websocketUrl, logger)
    }

    /** parameterized version , support all languageId */
    const initWebSocketAndStartClient = (
      url: string,
      logger: ConsoleLogger
    ): WebSocket => {
      const webSocket = new WebSocket(url)
      webSocket.onopen = () => {
        const socket = toSocket(webSocket)
        const reader = new WebSocketMessageReader(socket)
        const writer = new WebSocketMessageWriter(socket)
        const languageClient = createLanguageClient(logger, {
          reader,
          writer,
        })
        languageClient.start()
        reader.onClose(() => languageClient.stop())
      }
      return webSocket
    }

    const createLanguageClient = (
      logger: ConsoleLogger,
      messageTransports: MessageTransports
    ) => {
      return new MonacoLanguageClient({
        name: 'JL4 Language Client',
        clientOptions: {
          // use a language id as a document selector
          documentSelector: ['jl4'],
          // disable the default error handler
          errorHandler: {
            error: () => ({ action: ErrorAction.Continue }),
            closed: () => ({ action: CloseAction.Restart }),
          },
          middleware: mkMiddleware(logger),
        },
        // create a language client connection from the JSON RPC connection on demand
        messageTransports,
      })
    }

    function mkMiddleware(logger: ConsoleLogger): Middleware {
      return {
        /* eslint-disable-next-line  @typescript-eslint/no-explicit-any */
        executeCommand: async (command: any, args: any, next: any) => {
          logger.debug(`trying to execute command ${command}`)
          const response = await next(command, args)

          logger.debug(
            `received response from language server ${JSON.stringify(response)}`
          )

          const decoded = decodeVizInfo(response)
          // TODO: Can improve this later
          switch (decoded._tag) {
            case 'Right':
              if (decoded.right) {
                const vizProgramInfo: VisualizeDecisionLogicIRInfo =
                  decoded.right
                declLirNode = VizDeclLirSource.toLir(
                  nodeInfo,
                  vizProgramInfo.program
                )
                lirRegistry.setRoot(
                  context,
                  'VizDecl' as LirRootType,
                  declLirNode
                )
                logger.debug(
                  'New declLirNode ',
                  (declLirNode as DeclLirNode).getId().toString()
                )
              }
              break
            case 'Left':
              errorMessage = `Internal error: Failed to decode response. ${decoded?.left}`
              break
          }
        },
        didChange: async (event, next) => {
          await next(event)
          // YM: If the http calls in persistSession() don't succeed (e.g. cos the web sessions server isn't loaded),
          // the rest of the didChange callback does not run, at least not when testing on localhost.
          if (persistSession) {
            await persistSession()
          }

          // YM: I don't like using middleware when, as far as I can see, we aren't really using the intercepting capabilities of middleware.
          // Also, I don't like how I'm lumping different things / concerns in the didChange handler.
          // But I guess this is fine for now. I should just put in the effort to refactor it if I really care about this.
          debouncedVisualize(event.document.uri.toString())
        },
      }
    }
    await runClient()
  })

  onDestroy(() => {
    // YM: I'm not sure that this is necessary --- just adding it for now because I've seen examples on GitHub that do this.
    // I'll look into this more in the future.
    if (editor) {
      editor.dispose()
      editor = undefined
    }
    // TODO: May also want to clean up the websocket, but not sure if necessary
  })

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
      OR \`for father or mother of\` p \`is settled in the qualifying territory in which the person is born\``
</script>

<Resizable.PaneGroup direction="horizontal">
  <Resizable.Pane defaultSize={60}>
    <div id="jl4-editor" class="h-full" bind:this={editorElement}></div>
  </Resizable.Pane>
  <Resizable.Handle style="width: 10px;" />
  <Resizable.Pane>
    <div id="jl4-webview" class="h-full bg-white">
      <div class="header">
        <h1>{funName}</h1>
      </div>
      {#if declLirNode}
        <!-- TODO: Think more about whether to use #key -- which destroys and rebuilds the component --- or have flow-base work with the reactive node prop -->
        {#key declLirNode}
          <div class="slightly-shorter-than-full-viewport-height pb-2">
            <LadderFlow {context} node={declLirNode} lir={lirRegistry} />
          </div>
        {/key}
      {/if}
      {#if errorMessage}
        {errorMessage}
      {/if}
    </div>
  </Resizable.Pane>
</Resizable.PaneGroup>

<style lang="postcss">
  @reference "tailwindcss"

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

  .header {
    padding-top: 3px;
    padding-bottom: 8px;
  }

  .slightly-shorter-than-full-viewport-height {
    height: 95svh;
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
