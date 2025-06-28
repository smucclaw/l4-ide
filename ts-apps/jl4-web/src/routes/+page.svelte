<script lang="ts">
  import { onMount, onDestroy } from 'svelte'
  import { SvelteToast, toast } from '@zerodevx/svelte-toast'
  import { debounce } from '$lib/utils'
  import * as Resizable from '$lib/components/ui/resizable/index.js'

  import type { MessageTransports, Middleware } from 'vscode-languageclient'
  import { type ConsoleLogger } from 'monaco-languageclient/tools'
  import * as vscode from 'vscode'
  import { createConverter as createCodeConverter } from 'vscode-languageclient/lib/common/codeConverter.js'
  import * as monaco from '@codingame/monaco-vscode-editor-api'
  import { monacoModuleWrapperForErrorLens } from '$lib/monaco-error-lens-helpers'

  import { MonacoL4LanguageClient } from '$lib/monaco-l4-language-client'
  import type { LadderBackendApi } from 'jl4-client-rpc'
  import { LadderApiForMonaco } from '$lib/ladder-api-for-monaco'
  import { MonacoErrorLens } from '@ym-han/monaco-error-lens'

  import { defaultExample, type LegalExample } from '$lib/legal-examples'
  import ExampleSelector from '$lib/components/example-selector.svelte'

  import {
    LadderFlow,
    LirContext,
    LirRegistry,
    type FunDeclLirNode,
    LadderEnv,
    VizDeclLirSource,
  } from 'l4-ladder-visualizer'
  import {
    makeVizInfoDecoder,
    type RenderAsLadderInfo,
    type VersionedDocId,
  } from '@repo/viz-expr'

  /***********************************
    Persistent-session-related vars
  ************************************/

  // `persistSession` does not need to be reactive
  let persistSession: undefined | (() => Promise<string | undefined>) =
    undefined
  const sessionUrl = import.meta.env.VITE_SESSION_URL || 'http://localhost:5008'

  let persistButtonBlocked = $state(false)

  // Set from URL search params as early as possible to avoid flicker
  const ownUrl = new URL(window.location.href)

  let showFrame = $state(!ownUrl.searchParams.has('standalone'))
  let showExamples = $state(!ownUrl.searchParams.has('no-examples'))
  let showSidebar = $state(window.innerWidth < 1024 ? false : !ownUrl.searchParams.has('no-examples'))

  /***********************************
        UI-related vars
  ************************************/

  /* svelte doesn't realize that there will be a div ready for use in either of the branches so 
  we force it to accept it */
  let editorElement: HTMLDivElement = $state(
    undefined as unknown as HTMLDivElement
  )
  let errorMessage: string | undefined = $state(undefined)

  /***********************************
          Set up Lir
  ************************************/

  const lirRegistry = new LirRegistry()
  const context = new LirContext()
  const nodeInfo = { registry: lirRegistry, context }

  /**************************
      FunDeclLirNode maker
  ****************************/

  const LADDER_VIZ_ROOT_TYPE = 'VizFunDecl'

  let backendApi: LadderBackendApi
  let ladderEnv: LadderEnv

  // See vscode webview for the rationale for this
  const placeholderAlwaysPendingPromise = new Promise<{
    funDeclLirNode: FunDeclLirNode
    env: LadderEnv
  }>(() => {})
  let renderLadderPromise: Promise<{
    funDeclLirNode: FunDeclLirNode
    env: LadderEnv
  }> = $state(placeholderAlwaysPendingPromise)

  async function makeFunDeclLirNodeAndSetLirRoot(
    ladderEnv: LadderEnv,
    renderLadderInfo: RenderAsLadderInfo
  ) {
    const funDeclLirNode = await VizDeclLirSource.toLir(
      nodeInfo,
      ladderEnv,
      renderLadderInfo.funDecl
    )
    lirRegistry.setRoot(context, LADDER_VIZ_ROOT_TYPE, funDeclLirNode)
    return { env: ladderEnv, funDeclLirNode }
  }

  /******************************
      VizInfo Payload Decoder
  *******************************/

  const decodeVizInfo = makeVizInfoDecoder()

  /**********************************
      Debounced run visualize cmd
  ***********************************/

  const debouncedVisualize = debounce(async (verDocId: VersionedDocId) => {
    await vscode.commands.executeCommand(
      // TODO: Should probably put the command in the viz-expr package
      'l4.visualize',
      verDocId
    )
  }, 150)

  // /**************************
  //       Monadco
  // ****************************/

  const code2ProtocolConverter = createCodeConverter()

  let editor: monaco.editor.IStandaloneCodeEditor | undefined
  let monacoL4LangClient: MonacoL4LanguageClient | undefined
  let monacoErrorLens: MonacoErrorLens | undefined

  // TODO: Need to refactor this --- too long
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
        colors: {
          // The following is the hex version of the --primary css variable in the default ladder diagram theme (modulo rounding error)
          // TODO: Would be better to reference our --primary css variable directly if possible
          'editor.foreground': '#104e64',
          foreground: '#104e64',
        },
      })

      editor = monaco.editor.create(editorElement, {
        value: defaultExample.content,
        language: 'jl4',
        automaticLayout: true,
        wordBasedSuggestions: 'off',
        theme: 'jl4Theme',
        'semanticHighlighting.enabled': true,
        glyphMargin: true, // Required for gutter icons
      })

      // Set up Monaco Error Lens
      monacoErrorLens = new MonacoErrorLens(
        editor,
        monacoModuleWrapperForErrorLens,
        {
          enableInlineMessages: true,
          enableLineHighlights: true,
          enableGutterIcons: true,
          followCursor: 'allLines',
          messageTemplate: '{message}',
          maxMessageLength: 150,
          updateDelay: 200,
        }
      )

      const sessionid: string | null = ownUrl.searchParams.get('id')
      if (sessionid) {
        const response = await fetch(`${sessionUrl}?id=${sessionid}`)
        logger.debug('sent GET for session')
        if (response.ok) {
          const prog = await response.json()
          if (prog) {
            editor.setValue(prog)
          }
        } else {
          logger.debug('response was not okay - deleting search param')
          ownUrl.searchParams.delete('id')
          history.pushState(null, '', ownUrl)
        }
      }

      const programParam: string | null = ownUrl.searchParams.get('program')
      if (programParam) {
        try {
          const decoded = decodeURIComponent(programParam)
          editor.setValue(decoded)
        } catch (e) {
          logger.error(`could not decode program from url param, error: ${e}`)
        }
        ownUrl.searchParams.delete('program')
        history.replaceState(null, '', ownUrl)
      }

      persistSession = async () => {
        if (!editor) return undefined
        const bufferContent: string = editor.getValue()
        const response = await fetch(sessionUrl, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify(bufferContent),
        })
        logger.debug('sent POST for session')
        const newSessionId: string = await response.json()
        if (newSessionId) {
          logger.debug(`new session id: ${newSessionId}`)
          return newSessionId
        } else {
          logger.error(`response was not present`)
          return undefined
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
      webSocket.onopen = async () => {
        const socket = toSocket(webSocket)
        const reader = new WebSocketMessageReader(socket)
        const writer = new WebSocketMessageWriter(socket)
        const languageClient = createLanguageClient(logger, {
          reader,
          writer,
        })
        await languageClient.start()
        reader.onClose(() => {
          languageClient.dispose()
        })
      }
      return webSocket
    }

    /**********************************
           makeLadderFlow
    ***********************************/

    const makeLadderFlow = async (
      ladderInfo: RenderAsLadderInfo
    ): Promise<void> => {
      // Re-make / update ladderEnv using the verDocId from the new ladderInfo,
      // just in case
      ladderEnv = LadderEnv.make(
        lirRegistry,
        ladderInfo.verDocId,
        backendApi,
        LADDER_VIZ_ROOT_TYPE
      )
      renderLadderPromise = makeFunDeclLirNodeAndSetLirRoot(
        ladderEnv,
        ladderInfo
      )
      await renderLadderPromise
    }

    /**********************************
           createLanguageClient
    ***********************************/

    /** Make MonacoL4LanguageClient and LadderBackendApi */
    const createLanguageClient = (
      logger: ConsoleLogger,
      messageTransports: MessageTransports
    ) => {
      const internalClient = new MonacoLanguageClient({
        name: 'JL4 Language Client',
        clientOptions: {
          // use a language id as a document selector
          documentSelector: ['jl4'],
          // disable the default error handler
          errorHandler: {
            error: () => ({ action: ErrorAction.Continue }),
            closed: () => ({ action: CloseAction.Restart }),
          },
          middleware: showFrame
            ? mkMiddleware(logger, makeLadderFlow)
            : undefined,
        },
        // create a language client connection from the JSON RPC connection on demand
        messageTransports,
      })

      monacoL4LangClient = new MonacoL4LanguageClient(internalClient)

      /**********************************
            Init LadderBackendApi
      ***********************************/
      backendApi = new LadderApiForMonaco(monacoL4LangClient, makeLadderFlow)

      return monacoL4LangClient
    }

    function mkMiddleware(
      logger: ConsoleLogger,
      makeLadderFlow: (ladderInfo: RenderAsLadderInfo) => Promise<void>
    ): Middleware {
      return {
        /* eslint-disable-next-line @typescript-eslint/no-explicit-any */
        executeCommand: async (command: any, args: any, next: any) => {
          logger.debug(`trying to execute command ${command}`)
          const responseFromLangServer = await next(command, args)

          logger.debug(
            `received response from language server ${JSON.stringify(responseFromLangServer)}`
          )
          if (responseFromLangServer === null) {
            logger.info('language server returned `null`, so doing nothing')
            return
          }

          const decoded = decodeVizInfo(responseFromLangServer)
          // TODO: Can improve this later
          switch (decoded._tag) {
            case 'Right':
              if (decoded.right) {
                const renderLadderInfo: RenderAsLadderInfo = decoded.right
                await makeLadderFlow(renderLadderInfo)
              }
              break
            case 'Left':
              errorMessage = `Internal error: Failed to decode response. Please report this to the JL4 developers. ${decoded?.left}`
              renderLadderPromise = Promise.reject(new Error(errorMessage)) // Ensure #await catches this
              break
          }
        },
        didChange: async (event, next) => {
          await next(event)

          const ownUrl: URL = new URL(window.location.href)
          if (ownUrl.searchParams.has('id')) {
            ownUrl.searchParams.delete('id')
            history.pushState(null, '', ownUrl)
          }

          if (persistButtonBlocked) {
            persistButtonBlocked = false
          }

          // YM: I don't like using middleware when, as far as I can see, we aren't really using the intercepting capabilities of middleware.
          // Also, I don't like how I'm lumping different things / concerns in the didChange handler.
          // But I guess this is fine for now. I should just put in the effort to refactor it if I really care about this.
          const verDocId: VersionedDocId =
            code2ProtocolConverter.asVersionedTextDocumentIdentifier(
              event.document
            )
          debouncedVisualize(verDocId)
        },
      }
    }
    await runClient()
  })

  onDestroy(() => {
    if (monacoErrorLens) {
      monacoErrorLens.dispose()
      monacoErrorLens = undefined
    }

    // YM: I'm not sure that this is necessary --- just adding it for now because I've seen examples on GitHub that do this.
    // I'll look into this more in the future.
    if (editor) {
      editor.dispose()
      editor = undefined
    }

    if (monacoL4LangClient) {
      monacoL4LangClient.dispose?.()
      monacoL4LangClient = undefined
    }
  })

  async function handlePersist() {
    if (!persistSession) return undefined

    persistButtonBlocked = true
    try {
      const sessionId = await persistSession()
      return sessionId
    } finally {
      persistButtonBlocked = false
    }
  }

  async function handleShare() {
    const sessionId = await handlePersist()
    if (sessionId) {
      const shareUrl = `${window.location.origin}${window.location.pathname}?id=${sessionId}`
      await navigator.clipboard.writeText(shareUrl)
      window.history.pushState(null, '', shareUrl)
      toast.push('Session persisted and share link copied to clipboard')
    } else {
      toast.push('Could not persist the file.')
    }
  }

  function handleExampleSelect(example: LegalExample) {
    if (showExamples && editor) {
      editor.setValue(example.content)

      if (window.innerWidth <= 1023) {
        showSidebar = false
      }
    }
  }
</script>

{#if showFrame}
  <div class="top-bar">
    <div>
      <h3>L4 Editor</h3>
    </div>
    {#if showExamples}
      <button
        class="fab fab-sidebar {showSidebar ? 'open' : ''}"
        onclick={() => (showSidebar = !showSidebar)}
        aria-label="Toggle sidebar"
      >
        <svg
          style="opacity: .7"
          width="22"
          height="22"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="1.4"
          stroke-linecap="round"
          stroke-linejoin="round"
        >
          <!-- Square with rounded corners -->
          <rect x="2.5" y="2.5" width="19" height="19" rx="4" />
          <!-- Sidebar divider (vertical line) -->
          <line x1="8.5" y1="3.5" x2="8.5" y2="20.5" />
          <!-- Arrowhead pointing left (no stem), shifted right for centering -->
          <polyline class="arrow-left" points="16,9 12,12 16,15" />
          <polyline class="arrow-right" points="13,9 17,12 13,15" />
        </svg>
      </button>
    {/if}
    <button
      class="fab fab-share"
      onclick={handleShare}
      aria-label="Share the current file"
      title="Share the current file"
      disabled={persistButtonBlocked}
    >
      <svg
        style="opacity: .6; font-size: 24px; vertical-align: middle; margin-right: .5rem"
        width="20"
        height="20"
        viewBox="0 0 24 24"
        fill="none"
        stroke="currentColor"
        stroke-width="1.6"
        stroke-linecap="round"
        stroke-linejoin="round"
        aria-hidden="true"
        focusable="false"
      >
        <!-- Circle for the share origin -->
        <circle cx="18" cy="5" r="3" />
        <!-- Circle for the left endpoint -->
        <circle cx="6" cy="12" r="3" />
        <!-- Circle for the right endpoint -->
        <circle cx="18" cy="19" r="3" />
        <!-- Line from left to top -->
        <line x1="8.59" y1="10.51" x2="15.42" y2="6.49" />
        <!-- Line from left to bottom -->
        <line x1="8.59" y1="13.49" x2="15.42" y2="17.51" />
      </svg>
      Share
    </button>
  </div>
  {#if showExamples && showSidebar}
    <div class="sidebar">
      <ExampleSelector onExampleSelect={handleExampleSelect} />
    </div>
  {/if}
  <div class="panes">
    <Resizable.PaneGroup direction="horizontal">
      <Resizable.Pane defaultSize={50}>
        <div id="jl4-editor" class="h-full" bind:this={editorElement}></div>
      </Resizable.Pane>
      <Resizable.Handle />
      <Resizable.Pane class="hidden lg:block">
        <div class="relative h-full ladder-border">
          <div id="jl4-webview" class="h-full max-w-[96%] mx-auto">
            {#await renderLadderPromise then ladder}
              {#key ladder.funDeclLirNode}
                <div class="slightly-shorter-than-full-viewport-height pb-1">
                  <LadderFlow
                    {context}
                    node={ladder.funDeclLirNode}
                    env={ladder.env}
                  />
                </div>
              {/key}
            {:catch error}
              <p>Error loading Ladder Diagram: {error.message}</p>
            {/await}
          </div>
        </div>
        <style>
          .ladder-border {
            border-top: 1px solid #999;
            border-right: 1px solid #999;
            border-bottom: 1px solid #999;
          }
        </style>
      </Resizable.Pane>
    </Resizable.PaneGroup>
  </div>
{:else}
  <div class="h-full w-full relative">
    <div
      id="jl4-editor"
      class="relative h-full w-full"
      bind:this={editorElement}
    ></div>
  </div>
{/if}

<SvelteToast />

<style>
  :root {
    --toastColor: rgb(96, 56, 19);
    --toastBackground: white;
    --toastBorderRadius: 4px;
  }
  .top-bar {
    height: 42px;
    font-family: 'Merriweather', Times, serif;
    color: rgb(30, 29, 28);
    background: rgba(250, 250, 249, 0.88);
    display: flex;
  }
  .sidebar {
    width: max(20%, 250px);
    position: absolute;
    top: 42px;
    height: calc(100dvh - 42px);
    left: 0;
  }
  .panes {
    position: absolute;
    overflow: hidden;
    top: 42px;
    left: 0;
    right: 0;
    height: calc(100dvh - 42px);
  }
  .sidebar + .panes {
    left: max(20%, 250px);
  }
  h3 {
    font-size: 1.4rem;
    font-weight: bold;
    padding: 0.4rem 1rem;
  }
  .fab {
    position: absolute;
    min-width: 24px;
    height: 24px;
    font-size: 0.8rem;
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 300;
    border: none;
    cursor: pointer;
    transition:
      box-shadow 0.2s,
      background 0.2s;
    color: rgba(30, 29, 28, 0.698);
  }
  .fab:hover {
    background: #f3f2f1;
    color: rgba(30, 29, 28, 1);
    border-radius: 2px;
    outline: 3px solid #f3f2f1;
  }
  .fab-sidebar.open {
    position: absolute;
    top: 10px;
    left: calc(max(20%, 250px) + 0.5rem);
  }
  .fab-sidebar.open .arrow-right,
  .fab-sidebar:not(.open) .arrow-left {
    display: none;
  }
  .fab-sidebar {
    position: relative;
    top: 10px;
    left: auto;
  }
  .fab-share {
    top: 10px;
    right: 1rem;
  }
  .slightly-shorter-than-full-viewport-height {
    height: 98svh;
  }

  @media (max-width: 1023px) {
    .sidebar + .panes {
      left: 0;
    }
    .sidebar {
      position: fixed;
      z-index: 10;
      width: 100vw;
      backdrop-filter: blur(4px);
    }
    .fab-sidebar {
      position: relative !important;
      left: auto !important;
    }
  }
</style>
