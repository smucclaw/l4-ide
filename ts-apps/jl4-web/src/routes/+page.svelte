<script lang="ts">
  import { onMount, onDestroy } from 'svelte'
  import { SvelteToast, toast } from '@zerodevx/svelte-toast'
  import { faShareAlt } from '@fortawesome/free-solid-svg-icons'
  import { FontAwesomeIcon } from '@fortawesome/svelte-fontawesome'
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
  let showVisualizer = $state(true)
  let showExamples = $state(true)
  let showSidebar = $state(false)

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

      const ownUrl: URL = new URL(window.location.href)
      showVisualizer = !ownUrl.searchParams.has('no-visualizer')
      showExamples = !ownUrl.searchParams.has('no-examples')

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
          middleware: mkMiddleware(logger, makeLadderFlow),
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
      toast.push('Session persisted and share link copied to clipboard')
    } else {
      toast.push('Could not persist the file.')
    }
  }

  function handleExampleSelect(example: LegalExample) {
    if (showExamples && editor) {
      editor.setValue(example.content)
    }
  }
</script>

<!-- Hamburger FAB (fixed, over editor panel) -->
{#if !showSidebar}
  <button
    class="fab fab-hamburger"
    onclick={() => (showSidebar = true)}
    aria-label="Open examples sidebar"
    title="Open examples sidebar"
    style="left: 2.5rem; top: 1.5rem;"
  >
    <svg
      width="24"
      height="24"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      stroke-width="1.5"
    >
      <line x1="4" y1="7" x2="20" y2="7" />
      <line x1="4" y1="12" x2="20" y2="12" />
      <line x1="4" y1="17" x2="20" y2="17" />
    </svg>
  </button>
{/if}

<!-- Share FAB (fixed, top-right) -->
<button
  class="fab fab-share"
  onclick={handleShare}
  aria-label="Share the current file"
  title="Share the current file"
  disabled={persistButtonBlocked}
>
  <FontAwesomeIcon
    icon={faShareAlt}
    style="font-size: 24px; vertical-align: middle;"
  />
</button>

<!-- Sidebar Overlay -->
<div
  class="sidebar-overlay {showSidebar ? 'open' : ''}"
  onclick={() => (showSidebar = false)}
  tabindex="-1"
  aria-hidden={!showSidebar}
></div>

<!-- Sidebar Panel (overlay) -->
<aside class="sidebar {showSidebar ? 'open' : ''}" aria-label="Examples menu">
  <button
    class="close-btn"
    onclick={() => (showSidebar = false)}
    aria-label="Close sidebar"
  >
    <svg
      width="24"
      height="24"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      stroke-width="1.5"
    >
      <line x1="6" y1="6" x2="18" y2="18" />
      <line x1="6" y1="18" x2="18" y2="6" />
    </svg>
  </button>
  <ExampleSelector onExampleSelect={handleExampleSelect} />
</aside>

{#if showVisualizer}
  <Resizable.PaneGroup direction="horizontal">
    <!-- Only show editor and visualizer by default -->
    <Resizable.Pane defaultSize={50}>
      <div id="jl4-editor" class="h-full" bind:this={editorElement}></div>
    </Resizable.Pane>
    <Resizable.Handle />
    <Resizable.Pane>
      <div class="relative h-full">
        <div id="jl4-webview" class="h-full max-w-[96%] mx-auto bg-white">
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
    </Resizable.Pane>
  </Resizable.PaneGroup>
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
    --toastColor: #104e64;
    --toastBackground: #white;
    --toastBorderRadius: 4px;
  }

  .fab {
    position: fixed;
    width: 48px;
    height: 48px;
    border-radius: 50%;
    background: #fff;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.12);
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
    box-shadow: 0 4px 16px rgba(0, 0, 0, 0.18);
    background: #f3f2f1;
    color: rgba(30, 29, 28, 1);
  }
  .fab-share {
    top: 1.5rem;
    right: 1.5rem;
  }
  .sidebar-overlay {
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.2);
    opacity: 0;
    pointer-events: none;
    transition: opacity 0.2s;
    z-index: 100;
  }
  .sidebar-overlay.open {
    opacity: 1;
    pointer-events: auto;
  }
  .sidebar {
    position: fixed;
    top: 0;
    left: 0;
    bottom: 0;
    width: 260px;
    background: #fff;
    box-shadow: 2px 0 8px rgba(0, 0, 0, 0.08);
    transform: translateX(-100%);
    transition: transform 0.3s cubic-bezier(0.4, 0, 0.2, 1);
    z-index: 150;
    overflow-y: auto;
    display: flex;
    flex-direction: column;
    min-height: 0;
  }
  .sidebar.open {
    transform: translateX(0);
  }
  .close-btn {
    position: absolute;
    top: 1.5rem;
    right: 1.5rem;
    z-index: 1;
    background: none;
    border: none;
    color: rgba(30, 29, 28, 0.698);
    font-size: 1.5rem;
    cursor: pointer;
    padding: 0.5rem;
    transition: color 0.2s;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  .close-btn:hover {
    color: rgba(30, 29, 28, 1);
  }
  .slightly-shorter-than-full-viewport-height {
    height: 98svh;
  }
</style>
