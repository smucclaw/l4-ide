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
  import {
    createLspConnection,
    getDefaultConfig,
    type LspConnectionResult,
  } from '$lib/lsp-connection-factory'
  import type { LadderBackendApi } from 'jl4-client-rpc'
  import { LadderApiForMonaco } from '$lib/ladder-api-for-monaco'
  import { MonacoErrorLens } from '@ym-han/monaco-error-lens'

  import { defaultExample, type LegalExample } from '$lib/legal-examples'
  import ExampleSelector from '$lib/components/example-selector.svelte'
  import {
    fetchQueryPlan,
    upsertFunctionFromSource,
    type DecisionServiceClient,
  } from '$lib/decision-service-client'

  import {
    LadderFlow,
    LirContext,
    LirRegistry,
    type FunDeclLirNode,
    LadderEnv,
    elicitationOverrideFromQueryPlan,
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
  const decisionServiceUrl =
    import.meta.env.VITE_DECISION_SERVICE_URL || 'http://localhost:8001'
  const wizardUrl = import.meta.env.VITE_WIZARD_URL || 'http://localhost:5174'
  const decisionServiceClient: DecisionServiceClient = {
    baseUrl: decisionServiceUrl,
  }

  let currentDecisionServiceFunctionName: string | null = $state(null)
  let currentLadderGraphId: import('l4-ladder-visualizer').LirId | null = null
  let ensureDecisionServiceFnReady: Promise<void> = Promise.resolve()
  let lastQueryPlanBindingsKey: string | null = null
  let queryPlanInFlight = false
  let queryPlanNeedsRerun = false

  let persistButtonBlocked = $state(false)

  // Set from URL search params as early as possible to avoid flicker
  const ownUrl = new URL(window.location.href)

  let showFrame = $state(!ownUrl.searchParams.has('standalone'))
  let showExamples = $state(!ownUrl.searchParams.has('no-examples'))
  let showSidebar = $state(
    window.innerWidth < 1024 ? false : !ownUrl.searchParams.has('no-examples')
  )
  let showVisualizer = $state(false)
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  let paneGroup: any = $state()

  // Reactive statement to handle visualizer state changes
  $effect(() => {
    if (
      showVisualizer &&
      paneGroup &&
      typeof paneGroup.setSizes === 'function'
    ) {
      // Try to use the API if available
      setTimeout(() => {
        if (paneGroup && typeof paneGroup.setSizes === 'function') {
          paneGroup.setSizes([50, 50])
        }
      }, 0)
    }
  })

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
  let lspConnection: LspConnectionResult | undefined

  function bindingsKey(bindings: Record<string, boolean>) {
    const entries = Object.entries(bindings).sort(([a], [b]) =>
      a.localeCompare(b)
    )
    return JSON.stringify(entries)
  }

  function getCurrentAtomBindings(): {
    fnName: string
    bindings: Record<string, boolean>
  } | null {
    if (!currentDecisionServiceFunctionName) return null
    const top = ladderEnv?.getTopFunDeclLirNode(context)
    const ladderGraph = top?.getBody(context)
    if (!ladderGraph) return null

    const out: Record<string, boolean> = {}
    for (const [unique, val] of ladderGraph.getBindings(context).getEntries()) {
      if (!val) continue
      const atomId =
        ladderGraph.getAtomIdForUnique(context, unique) ??
        ladderGraph.getLabelForUnique(context, unique)
      if (val.$type === 'TrueV') {
        out[atomId] = true
      } else if (val.$type === 'FalseV') {
        out[atomId] = false
      }
    }
    return { fnName: currentDecisionServiceFunctionName, bindings: out }
  }

  async function refreshQueryPlanFromDecisionService() {
    const curr = getCurrentAtomBindings()
    if (!curr) return

    const nextKey = `${curr.fnName}|${bindingsKey(curr.bindings)}`
    if (lastQueryPlanBindingsKey === nextKey) return

    await ensureDecisionServiceFnReady

    const resp = await fetchQueryPlan(
      decisionServiceClient,
      curr.fnName,
      curr.bindings
    )
    lastQueryPlanBindingsKey = nextKey

    const ladderGraph = ladderEnv.getTopFunDeclLirNode(context).getBody(context)

    ladderGraph.setElicitationOverride(
      context,
      elicitationOverrideFromQueryPlan(context, ladderGraph, resp)
    )
  }

  function scheduleQueryPlanRefresh() {
    if (queryPlanInFlight) {
      queryPlanNeedsRerun = true
      return
    }
    queryPlanInFlight = true
    void refreshQueryPlanFromDecisionService()
      .catch((e) => {
        console.warn('decision-service query-plan failed', e)
      })
      .finally(() => {
        queryPlanInFlight = false
        if (queryPlanNeedsRerun) {
          queryPlanNeedsRerun = false
          scheduleQueryPlanRefresh()
        }
      })
  }

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
    const { configureDefaultWorkerFactory } = await import(
      'monaco-editor-wrapper/workers/workerLoaders'
    )
    const { ConsoleLogger } = await import('monaco-languageclient/tools')

    // Get LSP connection configuration (supports WebSocket or WASM)
    const lspConfig = getDefaultConfig()

    const runClient = async () => {
      console.log('🚀 runClient() STARTING - LSP client initialization')
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

      // Add comment/uncomment action to context menu
      editor.addAction({
        id: 'toggle-line-comment',
        label: 'Toggle Line Comment',
        keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Slash],
        contextMenuGroupId: 'modification',
        contextMenuOrder: 1.5,
        run: (ed) => {
          const selection = ed.getSelection()
          if (!selection) return

          const model = ed.getModel()
          if (!model) return

          const startLineNumber = selection.startLineNumber
          const endLineNumber = selection.endLineNumber

          // Check if all selected lines are commented
          let allCommented = true
          for (
            let lineNumber = startLineNumber;
            lineNumber <= endLineNumber;
            lineNumber++
          ) {
            const lineContent = model.getLineContent(lineNumber)
            const trimmedContent = lineContent.trim()
            if (trimmedContent !== '' && !trimmedContent.startsWith('--')) {
              allCommented = false
              break
            }
          }

          const edits: monaco.editor.IIdentifiedSingleEditOperation[] = []

          for (
            let lineNumber = startLineNumber;
            lineNumber <= endLineNumber;
            lineNumber++
          ) {
            const lineContent = model.getLineContent(lineNumber)
            const trimmedContent = lineContent.trim()

            // Skip empty lines
            if (trimmedContent === '') continue

            if (allCommented) {
              // Uncomment: remove '--' and following space if present
              const match = lineContent.match(/^(\s*)--\s?/)
              if (match) {
                edits.push({
                  range: new monaco.Range(
                    lineNumber,
                    1,
                    lineNumber,
                    match[0].length + 1
                  ),
                  text: match[1], // Remove the comment prefix
                })
              }
            } else {
              // Comment: add '--' at the beginning of the line
              edits.push({
                range: new monaco.Range(lineNumber, 1, lineNumber, 1),
                text: '-- ',
              })
            }
          }

          if (edits.length > 0) {
            ed.executeEdits('toggle-comment', edits)
          }
        },
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

      // Initialize LSP connection using the factory
      // This abstracts over WebSocket vs WASM connection types
      await initLspConnection(logger, lspConfig)
    }

    /**
     * Initialize LSP connection using the connection factory.
     * Supports both WebSocket (current) and WASM (future) connection types.
     */
    const initLspConnection = async (
      logger: ConsoleLogger,
      config: ReturnType<typeof getDefaultConfig>
    ): Promise<void> => {
      try {
        logger.info(`[L4 LSP] Connecting with preferred type: ${config.preferredType}`)

        lspConnection = await createLspConnection(config)

        logger.info(`[L4 LSP] Connected via ${lspConnection.type}`)

        const languageClient = createLanguageClient(logger, lspConnection.transports)
        await languageClient.start()

        // Handle connection close
        lspConnection.transports.reader.onClose(() => {
          logger.info('[L4 LSP] Connection closed')
          languageClient.dispose()
        })
      } catch (error) {
        logger.error(`[L4 LSP] Failed to connect: ${error}`)
        throw error
      }
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
      // Automatically show visualizer when ladder object is updated
      showVisualizer = true

      const fnName = ladderInfo.funDecl.name.label
      currentDecisionServiceFunctionName = fnName
      lastQueryPlanBindingsKey = null

      const source = editor?.getValue() ?? ''
      ensureDecisionServiceFnReady = upsertFunctionFromSource(
        decisionServiceClient,
        fnName,
        source
      ).catch((e) => {
        console.warn('decision-service upsert failed', e)
      })

      const ladderGraph = ladderEnv
        .getTopFunDeclLirNode(context)
        .getBody(context)
      currentLadderGraphId = ladderGraph.getId()
      scheduleQueryPlanRefresh()

      // Re-initialize editor after state change
      setTimeout(() => {
        if (editorElement && editor) {
          editor.layout()
        }
      }, 0)
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
    console.log('📍 About to call runClient()')
    await runClient()
    console.log('✅ runClient() completed')

    const sub = lirRegistry.subscribe((_ctx, id) => {
      if (!currentLadderGraphId) return
      if (!id.isEqualTo(currentLadderGraphId)) return
      scheduleQueryPlanRefresh()
    })

    onDestroy(() => sub.unsubscribe())
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

    // Dispose LSP connection
    if (lspConnection) {
      lspConnection.dispose()
      lspConnection = undefined
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

  async function handleVizualiser() {
    showVisualizer = !showVisualizer
    // Re-initialize editor after state change
    setTimeout(() => {
      if (editorElement && editor) {
        editor.layout()
      }
    }, 0)
  }

  async function handleShare() {
    const sessionId = await handlePersist()
    if (sessionId) {
      const shareUrl = `${window.location.origin}${window.location.pathname}?id=${sessionId}`
      window.history.pushState(null, '', shareUrl)

      if (navigator.share) {
        navigator.share({
          url: shareUrl,
          text: 'L4 document',
        })
      }

      await navigator.clipboard.writeText(shareUrl)

      // Build wizard URL using path-based routing: /wizard/{sessionId}/{functionName}
      const shareWizardUrl = currentDecisionServiceFunctionName
        ? `${wizardUrl}/${sessionId}/${encodeURIComponent(currentDecisionServiceFunctionName)}`
        : `${wizardUrl}/${sessionId}`
      toast.push(
        `Link copied to clipboard. <a href="${shareWizardUrl}" target="_blank" style="color: #60a5fa; text-decoration: underline;">Open in Wizard</a>`,
        { duration: 6000 }
      )
    } else {
      toast.push('Could not persist the file to generate a share link.')
    }
  }

  async function handleOpenWizard() {
    // Open window synchronously to avoid popup blocking (async awaits would make it non-user-initiated)
    const wizardWindow = window.open('about:blank', '_blank')

    // Persist to session server - we need the session ID for the wizard URL
    const sessionId = await handlePersist()

    // If we have a function, ensure it's uploaded to the decision service
    if (currentDecisionServiceFunctionName) {
      try {
        await ensureDecisionServiceFnReady
      } catch (e) {
        console.warn('Failed to ensure function is ready:', e)
      }
    }

    // Navigate to wizard using path-based routing: /wizard/{sessionId}/{functionName}
    let targetWizardUrl: string
    if (sessionId && currentDecisionServiceFunctionName) {
      targetWizardUrl = `${wizardUrl}/${sessionId}/${encodeURIComponent(currentDecisionServiceFunctionName)}`
    } else if (sessionId) {
      targetWizardUrl = `${wizardUrl}/${sessionId}`
    } else {
      targetWizardUrl = wizardUrl
    }

    if (wizardWindow) {
      wizardWindow.location.href = targetWizardUrl
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

  function handleLoadFile() {
    // Create a hidden file input
    const input = document.createElement('input')
    input.type = 'file'
    input.accept = '.l4'
    input.style.display = 'none'

    input.onchange = (event) => {
      const file = (event.target as HTMLInputElement).files?.[0]
      if (file && editor) {
        const reader = new FileReader()
        reader.onload = (e) => {
          const content = e.target?.result as string
          if (content && editor) {
            editor.setValue(content)
            // Clear any URL parameters since we're loading a new file
            const ownUrl = new URL(window.location.href)
            if (ownUrl.searchParams.has('id')) {
              ownUrl.searchParams.delete('id')
              history.pushState(null, '', ownUrl)
            }
          }
        }
        reader.readAsText(file)
      }
      // Clean up the input element
      document.body.removeChild(input)
    }

    // Add to DOM, click, and it will be cleaned up in the onchange handler
    document.body.appendChild(input)
    input.click()
  }

  async function handleSaveFile() {
    if (!editor) return

    const content = editor.getValue()

    // Extract filename from first section if available
    let filename = 'Rules as Code'
    const sectionMatch = content.match(/^\u00a7\s*`([^`]+)`/m)
    if (sectionMatch && sectionMatch[1]) {
      // Clean the section name for use as filename
      filename = sectionMatch[1].trim().replace(/[<>:"/\\|?*]/g, '_') // Replace invalid filename characters
    } else {
      // Fallback to timestamp-based name if no section found
      filename = `Rules as Code ${new Date().toISOString().slice(0, 19).replace(/[:-]/g, '')}`
    }

    // Check if the File System Access API is supported
    if ('showSaveFilePicker' in window) {
      try {
        // Show the save dialog
        const fileHandle = await (
          window as typeof window & {
            showSaveFilePicker: (options: {
              suggestedName: string
              types: Array<{
                description: string
                accept: Record<string, string[]>
              }>
            }) => Promise<FileSystemFileHandle>
          }
        ).showSaveFilePicker({
          suggestedName: `${filename}.l4`,
          types: [
            {
              description: 'L4 files',
              accept: {
                'text/plain': ['.l4'],
              },
            },
          ],
        })

        // Create a writable stream and write the content
        const writable = await fileHandle.createWritable()
        await writable.write(content)
        await writable.close()

        // Success feedback
        toast.push('File saved successfully!')
        return
      } catch (error: unknown) {
        // User cancelled the dialog or other error
        if (error instanceof Error && error.name !== 'AbortError') {
          console.error('Error saving file:', error)
          toast.push('Error saving file. Falling back to download.')
        } else {
          // User cancelled - don't show error
          return
        }
      }
    }

    // Fallback for browsers that don't support File System Access API
    // or if the modern API failed
    const blob = new Blob([content], { type: 'text/plain' })
    const url = URL.createObjectURL(blob)

    // Create a temporary download link
    const link = document.createElement('a')
    link.href = url
    link.download = `${filename}.l4`
    link.style.display = 'none'

    // Trigger download
    document.body.appendChild(link)
    link.click()

    // Clean up
    document.body.removeChild(link)
    URL.revokeObjectURL(url)
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
    <div class="spacer"></div>
    <button
      class="fab fab-load-file"
      onclick={handleLoadFile}
      aria-label="Load .l4 file"
      title="Load .l4 file"
    >
      <svg
        width="20"
        height="20"
        style="font-size: 24px; vertical-align: middle; margin-right: 0.3em;"
        viewBox="-4 0 28 24"
        fill="none"
        stroke="currentColor"
        stroke-width="1.3"
        stroke-linecap="round"
        stroke-linejoin="round"
        aria-hidden="true"
        focusable="false"
      >
        <!-- Folder -->
        <path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z" />
        <polyline points="14,2 14,8 20,8" />
        <!-- Arrow pointing up into folder -->
        <polyline points="12,11 12,17" />
        <polyline points="9,14 12,11 15,14" />
      </svg>
      Load file
    </button>
    <button
      class="fab fab-save-file"
      onclick={handleSaveFile}
      aria-label="Save .l4 file"
      title="Save .l4 file"
    >
      <svg
        width="20"
        height="20"
        style="font-size: 24px; vertical-align: middle; margin-right: 0.3em;"
        viewBox="-4 0 28 24"
        fill="none"
        stroke="currentColor"
        stroke-width="1.3"
        stroke-linecap="round"
        stroke-linejoin="round"
        aria-hidden="true"
        focusable="false"
      >
        <!-- Folder -->
        <path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z" />
        <polyline points="14,2 14,8 20,8" />
        <!-- Arrow pointing down from folder -->
        <polyline points="12,17 12,11" />
        <polyline points="9,14 12,17 15,14" />
      </svg>
      Save file
    </button>
    {#if window.innerWidth > 1023}
      <button
        class="fab fab-logic {showVisualizer ? 'selected' : ''}"
        onclick={handleVizualiser}
        aria-label="Toggle Visualizer"
        title="Toggle Visualizer"
      >
        <svg
          style="font-size: 24px; vertical-align: middle; margin-right: .5rem"
          width="20"
          height="20"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="1.3"
          stroke-linecap="round"
          stroke-linejoin="round"
          aria-hidden="true"
          focusable="false"
        >
          <!-- Top node -->
          <circle cx="12" cy="5" r="2.2" />
          <!-- Bottom left node -->
          <circle cx="7" cy="18" r="2.2" />
          <!-- Bottom right node -->
          <circle cx="17" cy="18" r="2.2" />
          <!-- Line from top node down to split point -->
          <line x1="12" y1="7.2" x2="12" y2="12" />
          <!-- Curved line from split to left node -->
          <path d="M12 12 Q9 15 7 18" />
          <!-- Curved line from split to right node -->
          <path d="M12 12 Q15 15 17 18" />
        </svg>
        Logic Viz
      </button>
    {/if}
    <button
      class="fab fab-wizard"
      onclick={handleOpenWizard}
      aria-label="Deploy"
      title="Deploy"
    >
      <svg
        width="20"
        height="20"
        style="font-size: 24px; vertical-align: middle; margin-right: 0.2em;"
        viewBox="0 0 24 24"
        fill="none"
        xmlns="http://www.w3.org/2000/svg"
        stroke-width="1.5"
        stroke="currentColor"
      >
        <!-- Magic wand / wizard icon -->
        <path
          d="M15 4V2M15 16V14M8 9H10M20 9H22M17.8 11.8L19 13M17.8 6.2L19 5M12.2 11.8L11 13M12.2 6.2L11 5"
        />
        <path d="M15 9L3 21" stroke-linecap="round" />
        <path d="M13 7L17 11" stroke-linecap="round" />
      </svg>
      Deploy
    </button>
    <button
      class="fab fab-share"
      onclick={handleShare}
      aria-label="Share the current file"
      title="Share the current file"
      disabled={persistButtonBlocked}
    >
      <svg
        width="20"
        height="20"
        style="font-size: 24px; vertical-align: middle; margin-right: 0.2em;"
        viewBox="0 0 24 24"
        fill="none"
        xmlns="http://www.w3.org/2000/svg"
        stroke-width="1.3"
        ><path
          d="M6.99609 9L11.9961 4L16.9961 9M6.99609 9M12 16V4"
          stroke="currentColor"
        ></path><path
          d="M4 15V16C4 18.2091 5.79086 20 8 20H16C18.2091 20 20 18.2091 20 16V15"
          stroke="currentColor"
        ></path></svg
      >
      Share
    </button>
  </div>
  {#if showExamples && showSidebar}
    <div class="sidebar">
      <ExampleSelector onExampleSelect={handleExampleSelect} />
    </div>
  {/if}
  <div class="panes">
    <Resizable.PaneGroup direction="horizontal" bind:this={paneGroup}>
      <Resizable.Pane defaultSize={showVisualizer ? 50 : 100}>
        <div id="jl4-editor" class="h-full" bind:this={editorElement}></div>
      </Resizable.Pane>
      {#if showVisualizer}
        <Resizable.Handle />
        <Resizable.Pane defaultSize={50}>
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
      {/if}
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
    --toastColor: rgba(30, 29, 28, 0.698);
    --toastBackground: white;
    --toastBorderRadius: 3px;
  }
  :global(._toastContainer) {
    position: fixed !important;
    bottom: 1rem !important;
    right: 1rem !important;
    top: auto !important;
    left: auto !important;
    transform: none !important;
    max-width: 300px;
    z-index: 9999;
    font-family: 'Merriweather', Times, serif;
    font-size: 0.85em;
  }

  :global(._toastItem) {
    margin: 0.5rem 0 !important;
  }
  .top-bar {
    height: 42px;
    font-family: 'Merriweather', Times, serif;
    color: rgb(30, 29, 28);
    background: rgba(250, 250, 249, 0.88);
    display: flex;
    gap: 0.5rem;
    align-items: center;
    padding-right: 1em;
  }
  .spacer {
    flex: 1;
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
    padding: 0.4rem 0.5rem 0.4rem 1rem;
  }
  .fab {
    border-radius: 2px;
    min-width: 24px;
    padding: 0 0.3em;
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
  .fab svg {
    margin: 0 -0.1em;
  }
  .fab:hover {
    color: rgba(30, 29, 28, 1);
  }
  .fab.selected {
    background: #f3f2f1;
    outline: 3px solid #f3f2f1;
  }
  .fab-sidebar.open {
    position: absolute;
    left: calc(max(20%, 250px) + 0.5rem);
  }
  .fab-sidebar.open .arrow-right,
  .fab-sidebar:not(.open) .arrow-left {
    display: none;
  }
  .fab-sidebar {
    position: relative;
    left: auto;
    padding: 0;
  }
  .slightly-shorter-than-full-viewport-height {
    height: 100%;
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
    .fab-load-file,
    .fab-save-file,
    .fab-logic {
      display: none;
    }
  }
</style>
