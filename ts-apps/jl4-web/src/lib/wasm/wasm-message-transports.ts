/**
 * WASM Message Transports
 *
 * Provides MessageTransports-compatible adapters that route LSP requests
 * to the WASM bridge instead of a network connection.
 *
 * This enables running L4 language features in the browser
 * without a server connection, while remaining compatible with
 * the standard vscode-languageclient MessageTransports interface.
 */

import type { MessageTransports } from 'vscode-languageclient'
import type {
  MessageReader,
  MessageWriter,
  Message,
  DataCallback,
  Disposable,
  PartialMessageInfo,
  Event,
} from 'vscode-jsonrpc'
import type {
  L4WasmBridge,
  Diagnostic,
  EvalDirectiveResult,
} from './wasm-bridge'

/** LSP message types */
interface LspRequest {
  jsonrpc: '2.0'
  id: number | string
  method: string
  params?: unknown
}

interface LspNotification {
  jsonrpc: '2.0'
  method: string
  params?: unknown
}

interface LspResponse {
  jsonrpc: '2.0'
  id: number | string | null
  result?: unknown
  error?: {
    code: number
    message: string
  }
}

type LspMessage = LspRequest | LspNotification | LspResponse

type MessageCallback = (message: LspMessage) => void

/**
 * State tracking for the currently selected visualization.
 */
interface VisualizationState {
  functionName: string
  simplify: boolean
  uri: string
}

/**
 * WASM-based LSP message handler
 *
 * Processes LSP messages by routing them to the WASM bridge.
 * Tracks document state and publishes diagnostics.
 */
export class WasmLspHandler {
  private documentContents = new Map<string, string>()
  private documentVersions = new Map<string, number>()
  private messageCallback: MessageCallback | null = null
  private disposed = false

  /**
   * Currently selected visualization state.
   * When set, auto-refresh will update this function on document changes.
   */
  private currentVisualization: VisualizationState | null = null

  constructor(private bridge: L4WasmBridge) {}

  /**
   * Register callback for outgoing messages (responses & notifications)
   */
  onMessage(callback: MessageCallback): void {
    this.messageCallback = callback
  }

  /**
   * Handle an incoming message (request or notification)
   */
  async handleMessage(message: LspMessage): Promise<void> {
    if (this.disposed) return

    if (this.isRequest(message)) {
      await this.handleRequest(message)
    } else if (this.isNotification(message)) {
      await this.handleNotification(message)
    }
  }

  private isRequest(msg: LspMessage): msg is LspRequest {
    return 'id' in msg && 'method' in msg
  }

  private isNotification(msg: LspMessage): msg is LspNotification {
    return 'method' in msg && !('id' in msg)
  }

  private send(message: LspMessage): void {
    if (this.messageCallback) {
      this.messageCallback(message)
    }
  }

  private async handleRequest(request: LspRequest): Promise<void> {
    try {
      const result = await this.processMethod(request.method, request.params)
      this.send({ jsonrpc: '2.0', id: request.id, result })
    } catch (error) {
      this.send({
        jsonrpc: '2.0',
        id: request.id,
        error: {
          code: -32603,
          message: error instanceof Error ? error.message : 'Unknown error',
        },
      })
    }
  }

  private async handleNotification(
    notification: LspNotification
  ): Promise<void> {
    const { method, params } = notification

    switch (method) {
      case 'textDocument/didOpen': {
        const p = params as {
          textDocument: { uri: string; text: string; version: number }
        }
        this.documentContents.set(p.textDocument.uri, p.textDocument.text)
        this.documentVersions.set(p.textDocument.uri, p.textDocument.version)
        this.checkAndPublishDiagnostics(
          p.textDocument.uri,
          p.textDocument.text,
          p.textDocument.version
        )
        break
      }

      case 'textDocument/didChange': {
        const p = params as {
          textDocument: { uri: string; version: number }
          contentChanges: Array<{ text: string }>
        }
        const newContent = p.contentChanges[0]?.text
        if (newContent !== undefined) {
          this.documentContents.set(p.textDocument.uri, newContent)
          this.documentVersions.set(p.textDocument.uri, p.textDocument.version)
          this.checkAndPublishDiagnostics(
            p.textDocument.uri,
            newContent,
            p.textDocument.version
          )
        }
        break
      }

      case 'textDocument/didClose': {
        const p = params as { textDocument: { uri: string } }
        this.documentContents.delete(p.textDocument.uri)
        this.documentVersions.delete(p.textDocument.uri)
        // Clear visualization if closing the visualized document
        if (this.currentVisualization?.uri === p.textDocument.uri) {
          this.currentVisualization = null
        }
        break
      }

      case 'initialized':
      case 'exit':
        break

      default:
        // Silently ignore unknown notifications
        break
    }
  }

  private async processMethod(
    method: string,
    params: unknown
  ): Promise<unknown> {
    switch (method) {
      case 'initialize':
        return this.getInitializeResult()

      case 'shutdown':
        return null

      case 'textDocument/hover':
        return this.handleHover(params)

      case 'textDocument/completion':
        return this.handleCompletion(params)

      case 'textDocument/semanticTokens/full':
        return this.handleSemanticTokens(params)

      case 'textDocument/codeLens':
        return this.handleCodeLens(params)

      case 'textDocument/definition':
        return this.handleDefinition(params)

      case 'textDocument/references':
        return this.handleReferences(params)

      case 'workspace/executeCommand':
        return this.handleExecuteCommand(params)

      default:
        // Unhandled method
        return null
    }
  }

  private getInitializeResult(): unknown {
    return {
      capabilities: {
        textDocumentSync: 1, // Full sync
        hoverProvider: true,
        completionProvider: {
          triggerCharacters: [' ', '\n'],
          resolveProvider: false,
        },
        semanticTokensProvider: {
          legend: {
            tokenTypes: [
              'keyword',
              'type',
              'function',
              'variable',
              'number',
              'string',
              'operator',
              'comment',
              'decorator',
            ],
            tokenModifiers: [],
          },
          full: true,
        },
        // Code lens for visualization actions
        codeLensProvider: {
          resolveProvider: false,
        },
        // Execute command for visualization
        executeCommandProvider: {
          commands: ['l4.visualize', 'l4.resetvisualization'],
        },
        // Go-to-definition and find-references (single-file only)
        definitionProvider: true,
        referencesProvider: true,
        // Not implemented in WASM mode
        codeActionProvider: false,
      },
      serverInfo: { name: 'l4-wasm', version: '0.1.0' },
    }
  }

  private async handleHover(params: unknown): Promise<unknown> {
    const p = params as {
      textDocument: { uri: string }
      position: { line: number; character: number }
    }
    const content = this.documentContents.get(p.textDocument.uri)
    if (!content) return null

    return this.bridge.hover(content, p.position.line, p.position.character)
  }

  private async handleCompletion(params: unknown): Promise<unknown> {
    const p = params as {
      textDocument: { uri: string }
      position: { line: number; character: number }
    }
    const content = this.documentContents.get(p.textDocument.uri)
    if (!content) return { items: [] }

    const items = await this.bridge.completions(
      content,
      p.position.line,
      p.position.character
    )
    return { items }
  }

  private async handleSemanticTokens(params: unknown): Promise<unknown> {
    const p = params as { textDocument: { uri: string } }
    const content = this.documentContents.get(p.textDocument.uri)
    if (!content) return { data: [] }

    return this.bridge.semanticTokens(content)
  }

  /**
   * Handle textDocument/codeLens request.
   * Returns code lenses for all visualizable DECIDE rules.
   */
  private async handleCodeLens(params: unknown): Promise<unknown> {
    const p = params as { textDocument: { uri: string } }
    const uri = p.textDocument.uri
    const content = this.documentContents.get(uri)
    if (!content) {
      return []
    }

    const version = this.documentVersions.get(uri) ?? 0

    try {
      const lenses = await this.bridge.codeLenses(content, uri, version)
      // Transform to LSP CodeLens format
      return lenses.map((lens) => ({
        range: {
          start: {
            line: lens.range.startLineNumber - 1, // Convert to 0-indexed
            character: lens.range.startColumn - 1,
          },
          end: {
            line: lens.range.endLineNumber - 1,
            character: lens.range.endColumn - 1,
          },
        },
        command: {
          title: lens.command.title,
          command: lens.command.id,
          arguments: lens.command.arguments,
        },
      }))
    } catch {
      return []
    }
  }

  /**
   * Handle textDocument/definition request.
   * Returns the location of the definition for the symbol at the given position.
   */
  private async handleDefinition(params: unknown): Promise<unknown> {
    const p = params as {
      textDocument: { uri: string }
      position: { line: number; character: number }
    }
    const uri = p.textDocument.uri
    const content = this.documentContents.get(uri)
    if (!content) return null

    // Convert from 0-indexed (LSP) to 1-indexed (L4)
    const line = p.position.line + 1
    const col = p.position.character + 1

    try {
      const result = await this.bridge.definition(content, line, col)
      if (!result) return null

      // Return as LSP Location with the document URI
      return {
        uri: uri,
        range: result.range,
      }
    } catch {
      return null
    }
  }

  /**
   * Handle textDocument/references request.
   * Returns all locations where the symbol at the given position is used.
   */
  private async handleReferences(params: unknown): Promise<unknown> {
    const p = params as {
      textDocument: { uri: string }
      position: { line: number; character: number }
      context: { includeDeclaration: boolean }
    }
    const uri = p.textDocument.uri
    const content = this.documentContents.get(uri)
    if (!content) return []

    // Convert from 0-indexed (LSP) to 1-indexed (L4)
    const line = p.position.line + 1
    const col = p.position.character + 1

    try {
      const results = await this.bridge.references(content, line, col)
      // Add the document URI to each location
      return results.map((loc) => ({
        uri: uri,
        range: loc.range,
      }))
    } catch {
      return []
    }
  }

  /**
   * Handle workspace/executeCommand requests.
   * Supports l4.visualize and l4.resetvisualization commands.
   */
  private async handleExecuteCommand(params: unknown): Promise<unknown> {
    const p = params as {
      command: string
      arguments?: unknown[]
    }

    switch (p.command) {
      case 'l4.visualize':
        return this.handleVisualize(p.arguments)

      case 'l4.resetvisualization':
        this.currentVisualization = null
        return null

      default:
        // Unknown command
        return null
    }
  }

  /**
   * Handle the l4.visualize command.
   *
   * Supports two calling conventions:
   * 1. Code lens click: [verDocId, functionName, simplify]
   *    - Visualizes the specific function and sets it as current
   * 2. Auto-refresh: [verDocId]
   *    - If a function is currently selected, refreshes it
   *    - If function no longer exists, clears the visualization
   *    - If no function selected, visualizes the first available
   */
  private async handleVisualize(args?: unknown[]): Promise<unknown> {
    if (!args || args.length === 0) {
      return null
    }

    const verDocId = args[0] as { uri: string; version?: number }
    const uri = verDocId.uri
    const version = verDocId.version ?? this.documentVersions.get(uri) ?? 0

    const content = this.documentContents.get(uri)
    if (!content) {
      return null
    }

    if (!this.bridge.supportsVisualization()) {
      return null
    }

    // Check if this is a code lens click (has function name and simplify flag)
    if (args.length >= 3) {
      const functionName = args[1] as string
      const simplify = args[2] as boolean

      // Update current visualization state
      this.currentVisualization = { functionName, simplify, uri }

      // Visualize the specific function
      const result = await this.bridge.visualizeByName(
        content,
        uri,
        version,
        functionName,
        simplify
      )

      if (result.error) {
        if (result.notFound) {
          this.currentVisualization = null
        }
        return null
      }

      return result
    }

    // Auto-refresh: check if we have a current visualization for this document
    if (this.currentVisualization && this.currentVisualization.uri === uri) {
      const { functionName, simplify } = this.currentVisualization

      const result = await this.bridge.visualizeByName(
        content,
        uri,
        version,
        functionName,
        simplify
      )

      if (result.error) {
        if (result.notFound) {
          // Function no longer exists - clear the visualization
          this.currentVisualization = null
          return { cleared: true, reason: 'function_not_found' }
        }
        // Other error - still return null but keep the state
        return null
      }

      return result
    }

    // No current visualization selected - do nothing
    // User must click a code lens to start visualization
    return null
  }

  /**
   * Get the currently selected visualization state.
   * Useful for debugging and UI state sync.
   */
  getCurrentVisualization(): VisualizationState | null {
    return this.currentVisualization
  }

  /**
   * Clear the current visualization selection.
   */
  clearVisualization(): void {
    this.currentVisualization = null
  }

  private async checkAndPublishDiagnostics(
    uri: string,
    content: string,
    version?: number
  ): Promise<void> {
    // Get parse and type check diagnostics
    const checkDiagnostics = await this.bridge.check(content)

    // Also run evaluation to get #EVAL results as diagnostics
    // Only if there are no errors (matching real LSP behavior)
    let evalDiagnostics: Diagnostic[] = []
    const hasErrors = checkDiagnostics.some((d) => d.severity === 1)
    if (!hasErrors) {
      try {
        const evalResult = await this.bridge.evaluate(content)
        if (evalResult.success) {
          evalDiagnostics = evalResult.results.map(
            this.evalResultToDiagnostic.bind(this)
          )
        }
      } catch {
        // Evaluation failed, continue with check diagnostics only
      }
    }

    // Merge diagnostics: check errors + eval results
    const diagnostics = [...checkDiagnostics, ...evalDiagnostics]

    this.send({
      jsonrpc: '2.0',
      method: 'textDocument/publishDiagnostics',
      params: { uri, version, diagnostics },
    } as LspNotification)
  }

  /**
   * Convert an evaluation result to an LSP diagnostic.
   * Matches the behavior of evalLazyResultToDiagnostic in jl4-lsp.
   */
  private evalResultToDiagnostic(evalResult: EvalDirectiveResult): Diagnostic {
    // Severity: Error (1) for failed assertions, Information (3) otherwise
    // This matches the real LSP server behavior in Rules.hs
    const severity = evalResult.success === false ? 1 : 3

    return {
      range: evalResult.range ?? {
        start: { line: 0, character: 0 },
        end: { line: 0, character: 0 },
      },
      severity,
      message: evalResult.result,
      source: 'eval',
    }
  }

  dispose(): void {
    this.disposed = true
    this.documentContents.clear()
    this.documentVersions.clear()
    this.messageCallback = null
    this.currentVisualization = null
  }
}

/**
 * Create a WASM LSP handler
 */
export function createWasmLspHandler(bridge: L4WasmBridge): WasmLspHandler {
  return new WasmLspHandler(bridge)
}

/**
 * A MessageReader that receives messages from the WasmLspHandler.
 *
 * This adapter allows the WasmLspHandler to push messages (responses and
 * notifications) that the language client will receive as if they came
 * from a real LSP server.
 */
class WasmMessageReader implements MessageReader {
  private callback: DataCallback | null = null
  private errorListener: ((error: Error) => void) | null = null
  private closeListener: (() => void) | null = null
  private partialMessageListener: ((info: PartialMessageInfo) => void) | null =
    null
  private disposed = false

  /**
   * Push a message to the reader (called by WasmLspHandler)
   */
  pushMessage(message: Message): void {
    if (this.disposed) return
    if (this.callback) {
      this.callback(message)
    }
  }

  listen(callback: DataCallback): Disposable {
    this.callback = callback
    return {
      dispose: () => {
        this.callback = null
      },
    }
  }

  /**
   * Event emitter for errors.
   * Implements the vscode-jsonrpc Event<T> interface.
   */
  readonly onError: Event<Error> = (
    listener: (e: Error) => void
  ): Disposable => {
    this.errorListener = listener
    return {
      dispose: () => {
        this.errorListener = null
      },
    }
  }

  /**
   * Event emitter for close.
   * Implements the vscode-jsonrpc Event<T> interface.
   */
  readonly onClose: Event<void> = (listener: () => void): Disposable => {
    this.closeListener = listener
    return {
      dispose: () => {
        this.closeListener = null
      },
    }
  }

  /**
   * Event emitter for partial messages.
   * Implements the vscode-jsonrpc Event<T> interface.
   */
  readonly onPartialMessage: Event<PartialMessageInfo> = (
    listener: (info: PartialMessageInfo) => void
  ): Disposable => {
    this.partialMessageListener = listener
    return {
      dispose: () => {
        this.partialMessageListener = null
      },
    }
  }

  /**
   * Emit a close event
   */
  emitClose(): void {
    if (this.closeListener) {
      this.closeListener()
    }
  }

  dispose(): void {
    this.disposed = true
    this.callback = null
    this.errorListener = null
    this.closeListener = null
    this.partialMessageListener = null
  }
}

/**
 * A MessageWriter that sends messages to the WasmLspHandler.
 *
 * This adapter allows the language client to send messages (requests and
 * notifications) that will be processed by the WASM module.
 */
class WasmMessageWriter implements MessageWriter {
  private errorListener:
    | ((error: [Error, Message | undefined, number | undefined]) => void)
    | null = null
  private closeListener: (() => void) | null = null
  private disposed = false

  constructor(private handler: WasmLspHandler) {}

  async write(msg: Message): Promise<void> {
    if (this.disposed) return
    await this.handler.handleMessage(msg as LspMessage)
  }

  end(): void {
    // No-op for WASM
  }

  /**
   * Event emitter for errors.
   * Implements the vscode-jsonrpc Event<T> interface.
   */
  readonly onError: Event<[Error, Message | undefined, number | undefined]> = (
    listener: (error: [Error, Message | undefined, number | undefined]) => void
  ): Disposable => {
    this.errorListener = listener
    return {
      dispose: () => {
        this.errorListener = null
      },
    }
  }

  /**
   * Event emitter for close.
   * Implements the vscode-jsonrpc Event<T> interface.
   */
  readonly onClose: Event<void> = (listener: () => void): Disposable => {
    this.closeListener = listener
    return {
      dispose: () => {
        this.closeListener = null
      },
    }
  }

  dispose(): void {
    this.disposed = true
    this.errorListener = null
    this.closeListener = null
  }
}

/**
 * Create MessageTransports that route messages through the WASM bridge.
 *
 * This allows the WASM LSP handler to be used with MonacoLanguageClient
 * just like a WebSocket connection.
 *
 * @param bridge - The initialized L4WasmBridge
 * @returns MessageTransports compatible with vscode-languageclient
 */
export function createWasmMessageTransports(
  bridge: L4WasmBridge
): MessageTransports {
  const handler = createWasmLspHandler(bridge)
  const reader = new WasmMessageReader()
  const writer = new WasmMessageWriter(handler)

  // Connect handler's outgoing messages to the reader
  handler.onMessage((message) => {
    reader.pushMessage(message as Message)
  })

  return { reader, writer }
}

/**
 * Create MessageTransports from an existing WasmLspHandler.
 *
 * Use this if you need access to both the handler and the transports.
 *
 * @param handler - An existing WasmLspHandler
 * @returns Object containing reader, writer, and a dispose function
 */
export function createWasmMessageTransportsFromHandler(
  handler: WasmLspHandler
): MessageTransports & { disposeHandler: () => void } {
  const reader = new WasmMessageReader()
  const writer = new WasmMessageWriter(handler)

  // Connect handler's outgoing messages to the reader
  handler.onMessage((message) => {
    reader.pushMessage(message as Message)
  })

  return {
    reader,
    writer,
    disposeHandler: () => handler.dispose(),
  }
}
