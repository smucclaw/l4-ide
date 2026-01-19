/**
 * WASM Message Transports
 *
 * Provides an LSP-like message handler that routes requests
 * to the WASM bridge instead of a network connection.
 *
 * This enables running L4 language features in the browser
 * without a server connection.
 *
 * Note: This doesn't implement the full vscode-languageclient
 * MessageTransports interface. Instead, it's designed to work
 * with a custom WASM-aware language client setup.
 */

import type { L4WasmBridge } from './wasm-bridge'

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
 * WASM-based LSP message handler
 *
 * Processes LSP messages by routing them to the WASM bridge.
 * Tracks document state and publishes diagnostics.
 */
export class WasmLspHandler {
  private documentContents = new Map<string, string>()
  private messageCallback: MessageCallback | null = null
  private disposed = false

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

  private async handleNotification(notification: LspNotification): Promise<void> {
    const { method, params } = notification
    
    switch (method) {
      case 'textDocument/didOpen': {
        const p = params as {
          textDocument: { uri: string; text: string; version: number }
        }
        this.documentContents.set(p.textDocument.uri, p.textDocument.text)
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
        break
      }
      
      case 'initialized':
      case 'exit':
        break
        
      default:
        console.log('[WASM LSP] Ignoring notification:', method)
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
        
      default:
        console.warn('[WASM LSP] Unhandled method:', method)
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
        // Not implemented in WASM mode
        definitionProvider: false,
        referencesProvider: false,
        codeActionProvider: false,
      },
      serverInfo: { name: 'l4-wasm', version: '0.1.0' },
    }
  }

  private handleHover(params: unknown): unknown {
    const p = params as {
      textDocument: { uri: string }
      position: { line: number; character: number }
    }
    const content = this.documentContents.get(p.textDocument.uri)
    if (!content) return null

    return this.bridge.hover(content, p.position.line, p.position.character)
  }

  private handleCompletion(params: unknown): unknown {
    const p = params as {
      textDocument: { uri: string }
      position: { line: number; character: number }
    }
    const content = this.documentContents.get(p.textDocument.uri)
    if (!content) return { items: [] }
    
    const items = this.bridge.completions(
      content,
      p.position.line,
      p.position.character
    )
    return { items }
  }

  private handleSemanticTokens(params: unknown): unknown {
    const p = params as { textDocument: { uri: string } }
    const content = this.documentContents.get(p.textDocument.uri)
    if (!content) return { data: [] }
    
    return this.bridge.semanticTokens(content)
  }

  private checkAndPublishDiagnostics(
    uri: string,
    content: string,
    version?: number
  ): void {
    const diagnostics = this.bridge.check(content)
    this.send({
      jsonrpc: '2.0',
      method: 'textDocument/publishDiagnostics',
      params: { uri, version, diagnostics },
    } as LspNotification)
  }

  dispose(): void {
    this.disposed = true
    this.documentContents.clear()
    this.messageCallback = null
  }
}

/**
 * Create a WASM LSP handler
 */
export function createWasmLspHandler(bridge: L4WasmBridge): WasmLspHandler {
  return new WasmLspHandler(bridge)
}
