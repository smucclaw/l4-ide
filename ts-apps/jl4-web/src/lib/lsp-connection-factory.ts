/**
 * Factory for creating L4 LSP connections.
 *
 * This module provides an abstraction over different LSP connection strategies:
 * 1. WebSocket (current default) - connects to remote jl4-lsp server
 * 2. WASM (future) - runs L4 analysis in-browser via WebAssembly
 *
 * The factory handles:
 * - Connection type selection based on configuration and availability
 * - Versioned caching of WASM binaries
 * - Graceful fallback between connection types
 */

import type { MessageTransports } from 'vscode-languageclient'

/** Connection type enumeration */
export type LspConnectionType = 'websocket' | 'wasm'

/** Configuration for LSP connection */
export interface LspConnectionConfig {
  /** Preferred connection type */
  preferredType: LspConnectionType
  /** WebSocket URL for remote LSP server */
  websocketUrl: string
  /** URL to fetch WASM binary (if using WASM mode) */
  wasmUrl?: string
  /** URL to fetch WASM JS FFI glue code (if using WASM mode) */
  wasmJsUrl?: string
  /** Version string for WASM caching */
  wasmVersion?: string
  /** Whether to fall back to alternative connection if preferred fails */
  enableFallback: boolean
}

/** Result of connection attempt */
export interface LspConnectionResult {
  /** The actual connection type used */
  type: LspConnectionType
  /** Message transports for the language client */
  transports: MessageTransports
  /** Cleanup function */
  dispose: () => Promise<void>
}

/** Cache name for WASM binaries */
const WASM_CACHE_NAME = 'l4-wasm-cache-v1'

/**
 * Check if WASM is available and can be used
 */
export function isWasmAvailable(): boolean {
  return (
    typeof WebAssembly !== 'undefined' &&
    typeof WebAssembly.instantiateStreaming === 'function'
  )
}

/**
 * Clear old WASM versions from cache
 */
export async function clearOldWasmCache(currentVersion: string): Promise<void> {
  const cache = await caches.open(WASM_CACHE_NAME)
  const keys = await cache.keys()

  const currentKey = `l4-wasm-${currentVersion}`

  for (const request of keys) {
    // Keep current version, delete others
    if (!request.url.endsWith(currentKey)) {
      console.log(`[L4 WASM] Clearing old cache: ${request.url}`)
      await cache.delete(request)
    }
  }
}

/**
 * Create a WebSocket-based LSP connection
 */
export async function createWebSocketConnection(
  websocketUrl: string
): Promise<LspConnectionResult> {
  // Dynamic imports to support code splitting
  const { WebSocketMessageReader, WebSocketMessageWriter, toSocket } =
    await import('vscode-ws-jsonrpc')

  return new Promise((resolve, reject) => {
    const webSocket = new WebSocket(websocketUrl)

    const timeout = setTimeout(() => {
      webSocket.close()
      reject(new Error(`WebSocket connection timeout: ${websocketUrl}`))
    }, 10000) // 10 second timeout

    webSocket.onerror = (error) => {
      clearTimeout(timeout)
      reject(new Error(`WebSocket connection failed: ${error}`))
    }

    webSocket.onopen = () => {
      clearTimeout(timeout)
      console.log(`[L4 LSP] WebSocket connected: ${websocketUrl}`)

      const socket = toSocket(webSocket)
      const reader = new WebSocketMessageReader(socket)
      const writer = new WebSocketMessageWriter(socket)

      resolve({
        type: 'websocket',
        transports: { reader, writer },
        dispose: async () => {
          reader.dispose()
          writer.dispose()
          webSocket.close()
        },
      })
    }
  })
}

/**
 * Create a WASM-based LSP connection
 *
 * This uses the L4 WASM module to provide language features
 * directly in the browser without a server connection.
 *
 * Returns MessageTransports that are compatible with MonacoLanguageClient,
 * allowing WASM mode to work identically to WebSocket mode from the
 * language client's perspective.
 */
export async function createWasmConnection(
  wasmUrl: string,
  jsUrl: string,
  version: string
): Promise<LspConnectionResult> {
  if (!isWasmAvailable()) {
    throw new Error('WebAssembly is not supported in this browser')
  }

  // Import WASM bridge modules
  const { L4WasmBridge, createWasmMessageTransports } = await import(
    './wasm/index'
  )

  // Create and initialize the WASM bridge
  const bridge = new L4WasmBridge(wasmUrl, jsUrl, version)
  await bridge.initialize()

  // Create MessageTransports that route to the WASM bridge
  // This allows WASM mode to work with MonacoLanguageClient
  const transports = createWasmMessageTransports(bridge)

  console.log('[L4 LSP] WASM connection established')

  return {
    type: 'wasm',
    transports,
    dispose: async () => {
      transports.reader.dispose()
      transports.writer.dispose()
      bridge.dispose()
    },
  }
}

/**
 * Create an LSP connection based on configuration
 */
export async function createLspConnection(
  config: LspConnectionConfig
): Promise<LspConnectionResult> {
  const { preferredType, enableFallback } = config

  if (preferredType === 'wasm') {
    if (!config.wasmUrl || !config.wasmJsUrl || !config.wasmVersion) {
      throw new Error(
        'WASM URL, JS URL, and version are required for WASM mode'
      )
    }

    try {
      return await createWasmConnection(
        config.wasmUrl,
        config.wasmJsUrl,
        config.wasmVersion
      )
    } catch (error) {
      console.warn('[L4 LSP] WASM connection failed:', error)

      if (enableFallback) {
        console.log('[L4 LSP] Falling back to WebSocket')
        return await createWebSocketConnection(config.websocketUrl)
      }

      throw error
    }
  }

  // Default: WebSocket
  try {
    return await createWebSocketConnection(config.websocketUrl)
  } catch (error) {
    console.warn('[L4 LSP] WebSocket connection failed:', error)

    if (
      enableFallback &&
      config.wasmUrl &&
      config.wasmJsUrl &&
      config.wasmVersion
    ) {
      console.log('[L4 LSP] Falling back to WASM')
      return await createWasmConnection(
        config.wasmUrl,
        config.wasmJsUrl,
        config.wasmVersion
      )
    }

    throw error
  }
}

/**
 * Get default configuration from environment
 */
export function getDefaultConfig(): LspConnectionConfig {
  const websocketUrl = import.meta.env.VITE_SOCKET_URL || 'ws://localhost:5007'
  const wasmUrl = import.meta.env.VITE_WASM_URL || '/wasm/jl4-core.wasm'
  const wasmJsUrl = import.meta.env.VITE_WASM_JS_URL || '/wasm/jl4-core.mjs'
  const wasmVersion = import.meta.env.VITE_WASM_VERSION || 'dev'
  const preferWasm = import.meta.env.VITE_PREFER_WASM === 'true'

  return {
    preferredType: preferWasm ? 'wasm' : 'websocket',
    websocketUrl,
    wasmUrl,
    wasmJsUrl,
    wasmVersion,
    enableFallback: true,
  }
}
