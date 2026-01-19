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
import type { L4LanguageClient } from 'jl4-client-rpc'

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
 * Load WASM module with versioned caching
 */
async function loadWasmCached(
  url: string,
  version: string
): Promise<WebAssembly.Module> {
  const cacheKey = `l4-wasm-${version}`

  // Try to open the cache
  const cache = await caches.open(WASM_CACHE_NAME)

  // Check for cached version
  const cachedResponse = await cache.match(cacheKey)
  if (cachedResponse) {
    console.log(`[L4 WASM] Loading from cache: ${cacheKey}`)
    const buffer = await cachedResponse.arrayBuffer()
    return WebAssembly.compile(buffer)
  }

  // Fetch fresh
  console.log(`[L4 WASM] Fetching: ${url}`)
  const response = await fetch(url)

  if (!response.ok) {
    throw new Error(`Failed to fetch WASM: ${response.status} ${response.statusText}`)
  }

  // Clone for caching (response body can only be consumed once)
  const responseForCache = response.clone()

  // Compile the module
  const module = await WebAssembly.compileStreaming(response)

  // Cache for future use
  await cache.put(cacheKey, responseForCache)
  console.log(`[L4 WASM] Cached: ${cacheKey}`)

  return module
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
 * Create a WASM-based LSP connection (stub for future implementation)
 *
 * This will be implemented when the L4 core is compiled to WASM.
 * For now, it throws an error indicating WASM is not yet available.
 */
export async function createWasmConnection(
  wasmUrl: string,
  version: string
): Promise<LspConnectionResult> {
  if (!isWasmAvailable()) {
    throw new Error('WebAssembly is not supported in this browser')
  }

  // Load the WASM module with caching
  const _module = await loadWasmCached(wasmUrl, version)

  // TODO: Implement WASM-based LSP bridge
  // This requires:
  // 1. l4-wasm-core Haskell package compiled to WASM
  // 2. JS bridge that implements MessageReader/MessageWriter
  // 3. LSP message routing to WASM exports

  throw new Error(
    'WASM LSP is not yet implemented. See doc/dev/specs/todo/WASM-LSP-SPEC.md'
  )
}

/**
 * Create an LSP connection based on configuration
 */
export async function createLspConnection(
  config: LspConnectionConfig
): Promise<LspConnectionResult> {
  const { preferredType, enableFallback } = config

  if (preferredType === 'wasm') {
    if (!config.wasmUrl || !config.wasmVersion) {
      throw new Error('WASM URL and version are required for WASM mode')
    }

    try {
      return await createWasmConnection(config.wasmUrl, config.wasmVersion)
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

    if (enableFallback && config.wasmUrl && config.wasmVersion) {
      console.log('[L4 LSP] Falling back to WASM')
      return await createWasmConnection(config.wasmUrl, config.wasmVersion)
    }

    throw error
  }
}

/**
 * Get default configuration from environment
 */
export function getDefaultConfig(): LspConnectionConfig {
  const websocketUrl = import.meta.env.VITE_SOCKET_URL || 'ws://localhost:5007'
  const wasmUrl = import.meta.env.VITE_WASM_URL || undefined
  const wasmVersion = import.meta.env.VITE_WASM_VERSION || undefined
  const preferWasm = import.meta.env.VITE_PREFER_WASM === 'true'

  return {
    preferredType: preferWasm && wasmUrl ? 'wasm' : 'websocket',
    websocketUrl,
    wasmUrl,
    wasmVersion,
    enableFallback: true,
  }
}
