/**
 * Low-level WASM bridge for L4 language features.
 * 
 * This module handles:
 * - Loading and instantiating the WASM module
 * - Calling exported functions via GHC's JS FFI
 *
 * The WASM module is built from jl4-core using GHC's WASM backend.
 * It exports JavaScript-callable functions using `foreign export javascript`.
 *
 * @see https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html
 */

/**
 * Interface for the L4 WASM module's JavaScript exports.
 *
 * These correspond to `foreign export javascript` declarations in Haskell.
 * GHC's JS FFI handles marshalling automatically - we pass JS values directly.
 */
export interface L4WasmExports {
  /**
   * Parse and type-check L4 source code.
   * @param source - L4 source code
   * @returns JSON-encoded array of diagnostics
   */
  l4_check(source: string): string

  /**
   * Get hover information at a position.
   * @param source - L4 source code
   * @param line - 0-indexed line number
   * @param column - 0-indexed column number
   * @returns JSON-encoded hover info or "null"
   */
  l4_hover(source: string, line: number, column: number): string

  /**
   * Get completion suggestions at a position.
   * @param source - L4 source code
   * @param line - 0-indexed line number
   * @param column - 0-indexed column number
   * @returns JSON-encoded array of completion items
   */
  l4_completions(source: string, line: number, column: number): string

  /**
   * Get semantic tokens for syntax highlighting.
   * @param source - L4 source code
   * @returns JSON-encoded semantic tokens
   */
  l4_semantic_tokens(source: string): string

  /**
   * Evaluate L4 source code and return #EVAL directive results.
   * @param source - L4 source code with #EVAL directives
   * @returns JSON-encoded evaluation results
   */
  l4_eval(source: string): Promise<string>

  /**
   * Initialize the Haskell RTS (called automatically on load).
   */
  hs_init?(): void
}

/**
 * L4 WASM Bridge
 * 
 * Provides a high-level interface to the L4 WASM module.
 * Handles loading, caching, and provides typed wrappers around exports.
 */
export class L4WasmBridge {
  private exports: L4WasmExports | null = null
  
  constructor(
    private wasmUrl: string,
    private version: string
  ) {}
  
  /**
   * Initialize the WASM module
   */
  async initialize(): Promise<void> {
    const module = await this.loadModule()
    
    // GHC WASM modules export functions directly to the instance exports
    // The JS FFI means we don't need to manage memory manually
    this.exports = module as unknown as L4WasmExports

    // Initialize Haskell RTS if needed
    if (this.exports.hs_init) {
      this.exports.hs_init()
    }

    console.log('[L4 WASM] Module initialized')
  }
  
  /**
   * Load WASM module with caching
   */
  private async loadModule(): Promise<WebAssembly.Exports> {
    const cacheKey = `l4-wasm-${this.version}`
    
    // Try cache first
    try {
      const cache = await caches.open('l4-wasm-cache-v1')
      const cached = await cache.match(cacheKey)
      
      if (cached) {
        console.log(`[L4 WASM] Loading from cache: ${cacheKey}`)
        const buffer = await cached.arrayBuffer()
        const module = await WebAssembly.compile(buffer)
        const instance = await WebAssembly.instantiate(module)
        return instance.exports
      }
    } catch (e) {
      console.warn('[L4 WASM] Cache not available:', e)
    }
    
    // Fetch fresh
    console.log(`[L4 WASM] Fetching: ${this.wasmUrl}`)
    const response = await fetch(this.wasmUrl)
    
    if (!response.ok) {
      throw new Error(
        `Failed to fetch WASM: ${response.status} ${response.statusText}`
      )
    }
    
    // Cache for future use
    try {
      const cache = await caches.open('l4-wasm-cache-v1')
      await cache.put(cacheKey, response.clone())
      console.log(`[L4 WASM] Cached: ${cacheKey}`)
    } catch (e) {
      console.warn('[L4 WASM] Failed to cache:', e)
    }

    const { instance } = await WebAssembly.instantiateStreaming(response)
    return instance.exports
  }
  
  /**
   * Check if initialized
   */
  isReady(): boolean {
    return this.exports !== null
  }
  
  /**
   * Parse and type-check source code
   */
  check(source: string): Diagnostic[] {
    if (!this.exports) {
      throw new Error('WASM not initialized')
    }
    const json = this.exports.l4_check(source)
    return JSON.parse(json)
  }
  
  /**
   * Get hover information at a position
   */
  hover(source: string, line: number, character: number): Hover | null {
    if (!this.exports) {
      throw new Error('WASM not initialized')
    }
    const json = this.exports.l4_hover(source, line, character)
    return JSON.parse(json)
  }
  
  /**
   * Get completions at a position
   */
  completions(source: string, line: number, character: number): CompletionItem[] {
    if (!this.exports) {
      throw new Error('WASM not initialized')
    }
    const json = this.exports.l4_completions(source, line, character)
    return JSON.parse(json)
  }
  
  /**
   * Get semantic tokens
   */
  semanticTokens(source: string): SemanticTokens {
    if (!this.exports) {
      throw new Error('WASM not initialized')
    }
    const json = this.exports.l4_semantic_tokens(source)
    return JSON.parse(json)
  }

  /**
   * Evaluate L4 source code
   */
  async evaluate(source: string): Promise<EvalResult> {
    if (!this.exports) {
      throw new Error('WASM not initialized')
    }
    const json = await this.exports.l4_eval(source)
    return JSON.parse(json)
  }
  
  /**
   * Clean up resources
   */
  dispose(): void {
    this.exports = null
  }
}

// LSP-compatible types

export interface Position {
  line: number
  character: number
}

export interface Range {
  start: Position
  end: Position
}

export interface Diagnostic {
  range: Range
  severity: number // 1=Error, 2=Warning, 3=Info, 4=Hint
  message: string
  source: string
}

export interface Hover {
  contents: {
    kind: 'markdown' | 'plaintext'
    value: string
  }
  range?: Range
}

export interface CompletionItem {
  label: string
  kind: number // CompletionItemKind
  detail?: string
  documentation?: string
  insertText?: string
}

export interface SemanticTokens {
  data: number[] // Delta-encoded token data
}

export interface EvalResultSuccess {
  success: true
  results: EvalDirectiveResult[]
}

export interface EvalResultFailure {
  success: false
  diagnostics: Diagnostic[]
}

export type EvalResult = EvalResultSuccess | EvalResultFailure

export interface EvalDirectiveResult {
  result: string
  success: boolean
  range?: Range
}
