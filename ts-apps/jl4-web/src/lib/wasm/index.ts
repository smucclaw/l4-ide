/**
 * WASM module exports
 * 
 * This module provides WebAssembly-based L4 language features
 * for running in the browser without a server connection.
 */

export { L4WasmBridge } from './wasm-bridge'
export type {
  L4WasmExports,
  Diagnostic,
  Hover,
  CompletionItem,
  SemanticTokens,
  Position,
  Range,
} from './wasm-bridge'

export {
  WasmLspHandler,
  createWasmLspHandler,
  createWasmMessageTransports,
  createWasmMessageTransportsFromHandler,
} from './wasm-message-transports'
