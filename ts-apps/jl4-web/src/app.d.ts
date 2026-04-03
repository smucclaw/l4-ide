// See https://svelte.dev/docs/kit/types#app.d.ts
// for information about these interfaces
declare global {
  namespace App {
    // interface Error {}
    // interface Locals {}
    // interface PageData {}
    // interface PageState {}
    // interface Platform {}
  }
}

// Vite environment variable types
interface ImportMetaEnv {
  /** WebSocket URL for LSP server (e.g., 'ws://localhost:5007') */
  readonly VITE_SOCKET_URL?: string
  /** URL to fetch L4 WASM binary (e.g., '/wasm/l4-core.wasm') */
  readonly VITE_WASM_URL?: string
  /** Version string for WASM binary caching */
  readonly VITE_WASM_VERSION?: string
  /** Set to 'true' to prefer WASM over WebSocket */
  readonly VITE_PREFER_WASM?: string
  /** Session persistence service URL */
  readonly VITE_SESSION_URL?: string
  /** Decision service URL */
  readonly VITE_DECISION_SERVICE_URL?: string
  /** Wizard application URL */
  readonly VITE_WIZARD_URL?: string
}

interface ImportMeta {
  readonly env: ImportMetaEnv
}

export {}
