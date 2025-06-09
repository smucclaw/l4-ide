import type * as monacoType from '@codingame/monaco-vscode-editor-api'

declare global {
  interface Window {
    monaco: typeof monacoType
  }
}
