import * as monaco from '@codingame/monaco-vscode-editor-api'
import type { MonacoMarkerData, MonacoModule } from '@ym-han/monaco-error-lens'

const convertMarkers = (
  markers: monaco.editor.IMarker[]
): MonacoMarkerData[] => {
  return markers.map((marker) => ({
    startLineNumber: marker.startLineNumber,
    startColumn: marker.startColumn,
    endLineNumber: marker.endLineNumber,
    endColumn: marker.endColumn,
    message: marker.message,
    severity: marker.severity,
    source: marker.source,
    code: typeof marker.code === 'object' ? marker.code?.value : marker.code,
  }))
}

export const monacoModuleWrapperForErrorLens: MonacoModule = {
  editor: {
    getModelMarkers: (options: { resource: unknown }) => {
      const filterObj = options.resource
        ? { resource: options.resource as monaco.Uri }
        : {}
      return convertMarkers(monaco.editor.getModelMarkers(filterObj))
    },
    onDidChangeMarkers: (
      callback: (resources: { toString(): string }[]) => void
    ) => {
      return monaco.editor.onDidChangeMarkers((uris: readonly monaco.Uri[]) => {
        callback([...uris])
      })
    },
  },
}
