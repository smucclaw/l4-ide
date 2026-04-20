import * as vscode from 'vscode'
import type { AiChatMessage } from 'jl4-client-rpc'

/**
 * Build the per-turn `<editor-context>` system message that tells the
 * model what the user is currently looking at. Only references the
 * active file by path — the model is expected to call `fs__read_file`
 * if it decides the body is worth reading. Sent as a second system
 * message (after ai-proxy's cached L4 reference) so it doesn't break
 * the provider's prompt cache. Only built when the user has the
 * "attach active file" chip enabled for this turn.
 */
export function buildEditorContextMessage(): AiChatMessage | null {
  const editor = vscode.window.activeTextEditor
  const visibleFiles = vscode.window.visibleTextEditors
    .filter(
      (e) => e.document.languageId === 'l4' || e.document.languageId === 'jl4'
    )
    .map((e) => workspaceRelative(e.document.uri))
  if (!editor && visibleFiles.length === 0) return null

  const lines: string[] = []
  lines.push('<editor-context>')
  if (editor) {
    const uri = editor.document.uri
    const rel = workspaceRelative(uri)
    const outsideWorkspace =
      uri.scheme === 'file' && !vscode.workspace.getWorkspaceFolder(uri)
    const pos = editor.selection.active
    lines.push(
      outsideWorkspace
        ? `activeFile: ${rel} (outside every loaded workspace folder — fs tools cannot read/edit/delete it)`
        : `activeFile: ${rel} (call fs__read_file on this path if you need the body)`
    )
    lines.push(`cursorLine: ${pos.line + 1}`)
    const sel = editor.document.getText(editor.selection)
    if (sel.trim().length > 0) {
      lines.push('selection: |')
      for (const sline of sel.split('\n')) lines.push(`  ${sline}`)
    }
  }
  if (visibleFiles.length > 0) {
    lines.push('openFiles:')
    for (const f of visibleFiles) lines.push(`  - ${f}`)
  }
  lines.push('</editor-context>')
  return { role: 'system', content: lines.join('\n') }
}

function workspaceRelative(uri: vscode.Uri): string {
  const folder = vscode.workspace.getWorkspaceFolder(uri)
  if (!folder) return uri.fsPath
  return vscode.workspace.asRelativePath(uri, false)
}
