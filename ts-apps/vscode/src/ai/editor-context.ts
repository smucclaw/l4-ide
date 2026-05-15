import * as vscode from 'vscode'
import type { AiChatMessage } from 'jl4-client-rpc'
import type { AuthManager } from '../auth.js'

/**
 * Build the per-turn `<editor-context>` system message that tells the
 * model what the user is currently looking at. Only references the
 * active file by path — the model is expected to call `fs__read_file`
 * if it decides the body is worth reading. Sent as a second system
 * message (after ai-proxy's cached L4 reference) so it doesn't break
 * the provider's prompt cache. Only built when the user has the
 * "attach active file" chip enabled for this turn.
 *
 * `chipSnapshot`, when provided, is the active-file state the
 * webview was showing at send time. It overrides
 * `vscode.window.activeTextEditor` so the system message reflects
 * exactly what the user saw on the chip — even across multi-window
 * setups (each window has its own activeTextEditor) or focus
 * changes between chip update and request assembly. cursorLine /
 * selection / openFiles still come from the live editor (those are
 * inherently editor-scoped and the snapshot doesn't carry them).
 */
export function buildEditorContextMessage(chipSnapshot?: {
  name: string
  path: string
}): AiChatMessage | null {
  const editor = vscode.window.activeTextEditor
  const visibleFiles = vscode.window.visibleTextEditors
    .filter(
      (e) => e.document.languageId === 'l4' || e.document.languageId === 'jl4'
    )
    .map((e) => workspaceRelative(e.document.uri))
  if (!chipSnapshot && !editor && visibleFiles.length === 0) return null

  const lines: string[] = []
  lines.push('<editor-context>')

  // Find the editor that's actually showing the chip's path. We look
  // through every visible editor (not just `activeTextEditor`) so a
  // focus race between chip update and request assembly — or the
  // sidebar webview itself momentarily owning focus — can't drop
  // the selection. activeTextEditor is preferred when it matches so
  // split-pane setups use the focused pane.
  let snapshotEditor: vscode.TextEditor | undefined
  if (chipSnapshot) {
    if (
      editor &&
      workspaceRelative(editor.document.uri) === chipSnapshot.path
    ) {
      snapshotEditor = editor
    } else {
      snapshotEditor = vscode.window.visibleTextEditors.find(
        (e) => workspaceRelative(e.document.uri) === chipSnapshot.path
      )
    }
  }
  const snapshotMatchesLiveEditor = !!snapshotEditor

  if (chipSnapshot) {
    // Snapshot wins — use the path the webview chip was showing at
    // send time. Live editor state (cursor / selection / openFiles)
    // only flows through when SOME visible editor in this window
    // shows the same file. The selection range (`selectionLines: A-B`)
    // is emitted as a pointer, not as content — the model can call
    // `fs__read_file` with a line range if it actually wants the
    // bytes, which keeps the system message tight on context.
    lines.push(
      `activeFile: ${chipSnapshot.path} (call fs__read_file on this path if you need the body)`
    )
    if (snapshotEditor) {
      const pos = snapshotEditor.selection.active
      lines.push(`cursorLine: ${pos.line + 1}`)
      if (!snapshotEditor.selection.isEmpty) {
        const startLine = snapshotEditor.selection.start.line + 1
        const endLine = snapshotEditor.selection.end.line + 1
        lines.push(`selectionLines: ${startLine}-${endLine}`)
      }
    }
  } else if (editor) {
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
    if (!editor.selection.isEmpty) {
      const startLine = editor.selection.start.line + 1
      const endLine = editor.selection.end.line + 1
      lines.push(`selectionLines: ${startLine}-${endLine}`)
    }
  }
  // Only surface `openFiles` from the live window when there's no
  // snapshot OR the snapshot points at a file that's also visible
  // in this window. Otherwise the visible-files list belongs to a
  // different VSCode window than the activeFile line and would
  // mislead the model.
  const showOpenFiles = !chipSnapshot || snapshotMatchesLiveEditor
  if (showOpenFiles && visibleFiles.length > 0) {
    lines.push('openFiles:')
    for (const f of visibleFiles) lines.push(`  - ${f}`)
  }
  lines.push('</editor-context>')
  return { role: 'system', content: lines.join('\n') }
}

/**
 * Build a `<session-context>` system message carrying the runtime
 * facts the model otherwise has no way to know: today's date/time
 * (with the user's IANA timezone), the org-specific deployment URL
 * the user is signed in to, and the L4 VSCode extension build the
 * request is coming from. Sent as an additional system message so it
 * doesn't invalidate the ai-proxy's cached L4 prompt prefix, and only
 * on the first turn of a conversation (matches workspace-exports).
 */
export function buildSessionContextMessage(
  auth: AuthManager,
  extensionVersion: string
): AiChatMessage | null {
  const lines: string[] = []
  lines.push('<session-context>')
  const now = new Date()
  const tz = Intl.DateTimeFormat().resolvedOptions().timeZone || 'UTC'
  // ISO timestamp for precision + a human-readable form in the local
  // zone so the model can cite a date naturally.
  lines.push(`currentTime: ${now.toISOString()}`)
  lines.push(
    `localTime: ${now.toLocaleString(undefined, { timeZone: tz, timeZoneName: 'short' })}`
  )
  lines.push(`timezone: ${tz}`)
  const deploymentUrl = auth.getEffectiveServiceUrl()
  if (deploymentUrl) {
    lines.push(`deploymentUrl: ${deploymentUrl}`)
  } else {
    lines.push('deploymentUrl: (none — user not signed in)')
  }
  lines.push(`l4VscodeExtensionVersion: ${extensionVersion}`)
  lines.push('</session-context>')
  return { role: 'system', content: lines.join('\n') }
}

function workspaceRelative(uri: vscode.Uri): string {
  const folder = vscode.workspace.getWorkspaceFolder(uri)
  if (!folder) return uri.fsPath
  return vscode.workspace.asRelativePath(uri, false)
}

/**
 * Build a `<mention-context>` system message for any `@`-mentions on
 * this turn. The user's text already carries the literal `@<label>`
 * token (so the visual chip survives), but the model treats it as an
 * arbitrary string until something tells it the token resolves to a
 * file path it can open. We surface those resolved paths separately
 * so the model knows which fs__read_file calls would actually work.
 *
 * Returns null when there are no mentions, or when every mention is a
 * non-path kind (selection / symbol — those are handled elsewhere).
 */
export function buildMentionContextMessage(
  mentions: Array<{ kind: 'file' | 'symbol' | 'selection'; label: string }>
): AiChatMessage | null {
  if (!mentions || mentions.length === 0) return null
  const files = mentions
    .filter((m) => m.kind === 'file')
    .map((m) => m.label)
    .filter((label) => label.length > 0)
  if (files.length === 0) return null
  // Dedupe while preserving the order the user picked them in.
  const seen = new Set<string>()
  const unique = files.filter((f) =>
    seen.has(f) ? false : (seen.add(f), true)
  )
  const lines: string[] = []
  lines.push('<mention-context>')
  lines.push(
    'The `@<path>` tokens in the user message resolve to these workspace files. Call fs__read_file on the path if you need the body:'
  )
  for (const f of unique) lines.push(`  - ${f}`)
  lines.push('</mention-context>')
  return { role: 'system', content: lines.join('\n') }
}
