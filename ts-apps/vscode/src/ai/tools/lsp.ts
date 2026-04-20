import * as vscode from 'vscode'
import { resolveFileUri } from './fs.js'

/**
 * LSP-backed diagnostics tool. Reads the current diagnostics VSCode has
 * for the target file (populated by jl4-lsp's publishDiagnostics), so
 * the model can verify a write passed type-check without a separate
 * compiler invocation. The LSP keeps diagnostics up-to-date per open
 * document; we open the document first to make sure it's loaded.
 */

export interface LspDiagnosticsArgs {
  path: string
  /** Optional source filter. `"jl4"` returns only the L4 language-
   *  server's diagnostics; omitted returns everything VSCode knows
   *  about the file. Default: no filter. */
  source?: string
}

type DiagRow = {
  line: number
  column: number
  severity: 'error' | 'warning' | 'info' | 'hint'
  message: string
  source: string
  code?: string | number
}

const DIAGNOSTIC_SETTLE_MS = 400

export async function lspDiagnostics(
  args: LspDiagnosticsArgs
): Promise<string> {
  if (!args.path) throw new Error('lsp__diagnostics: `path` is required')
  const uri = resolveFileUri(args.path)
  if (!uri) {
    throw new Error(
      `lsp__diagnostics: cannot resolve path ${args.path}. Only files inside a loaded workspace folder are supported.`
    )
  }
  // Opening a document triggers the LSP to parse + type-check it. If
  // it's already open in an editor this is effectively a no-op.
  try {
    await vscode.workspace.openTextDocument(uri)
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err)
    throw new Error(`lsp__diagnostics: failed to open ${args.path}: ${msg}`)
  }
  // Give the server a moment to finish if this is a fresh open. The
  // diagnostics collection updates async via publishDiagnostics.
  await new Promise((r) => setTimeout(r, DIAGNOSTIC_SETTLE_MS))

  const all = vscode.languages.getDiagnostics(uri)
  const filtered = args.source
    ? all.filter(
        (d) => (d.source ?? '').toLowerCase() === args.source!.toLowerCase()
      )
    : all
  const rows: DiagRow[] = filtered.map((d) => ({
    line: d.range.start.line + 1,
    column: d.range.start.character + 1,
    severity: severityName(d.severity),
    message: d.message,
    source: d.source ?? '',
    ...(d.code !== undefined
      ? {
          code:
            typeof d.code === 'object' && d.code !== null && 'value' in d.code
              ? (d.code as { value: string | number }).value
              : (d.code as string | number),
        }
      : {}),
  }))
  const counts = {
    error: rows.filter((r) => r.severity === 'error').length,
    warning: rows.filter((r) => r.severity === 'warning').length,
    info: rows.filter((r) => r.severity === 'info').length,
    hint: rows.filter((r) => r.severity === 'hint').length,
  }
  const rel = vscode.workspace.asRelativePath(uri, false)
  return JSON.stringify(
    {
      path: rel,
      total: rows.length,
      counts,
      diagnostics: rows,
    },
    null,
    2
  )
}

function severityName(
  s: vscode.DiagnosticSeverity | undefined
): DiagRow['severity'] {
  switch (s) {
    case vscode.DiagnosticSeverity.Error:
      return 'error'
    case vscode.DiagnosticSeverity.Warning:
      return 'warning'
    case vscode.DiagnosticSeverity.Information:
      return 'info'
    case vscode.DiagnosticSeverity.Hint:
      return 'hint'
    default:
      return 'info'
  }
}
