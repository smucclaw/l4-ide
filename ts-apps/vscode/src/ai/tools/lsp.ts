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
}

type DiagSeverity = 'error' | 'warning' | 'info' | 'hint'

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
  try {
    return await fetchL4Diagnostics(uri)
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err)
    throw new Error(`lsp__diagnostics: failed to open ${args.path}: ${msg}`)
  }
}

/**
 * Shared diagnostics fetcher. Returns a compact, glanceable text
 * block — one header line with counts, then one line per diagnostic
 * (`severity line:col — message [source:code]`). Consumed verbatim
 * by both the explicit `lsp__diagnostics` tool and the fs-tool
 * auto-append, so the model sees the same shape no matter how the
 * diagnostics were surfaced. Format keeps all fields the old JSON
 * exposed (line, column, severity, message, source, code) — nothing
 * is hidden, it's just ~5× fewer tokens than the pretty-printed JSON
 * for the same payload.
 */
export async function fetchL4Diagnostics(uri: vscode.Uri): Promise<string> {
  // Opening a document triggers the LSP to parse + type-check it. If
  // it's already open in an editor this is effectively a no-op.
  await vscode.workspace.openTextDocument(uri)
  // Give the server a moment to finish if this is a fresh open. The
  // diagnostics collection updates async via publishDiagnostics.
  await new Promise((r) => setTimeout(r, DIAGNOSTIC_SETTLE_MS))

  // Scan all collections and match by fsPath instead of relying on
  // exact URI equality — `vscode.Uri.file(path).toString()` can subtly
  // differ from what the LSP published with (casing, encoding, etc.).
  const everything = vscode.languages.getDiagnostics()
  const match = everything.find(
    ([u]) =>
      u.fsPath === uri.fsPath ||
      u.toString() === uri.toString() ||
      u.fsPath.toLowerCase() === uri.fsPath.toLowerCase()
  )
  const filtered = match?.[1] ?? []
  const rel = vscode.workspace.asRelativePath(uri, false)
  if (filtered.length === 0) {
    return `--- L4 diagnostics for ${rel}: clean ---`
  }
  const counts = {
    error: filtered.filter(
      (d) => d.severity === vscode.DiagnosticSeverity.Error
    ).length,
    warning: filtered.filter(
      (d) => d.severity === vscode.DiagnosticSeverity.Warning
    ).length,
    info: filtered.filter(
      (d) => d.severity === vscode.DiagnosticSeverity.Information
    ).length,
    hint: filtered.filter((d) => d.severity === vscode.DiagnosticSeverity.Hint)
      .length,
  }
  const summary = [
    counts.error ? `${counts.error} error${counts.error === 1 ? '' : 's'}` : '',
    counts.warning
      ? `${counts.warning} warning${counts.warning === 1 ? '' : 's'}`
      : '',
    counts.info ? `${counts.info} info` : '',
    counts.hint ? `${counts.hint} hint${counts.hint === 1 ? '' : 's'}` : '',
  ]
    .filter(Boolean)
    .join(', ')
  const header = `--- L4 diagnostics for ${rel}: ${filtered.length} issue${filtered.length === 1 ? '' : 's'} (${summary}) ---`
  const lines = filtered.map((d) => {
    const sev = severityName(d.severity)
    const line = d.range.start.line + 1
    const col = d.range.start.character + 1
    const source = d.source ?? ''
    const code =
      d.code === undefined
        ? ''
        : typeof d.code === 'object' && d.code !== null && 'value' in d.code
          ? String((d.code as { value: string | number }).value)
          : String(d.code)
    // `[source:code]` only when there's at least one of the two, to
    // avoid trailing brackets on diagnostics that carry neither.
    const tag =
      source || code ? ` [${[source, code].filter(Boolean).join(':')}]` : ''
    return `  ${sev} ${line}:${col} — ${d.message}${tag}`
  })
  return `${header}\n${lines.join('\n')}`
}

function severityName(s: vscode.DiagnosticSeverity | undefined): DiagSeverity {
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
