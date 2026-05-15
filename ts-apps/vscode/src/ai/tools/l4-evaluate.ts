import * as vscode from 'vscode'
import { resolveFileUri } from './fs.js'
import { fetchL4Diagnostics } from './lsp.js'
import {
  awaitDirectiveResults,
  createDirectiveSnapshotStore,
  getCachedDirectiveResults,
  hasCachedDirectiveResults,
  renderDirectiveResults,
} from './directive-snapshot.js'

/**
 * l4__evaluate — report the L4 language server's most-recent evaluation
 * results for a file's directives (`#EVAL`, `#CHECK`, `#TRACE`, …).
 *
 * The jl4-lsp broadcasts every directive result via the
 * `l4/directiveResultsUpdated` notification after each compile.
 * `directive-snapshot.ts` owns the per-URI live cache and the shared
 * snapshot factory.
 *
 * Output modes:
 *   - `"full"` (default) — every directive verbose. Maximises utility
 *     for the model when it explicitly invokes the tool.
 *   - `"changed"` — only directives added or whose value changed since
 *     the last l4__evaluate call; everything else folds into a count.
 *     Heavy bulk runs (e.g. 44 scenarios where 43 are unchanged)
 *     compress from thousands of tokens to dozens. When nothing moved,
 *     collapses to a single `"<N> unchanged"` header.
 *
 * Either way the snapshot is refreshed every call so subsequent
 * `"changed"` queries always diff against the most recent baseline.
 */

// Public re-exports so `extension.mts` and other consumers don't need
// to know about the new file split.
export type { DirectiveResultRow } from './directive-snapshot.js'
export { recordDirectiveResults } from './directive-snapshot.js'

// Private snapshot store for l4__evaluate. fs__edit_file uses its own
// store instance (see fs.ts) so the two tools don't pollute each
// other's "what did I last report" view.
const evaluateStore = createDirectiveSnapshotStore()

export type L4EvaluateMode = 'changed' | 'full'

export interface L4EvaluateArgs {
  path: string
  /** How long to wait (ms) for a fresh LSP push if the cache is cold. */
  timeoutMs?: number
  /** Output mode. Default `"full"`. */
  mode?: L4EvaluateMode
}

export async function l4Evaluate(args: L4EvaluateArgs): Promise<string> {
  if (!args.path) throw new Error('l4__evaluate: `path` is required')
  const uri = resolveFileUri(args.path)
  if (!uri) {
    throw new Error(
      `l4__evaluate: cannot resolve path ${args.path}. Only files inside a loaded workspace folder are supported.`
    )
  }
  // Loading the doc triggers an LSP compile if cold; if already open
  // it's a no-op. We hold a reference only to keep VSCode from
  // garbage-collecting the in-memory TextDocument before the LSP
  // settles.
  let doc: vscode.TextDocument
  try {
    doc = await vscode.workspace.openTextDocument(uri)
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err)
    throw new Error(`l4__evaluate: failed to open ${args.path}: ${msg}`)
  }

  // Diagnostics gate: if the file fails to type-check, directive
  // evaluation results are either stale (from a prior good compile)
  // or empty (the LSP skipped evaluation this round) — return
  // diagnostics so the model fixes the errors first. Match `fs.ts`'s
  // edit-time check: gate strictly on `\d+ error[s]` in the
  // diagnostic header. Info/hint diagnostics (jl4 emits one per
  // directive trace) and warnings don't suppress evaluation — the
  // model still wants the directive results in those cases.
  const diagnostics = await fetchL4Diagnostics(uri).catch(() => null)
  if (diagnostics && /\b\d+ errors?\b/.test(diagnostics)) {
    return diagnostics
  }

  const uriStr = uri.toString()
  const timeoutMs = Math.min(args.timeoutMs ?? 6000, 15000)
  if (!hasCachedDirectiveResults(uriStr)) {
    await awaitDirectiveResults(uriStr, timeoutMs)
  }
  const results = getCachedDirectiveResults(uriStr) ?? []
  const rel = vscode.workspace.asRelativePath(uri, false)
  const mode = args.mode ?? 'full'

  const directiveLines = results
    .map((r) => parseLineFromDirectiveId(r.directiveId))
    .filter((n) => n > 0)
  const header = buildHeader(rel, directiveLines, doc.lineCount)
  const block = renderDirectiveResults({
    results,
    store: evaluateStore,
    uri: uriStr,
    mode,
  })

  if (results.length === 0) {
    return `${header}\n${block}\n(Add a #EVAL line referencing a function, or wait for the LSP to finish compiling.)`
  }
  return `${header}\n${block}`
}

/**
 * Build the `[<path> <start>-<end>/<total>]` header — same shape
 * `fs__read_file` uses so the model can scan tool outputs uniformly.
 * `start-end` is the line span of the directives this call evaluated
 * (min..max of their source lines); `total` is the file's line count.
 * When there are no parseable directive lines, falls back to `0/total`
 * so the row still anchors to the file.
 */
function buildHeader(
  rel: string,
  directiveLines: number[],
  totalFileLines: number
): string {
  if (directiveLines.length === 0) return `[${rel} 0/${totalFileLines}]`
  const min = Math.min(...directiveLines)
  const max = Math.max(...directiveLines)
  return `[${rel} ${min}-${max}/${totalFileLines}]`
}

function parseLineFromDirectiveId(directiveId: string): number {
  const head = directiveId.split(':', 1)[0]
  const n = Number(head)
  return Number.isFinite(n) ? n : 0
}
