import { createHash } from 'node:crypto'
import * as vscode from 'vscode'
import { resolveFileUri } from './fs.js'
import { fetchL4Diagnostics } from './lsp.js'

/**
 * l4__evaluate — report the L4 language server's most-recent evaluation
 * results for a file's directives (`#EVAL`, `#CHECK`, `#TRACE`, …).
 *
 * The jl4-lsp broadcasts every directive result via the
 * `l4/directiveResultsUpdated` notification after each compile. We tap
 * that notification from extension.mts (see `registerDirectiveCache`
 * below) and mirror the latest per-URI payload here. When the tool
 * runs, it opens the target file (forcing a compile if it's cold) and
 * returns the cached results.
 *
 * Output modes (default `"changed"`):
 *   - `"changed"`  — verbose only for directives that changed value, were
 *                    added, or failed; everything else collapses to a count.
 *                    Heavy bulk runs (e.g. 44 scenarios where 43 are
 *                    unchanged) shrink from thousands of tokens to dozens.
 *   - `"full"`     — every directive verbose. Today's behaviour.
 *   - `"summary"`  — counts only; no values. Cheap "did anything regress?".
 *
 * Optional filters:
 *   - `lineRange: [start, end]` — keep only directives whose source line
 *                                 falls in the inclusive range.
 *   - `directiveIds: string[]`  — explicit allowlist.
 *
 * Per-URI snapshot cache: we remember the last-reported set of results
 * and the SHA-1 of the document text at that moment. On a re-call:
 *   - if the document hasn't changed AND we already reported these
 *     results, we short-circuit with `{ unchanged: true, count }`.
 *   - otherwise we diff directive-by-directive against the snapshot.
 * The snapshot updates only when we successfully report a non-empty
 * verbose result, so a `"summary"` call doesn't blind the next
 * `"changed"` call.
 */

export type DirectiveResultRow = {
  directiveId: string
  prettyText: string
  success: boolean | null
  lineContent: string
}

type ResultsCache = Map<string, DirectiveResultRow[]>

// Shared cache: populated by extension.mts, read by the tool executor.
// A module-level singleton is the cheapest way to bridge the LSP
// notification stream and an on-demand tool call without plumbing
// handles through every layer.
const cache: ResultsCache = new Map()
const waiters = new Map<string, Array<() => void>>()

/**
 * Per-URI snapshot of what we last *reported* to the model, plus the
 * content hash at that moment. The change-diff key is identity of
 * (contentHash, directiveId, prettyText) — re-edits that don't touch
 * a directive's value are nearly free, full re-edits report a clean
 * delta.
 */
type ReportSnapshot = {
  /** SHA-1 of the document text at the moment of the prior report. */
  contentHash: string
  /** Map directiveId → its prettyText at the prior report. */
  byId: Map<string, string>
}
const lastReported: Map<string, ReportSnapshot> = new Map()

/** Called from extension.mts whenever `l4/directiveResultsUpdated`
 *  fires. Updates the cache and wakes anyone awaiting fresh results
 *  for that URI. */
export function recordDirectiveResults(
  uri: string,
  results: DirectiveResultRow[]
): void {
  cache.set(uri, results)
  const list = waiters.get(uri)
  if (list) {
    waiters.delete(uri)
    for (const fn of list) fn()
  }
}

/** Await the NEXT push of results for `uri`, or resolve immediately
 *  if a push arrives before the timeout. */
function waitForNextResults(uri: string, timeoutMs: number): Promise<void> {
  return new Promise((resolve) => {
    const list = waiters.get(uri) ?? []
    list.push(resolve)
    waiters.set(uri, list)
    setTimeout(() => {
      const still = waiters.get(uri)
      if (still) {
        waiters.set(
          uri,
          still.filter((fn) => fn !== resolve)
        )
      }
      resolve()
    }, timeoutMs)
  })
}

export type L4EvaluateMode = 'changed' | 'full' | 'summary'

export interface L4EvaluateArgs {
  path: string
  /** How long to wait (ms) for a fresh LSP push if the cache is cold. */
  timeoutMs?: number
  /** Inclusive [start, end] line filter. Lines are 1-based source lines. */
  lineRange?: [number, number]
  /** Explicit directive-id allowlist. */
  directiveIds?: string[]
  /** Output mode. Default `"changed"`. */
  mode?: L4EvaluateMode
}

interface DirectiveOut {
  directiveId: string
  line: string
  success: boolean | null
  value: string
}

interface RenderedResult {
  /** Verbose entries to include in the response. */
  changed: DirectiveOut[]
  /** Entries added since the last report (verbose). */
  added: DirectiveOut[]
  /** Entries whose value matched the last report. */
  unchangedCount: number
  /** Failing directives — always verbose regardless of mode. */
  failing: DirectiveOut[]
}

function toOut(r: DirectiveResultRow): DirectiveOut {
  return {
    directiveId: r.directiveId,
    line: r.lineContent,
    success: r.success,
    value: r.prettyText,
  }
}

function applyFilters(
  rows: DirectiveResultRow[],
  args: L4EvaluateArgs
): DirectiveResultRow[] {
  let out = rows
  if (args.directiveIds && args.directiveIds.length > 0) {
    const allow = new Set(args.directiveIds)
    out = out.filter((r) => allow.has(r.directiveId))
  }
  if (args.lineRange) {
    const [s, e] = args.lineRange
    // The LSP populates lineContent with the source line text, not a
    // number, so we have nothing line-numeric to filter on directly.
    // Match the directiveId convention used by the LSP: it includes a
    // `:<line>` suffix for source-anchored directives. If neither
    // affordance is available, fall through unfiltered so the caller
    // doesn't get a confusing empty result.
    out = out.filter((r) => {
      const m = r.directiveId.match(/(?:^|:)(\d+)(?:$|:)/)
      if (!m) return true
      const ln = Number(m[1])
      return ln >= s && ln <= e
    })
  }
  return out
}

function diffAgainstSnapshot(
  filtered: DirectiveResultRow[],
  snapshot: ReportSnapshot | undefined
): RenderedResult {
  const changed: DirectiveOut[] = []
  const added: DirectiveOut[] = []
  const failing: DirectiveOut[] = []
  let unchangedCount = 0

  for (const r of filtered) {
    const prior = snapshot?.byId.get(r.directiveId)
    const out = toOut(r)
    if (r.success === false) {
      failing.push(out)
      continue
    }
    if (prior === undefined) {
      added.push(out)
    } else if (prior !== r.prettyText) {
      changed.push(out)
    } else {
      unchangedCount++
    }
  }

  return { changed, added, unchangedCount, failing }
}

function buildPayload(
  args: L4EvaluateArgs,
  rel: string,
  filtered: DirectiveResultRow[],
  rendered: RenderedResult,
  unchangedTotal: boolean
): Record<string, unknown> {
  const mode = args.mode ?? 'changed'
  const totalCount = filtered.length
  const payload: Record<string, unknown> = {
    path: rel,
    count: totalCount,
    mode,
  }

  if (totalCount === 0) {
    payload.note =
      'No directives evaluated. Add a #EVAL line referencing a function, ' +
      'or wait for the LSP to finish compiling.'
    return payload
  }

  if (mode === 'summary') {
    payload.summary = {
      changed: rendered.changed.length,
      added: rendered.added.length,
      failing: rendered.failing.length,
      unchanged: rendered.unchangedCount,
    }
    if (rendered.failing.length > 0) {
      // Even in summary mode we surface failing ids — without them the
      // model has no way to know what to fix.
      payload.failing = rendered.failing.map((f) => ({
        directiveId: f.directiveId,
        line: f.line,
      }))
    }
    return payload
  }

  if (mode === 'full') {
    payload.results = filtered.map(toOut)
    return payload
  }

  // mode === 'changed'
  if (unchangedTotal && rendered.failing.length === 0) {
    payload.unchangedSinceLastReport = true
    payload.unchanged = totalCount
    return payload
  }

  if (rendered.changed.length > 0) payload.changed = rendered.changed
  if (rendered.added.length > 0) payload.added = rendered.added
  if (rendered.failing.length > 0) payload.failing = rendered.failing
  if (rendered.unchangedCount > 0) payload.unchanged = rendered.unchangedCount
  return payload
}

function sha1(s: string): string {
  return createHash('sha1').update(s).digest('hex')
}

export async function l4Evaluate(args: L4EvaluateArgs): Promise<string> {
  if (!args.path) throw new Error('l4__evaluate: `path` is required')
  const uri = resolveFileUri(args.path)
  if (!uri) {
    throw new Error(
      `l4__evaluate: cannot resolve path ${args.path}. Only files inside a loaded workspace folder are supported.`
    )
  }
  // Loading the doc triggers a compile in the LSP if it's cold; if
  // it's already loaded this is a no-op. We also grab `doc.getText()`
  // below so the content hash reflects the same view the LSP just
  // evaluated.
  let doc: vscode.TextDocument
  try {
    doc = await vscode.workspace.openTextDocument(uri)
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err)
    throw new Error(`l4__evaluate: failed to open ${args.path}: ${msg}`)
  }

  // Diagnostics gate: if the file doesn't type-check, directive
  // evaluation results are either stale (from a prior good compile)
  // or empty (the LSP skipped evaluation this round). Either way
  // they're misleading — return the diagnostics text instead so the
  // model fixes the errors first. `fetchL4Diagnostics` already opens
  // the doc + waits for publishDiagnostics to settle, so we don't
  // need an extra settle delay here.
  const diagnostics = await fetchL4Diagnostics(uri).catch(() => null)
  if (diagnostics && !/:\s*clean\s*---/.test(diagnostics)) {
    return diagnostics
  }

  const uriStr = uri.toString()
  const timeoutMs = Math.min(args.timeoutMs ?? 6000, 15000)
  if (!cache.has(uriStr)) {
    // Cache miss — wait for the next directiveResultsUpdated for this
    // URI. If evaluation fails or the file has no directives, the LSP
    // still posts an empty list.
    await waitForNextResults(uriStr, timeoutMs)
  }
  const results = cache.get(uriStr) ?? []
  const rel = vscode.workspace.asRelativePath(uri, false)
  const mode = args.mode ?? 'changed'
  const filtered = applyFilters(results, args)

  // Content-hash short-circuit: if the document is byte-identical to
  // the moment we last reported AND every directive's prettyText
  // matches the snapshot, the caller is re-reading without changes.
  // Skip serialisation, return a degenerate response.
  const contentHash = sha1(doc.getText())
  const snapshot = lastReported.get(uriStr)
  const allUnchanged =
    snapshot !== undefined &&
    snapshot.contentHash === contentHash &&
    filtered.length === snapshot.byId.size &&
    filtered.every((r) => snapshot.byId.get(r.directiveId) === r.prettyText)

  const rendered = diffAgainstSnapshot(filtered, snapshot)

  const payload = buildPayload(args, rel, filtered, rendered, allUnchanged)

  // Update the snapshot only for modes that actually surfaced values.
  // `"summary"` deliberately doesn't refresh — leaving the prior
  // snapshot intact means a follow-up `"changed"` call still sees the
  // true prior baseline rather than the value-less summary view.
  if (mode !== 'summary' && filtered.length > 0) {
    const byId = new Map<string, string>()
    for (const r of filtered) byId.set(r.directiveId, r.prettyText)
    lastReported.set(uriStr, { contentHash, byId })
  }

  return JSON.stringify(payload, null, 2)
}
