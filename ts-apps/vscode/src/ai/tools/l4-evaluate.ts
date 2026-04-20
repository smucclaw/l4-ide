import * as vscode from 'vscode'
import { resolveFileUri } from './fs.js'

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
 * A dedicated MCP-style evaluation endpoint (run a function on user-
 * supplied inputs) is a future phase — this first pass only surfaces
 * what the LSP already computes for directives that are literally
 * written into the source.
 */

export type DirectiveResultRow = {
  directiveId: string
  prettyText: string
  success: boolean | null
  lineContent: string
}

type Cache = Map<string, DirectiveResultRow[]>

// Shared cache: populated by extension.mts, read by the tool executor.
// A module-level singleton is the cheapest way to bridge the LSP
// notification stream and an on-demand tool call without plumbing
// handles through every layer.
const cache: Cache = new Map()
const waiters = new Map<string, Array<() => void>>()

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

export interface L4EvaluateArgs {
  path: string
  /** How long to wait (ms) for a fresh LSP push if the cache is cold. */
  timeoutMs?: number
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
  // it's already loaded this is a no-op.
  try {
    await vscode.workspace.openTextDocument(uri)
  } catch (err) {
    const msg = err instanceof Error ? err.message : String(err)
    throw new Error(`l4__evaluate: failed to open ${args.path}: ${msg}`)
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
  return JSON.stringify(
    {
      path: rel,
      count: results.length,
      note:
        results.length === 0
          ? 'No directives evaluated. Add a #EVAL line referencing a function, or wait for the LSP to finish compiling.'
          : undefined,
      results: results.map((r) => ({
        directiveId: r.directiveId,
        line: r.lineContent,
        success: r.success,
        value: r.prettyText,
      })),
    },
    null,
    2
  )
}
