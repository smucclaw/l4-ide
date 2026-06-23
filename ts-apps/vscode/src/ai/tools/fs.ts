import * as vscode from 'vscode'
import * as path from 'path'
import { promises as fs, existsSync } from 'fs'
import { fetchL4Diagnostics } from './lsp.js'
import {
  awaitDirectiveResults,
  createDirectiveSnapshotStore,
  getCachedDirectiveResults,
  renderDirectiveResults,
} from './directive-snapshot.js'

/**
 * Per-tool directive snapshot store for `fs__edit_file`. Keeping this
 * separate from `l4__evaluate`'s store means an edit can report
 * "what changed since the last edit" independently from
 * "what changed since the last l4__evaluate call".
 */
const editStore = createDirectiveSnapshotStore()

/** How long to wait (ms) after applyEdit for the LSP to push fresh
 *  directive results before computing the diff. The LSP normally
 *  finishes a small file in under 100ms; we give it a bit more
 *  headroom but bail rather than block the tool turn. */
const DIRECTIVE_PUSH_WAIT_MS = 1000

/**
 * Built-in filesystem tools. All paths are workspace-relative; absolute
 * paths outside any workspace folder are rejected. Each tool returns a
 * string (tool result). Errors throw — the dispatcher wraps them into
 * a tool-result `{ error: ... }`.
 */

interface ResolvedPath {
  relative: string
  uri: vscode.Uri
  fsPath: string
}

/**
 * Resolve a user-supplied path to a workspace-rooted URI, rejecting
 * anything that escapes the workspace. Accepts both absolute paths
 * (inside the workspace) and relative paths (resolved against the
 * first workspace folder).
 */
function resolveWorkspacePath(p: string): ResolvedPath {
  if (!p || typeof p !== 'string') {
    throw new Error('path is required')
  }
  const folders = vscode.workspace.workspaceFolders ?? []
  if (folders.length === 0) {
    throw new Error(
      'No workspace folder is open. fs tools only operate on files inside a loaded workspace folder.'
    )
  }
  // Candidate roots for relative paths. Active file's folder wins the
  // tiebreak so a tool call from inside a multi-root workspace lands
  // next to the file the user is looking at.
  const active = vscode.window.activeTextEditor?.document.uri
  const activeFolder = active
    ? vscode.workspace.getWorkspaceFolder(active)
    : undefined
  const roots: string[] = []
  if (activeFolder) roots.push(activeFolder.uri.fsPath)
  for (const f of folders) {
    if (!roots.includes(f.uri.fsPath)) roots.push(f.uri.fsPath)
  }
  const preferredBase = roots[0]!

  let absolute: string
  if (path.isAbsolute(p)) {
    absolute = p
  } else {
    // Try each root; first hit wins. If none exist, fall through to
    // the preferred base so create can land in a sensible location.
    let picked: string | null = null
    for (const root of roots) {
      const candidate = path.resolve(root, p)
      if (existsSync(candidate)) {
        picked = candidate
        break
      }
    }
    absolute = picked ?? path.resolve(preferredBase, p)
  }
  const insideWorkspace = folders.some(
    (f) =>
      absolute === f.uri.fsPath || absolute.startsWith(f.uri.fsPath + path.sep)
  )
  if (!insideWorkspace) {
    throw new Error(
      `Path is outside every loaded workspace folder: ${p}. fs tools only operate on files inside a loaded workspace folder — ask the user to add the target folder to the workspace first.`
    )
  }
  return {
    relative: path.relative(preferredBase, absolute) || path.basename(absolute),
    uri: vscode.Uri.file(absolute),
    fsPath: absolute,
  }
}

export interface FsReadArgs {
  path: string
  /** 1-based inclusive start line in the target's text output.
   *  For files this is a source-line number; for directories it's
   *  the position in the sorted entry listing (1 = first entry).
   *  Default 1. Applied after `search_keywords` when both are set. */
  startLine?: number
  /** 1-based inclusive end line. Default: read to the end of the
   *  output, capped at `startLine + 499` (500-line ceiling). */
  endLine?: number
  /** Optional keyword filter. One or more keywords separated by
   *  whitespace; a line is a hit if it matches ANY of them (OR).
   *  Each keyword is parsed as a case-insensitive regex, falling
   *  back to a literal substring when the regex doesn't compile.
   *  For files, matching lines are returned with 2 lines of
   *  surrounding context. For directories, the tree is walked
   *  recursively (skipping `.git`, `node_modules`, `.DS_Store`)
   *  and file *contents* are grepped — results are emitted as
   *  `<relative-path>:<lineno>: <text>` so the model can jump to
   *  the file with a follow-up `fs__read_file`. Use this to find
   *  a symbol or string anywhere under the workspace. */
  search_keywords?: string
}

/** Hard caps per response. Picked so a single call surfaces enough
 *  to read a typical function or section without letting a 30 kLOC
 *  file blow up the agent's context window. The model pages via
 *  `startLine`/`endLine` when a target is bigger. Applied uniformly
 *  to files and directories. */
const LINE_LIMIT = 500
const CHAR_LIMIT = 4000
const PATTERN_CONTEXT_LINES = 2

/**
 * For `.l4` files, append the exact same diagnostics payload the
 * `l4__evaluate` tool surfaces when the file fails type-check. Runs on
 * every `fs__read_file` / `fs__create_file` / `fs__edit_file` so the
 * model doesn't need a separate round-trip to confirm the file
 * compiles. Failures are swallowed — the primary tool result is more
 * important than the diagnostic annotation.
 */
async function appendL4Diagnostics(
  r: ResolvedPath,
  body: string
): Promise<string> {
  if (!r.fsPath.toLowerCase().endsWith('.l4')) return body
  try {
    const diagnostics = await fetchL4Diagnostics(r.uri)
    // Errors gate everything: the model needs to fix them before any
    // directive-level diff is meaningful. Return the diagnostics block
    // verbatim — same payload `l4__evaluate` surfaces in that case.
    if (/\b\d+ errors?\b/.test(diagnostics)) {
      return `${body}\n${diagnostics}`
    }
    // Clean compile: emit a compact directive diff against the
    // edit-tool's snapshot. A fully-clean call with zero changes
    // collapses to a single header line.
    const diffBlock = await computeEditDiff(r)
    return `${body}\n${diffBlock}`
  } catch {
    return body
  }
}

/** Build the directive-diff tail attached to a successful edit. Uses
 *  the edit-tool's private snapshot store (NOT l4__evaluate's) so the
 *  two tools' "what changed since I last spoke" views stay
 *  independent. Always renders in `changed` mode — the edit's purpose
 *  is "did this write move any directive values?", which the full
 *  catalog would drown. Waits briefly for the LSP to push fresh
 *  directive results post-edit; falls back to whatever's in the cache
 *  on timeout. */
async function computeEditDiff(r: ResolvedPath): Promise<string> {
  const uriStr = r.uri.toString()
  // After applyEdit + save the LSP recompiles and pushes a fresh
  // result set. We always wait for the next push (even if a stale
  // cache entry exists) so the diff reflects post-edit state. The
  // timeout caps the worst-case tool latency.
  await awaitDirectiveResults(uriStr, DIRECTIVE_PUSH_WAIT_MS)
  const results = getCachedDirectiveResults(uriStr) ?? []
  return renderDirectiveResults({
    results,
    store: editStore,
    uri: uriStr,
    mode: 'changed',
  })
}

/** Skip these at any depth to keep listings useful — they're almost
 *  always noise the model doesn't want to reason about. */
const DIR_IGNORES = new Set(['.git', 'node_modules', '.DS_Store'])

/**
 * Read a file (one source line per output line) or list a directory
 * (one entry per output line, `name/` for subdirs, `name` for files,
 * dirs sorted first). Same shape for both so the model learns one
 * rule. Pagination via `startLine`/`endLine`, search via
 * `search_keywords`.
 *
 * Response: a compact header line describing what came back, then
 * the selected lines. Header examples:
 *   [<path> 1-40/40]                                   — full file/dir
 *   [<path> 1-500/2489]                                — paginated (more after line 500)
 *   [<path> keywords="..." matches=3 chunks=2/2]       — file grep
 *   [<path> keywords="..." matches=0]                  — no hits
 *   [<dir>/ keywords="..." matches=12 files=3]         — recursive dir grep
 *
 * For file grep, matching lines are prefixed `>>>` and context
 * lines `   ` so the model can distinguish hits from surroundings;
 * non-contiguous chunks are separated by `---`. For directory grep
 * the tree is walked recursively (skipping `.git`, `node_modules`,
 * `.DS_Store`) and matches are emitted as
 * `<relative-path>:<lineno>: <text>` rows — file contents, not just
 * entry names.
 */
export async function fsReadFile(args: FsReadArgs): Promise<string> {
  const r = resolveWorkspacePath(args.path)
  let stat: Awaited<ReturnType<typeof fs.stat>>
  try {
    stat = await fs.stat(r.fsPath)
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === 'ENOENT') {
      throw new Error(`No such file or directory: ${r.relative}`)
    }
    throw err
  }

  const isDir = stat.isDirectory()
  const hasKeywords =
    typeof args.search_keywords === 'string' &&
    args.search_keywords.trim().length > 0

  // Directory + keywords → recursive content grep across the whole
  // subtree. Different code path because we never want to materialise
  // the entire subtree as a flat `lines[]` (would defeat the cap and
  // burn memory on a big repo) — `grepDirRecursive` walks
  // file-by-file and stops once the cap is hit.
  if (isDir && hasKeywords) {
    return grepDirRecursive(r, args.search_keywords!)
  }

  // Materialise the target as a list of lines regardless of kind.
  // Files: one source line per entry. Directories: one entry per
  // entry, with a trailing `/` for subdirs. Diagnostics are NOT
  // auto-appended for .l4 reads any more — earlier versions did
  // this and trained the model to over-read; use l4__evaluate
  // explicitly.
  let lines: string[]
  if (isDir) {
    const raw = await fs.readdir(r.fsPath, { withFileTypes: true })
    const filtered = raw.filter((e) => !DIR_IGNORES.has(e.name))
    filtered.sort((a, b) => {
      const rank = (e: typeof a): number => (e.isDirectory() ? 0 : 1)
      const d = rank(a) - rank(b)
      return d !== 0 ? d : a.name.localeCompare(b.name)
    })
    lines = filtered.map((e) => (e.isDirectory() ? `${e.name}/` : e.name))
  } else {
    const buf = await fs.readFile(r.fsPath, 'utf-8')
    lines = buf.replace(/\r\n/g, '\n').split('\n')
  }

  // Keywords (grep mode) take precedence — once keywords are set
  // the output is keyword-focused and startLine/endLine narrow
  // WITHIN that focus.
  if (hasKeywords) {
    return grepLines(
      r,
      lines,
      args.search_keywords!,
      isDir,
      args.startLine,
      args.endLine
    )
  }
  return sliceLines(r, lines, args.startLine, args.endLine)
}

/**
 * Cap a line array to LINE_LIMIT / CHAR_LIMIT, optionally starting
 * at a specific 1-based line, and render with a compact header.
 * Used for the no-pattern path of fsReadFile (files AND directories).
 */
function sliceLines(
  r: ResolvedPath,
  lines: string[],
  startLineRaw: number | undefined,
  endLineRaw: number | undefined
): string {
  const total = lines.length
  const startLine = clampInt(startLineRaw ?? 1, 1, Math.max(1, total))
  const requestedEnd =
    endLineRaw !== undefined ? clampInt(endLineRaw, startLine, total) : total
  // Hard line cap even if the caller asks for more — prevents an
  // endLine: 99999 from smuggling the whole target back.
  const capByLines = Math.min(requestedEnd, startLine + LINE_LIMIT - 1, total)
  let endLine = capByLines
  let body = lines.slice(startLine - 1, endLine).join('\n')
  while (body.length > CHAR_LIMIT && endLine > startLine) {
    endLine--
    body = lines.slice(startLine - 1, endLine).join('\n')
  }
  // Edge case: even a single line exceeds the char cap. Clip
  // mid-line; the model can retry with a narrower window if needed.
  if (body.length > CHAR_LIMIT) {
    body = body.slice(0, CHAR_LIMIT)
  }
  // Header is uniform: `[<path> <start>-<end>/<total>]`. The model
  // can compute the next call's `startLine` from `<end>` directly, so
  // a separate "next startLine=…" hint is wasted tokens.
  return `[${r.relative} ${startLine}-${endLine}/${total}]\n${body}`
}

/**
 * Build a line matcher from a whitespace-separated keyword string.
 * Splits on runs of whitespace, compiles each keyword as a
 * case-insensitive regex (literal-substring fallback when it doesn't
 * compile), and returns a predicate that's true when a line matches
 * ANY keyword (OR semantics). An empty/blank string matches nothing.
 */
function buildKeywordMatcher(raw: string): (line: string) => boolean {
  const keywords = raw.trim().split(/\s+/).filter(Boolean)
  if (keywords.length === 0) return () => false
  const tests = keywords.map((kw): ((line: string) => boolean) => {
    try {
      const re = new RegExp(kw, 'i')
      return (line) => re.test(line)
    } catch {
      const needle = kw.toLowerCase()
      return (line) => line.toLowerCase().includes(needle)
    }
  })
  return (line) => tests.some((t) => t(line))
}

/**
 * Grep mode: filter `lines` down to those matching `keywords`,
 * then apply the same caps as `sliceLines`. For files, each match
 * carries PATTERN_CONTEXT_LINES of surrounding context (overlapping
 * chunks merged); for directories, matching entries are returned
 * verbatim (context is meaningless for a listing).
 *
 * When `startLine`/`endLine` are also set, they further narrow the
 * keyword match window to file lines inside that range — lets
 * the model search within a specific section.
 *
 * Output:
 *   [<path> keywords="…" matches=N chunks=K/total]
 *   >>> 42: matching line
 *       43: context
 *   ---
 *       86: context
 *   >>> 87: another match
 */
function grepLines(
  r: ResolvedPath,
  lines: string[],
  keywordsRaw: string,
  isDir: boolean,
  startLineRaw: number | undefined,
  endLineRaw: number | undefined
): string {
  const total = lines.length
  const rangeStart = clampInt(startLineRaw ?? 1, 1, Math.max(1, total)) - 1
  const rangeEnd =
    endLineRaw !== undefined
      ? clampInt(endLineRaw, rangeStart + 1, total)
      : total

  const matcher = buildKeywordMatcher(keywordsRaw)

  const matchIdx: number[] = []
  for (let i = rangeStart; i < rangeEnd; i++) {
    if (matcher(lines[i]!)) matchIdx.push(i)
  }
  const keywordsLabel = JSON.stringify(keywordsRaw)
  if (matchIdx.length === 0) {
    return `[${r.relative} keywords=${keywordsLabel} matches=0]`
  }

  // Directories have no "context" — just return the matching entries
  // as lines, applying the char/line caps.
  if (isDir) {
    const matchedLines = matchIdx.map((i) => `${i + 1}: ${lines[i]!}`)
    let body = matchedLines.slice(0, LINE_LIMIT).join('\n')
    while (body.length > CHAR_LIMIT && body.includes('\n')) {
      body = body.slice(0, body.lastIndexOf('\n'))
    }
    const shown = body ? body.split('\n').length : 0
    const truncated = shown < matchIdx.length
    const header =
      `[${r.relative} keywords=${keywordsLabel} matches=${matchIdx.length}` +
      (truncated ? `, shown=${shown}/${matchIdx.length}]` : `]`)
    return `${header}\n${body}`
  }

  // Files: expand each match to a [start, end] context window and
  // merge overlapping / adjacent windows so consecutive near-hits
  // produce one chunk instead of N.
  type Chunk = { start: number; end: number; hits: Set<number> }
  const chunks: Chunk[] = []
  for (const i of matchIdx) {
    const start = Math.max(0, i - PATTERN_CONTEXT_LINES)
    const end = Math.min(total - 1, i + PATTERN_CONTEXT_LINES)
    const tail = chunks[chunks.length - 1]
    if (tail && start <= tail.end + 1) {
      tail.end = Math.max(tail.end, end)
      tail.hits.add(i)
    } else {
      chunks.push({ start, end, hits: new Set([i]) })
    }
  }

  const rendered: string[] = []
  let renderedLineCount = 0
  let renderedCharCount = 0
  let chunksShown = 0
  let truncated = false
  for (const chunk of chunks) {
    const chunkLines: string[] = []
    for (let i = chunk.start; i <= chunk.end; i++) {
      const prefix = chunk.hits.has(i) ? '>>>' : '   '
      const line = `${prefix} ${i + 1}: ${lines[i]!}`
      const projectedLines = renderedLineCount + chunkLines.length + 1
      const projectedChars =
        renderedCharCount +
        chunkLines.reduce((a, l) => a + l.length + 1, 0) +
        line.length +
        1
      if (projectedLines > LINE_LIMIT || projectedChars > CHAR_LIMIT) {
        truncated = true
        break
      }
      chunkLines.push(line)
    }
    if (chunkLines.length === 0) {
      truncated = true
      break
    }
    rendered.push(chunkLines.join('\n'))
    renderedLineCount += chunkLines.length
    renderedCharCount += chunkLines.reduce((a, l) => a + l.length + 1, 0)
    chunksShown++
    if (truncated) break
  }

  const header =
    `[${r.relative} keywords=${keywordsLabel} matches=${matchIdx.length} ` +
    `chunks=${chunksShown}/${chunks.length}` +
    (truncated ? `, truncated]` : `]`)
  return `${header}\n${rendered.join('\n---\n')}`
}

function clampInt(n: number, lo: number, hi: number): number {
  if (!Number.isFinite(n)) return lo
  const rounded = Math.floor(n)
  if (rounded < lo) return lo
  if (rounded > hi) return hi
  return rounded
}

/**
 * Recursive content grep under a directory. Walks the tree
 * (depth-first, deterministic order), reads every file as utf-8 and
 * emits one row per matching line:
 *
 *   <relative-path>:<lineno>: <text>
 *
 * Skips `DIR_IGNORES` at every level and silently skips files that
 * fail to decode (binary blobs, broken symlinks). No surrounding
 * context — across many files the model wants `path:line` to navigate
 * to with a follow-up `fs__read_file`, not a chunk of code.
 *
 * Caps to LINE_LIMIT rows / CHAR_LIMIT chars, signalled in the header
 * as `shown=X/total`. Stops the walk early once the cap is hit.
 */
async function grepDirRecursive(
  r: ResolvedPath,
  keywordsRaw: string
): Promise<string> {
  const matcher = buildKeywordMatcher(keywordsRaw)

  const rows: string[] = []
  const filesWithMatches = new Set<string>()
  let totalMatches = 0
  let renderedChars = 0
  let truncated = false

  // DFS via an explicit stack so we can break out early once the cap
  // is hit without unwinding a recursion. `relRoot` is the path
  // displayed in each row — relative to the workspace, not to `r`.
  const stack: string[] = [r.fsPath]
  const baseRel = r.relative
  while (stack.length > 0) {
    const dir = stack.pop()!
    let entries: Array<import('fs').Dirent>
    try {
      entries = await fs.readdir(dir, { withFileTypes: true })
    } catch {
      continue
    }
    entries.sort((a, b) => {
      const rank = (e: typeof a): number => (e.isDirectory() ? 0 : 1)
      const d = rank(a) - rank(b)
      return d !== 0 ? d : a.name.localeCompare(b.name)
    })
    // Push directories in reverse so the stack pops them in
    // alphabetical order, matching the per-directory sort above.
    const subdirs: string[] = []
    for (const e of entries) {
      if (DIR_IGNORES.has(e.name)) continue
      const full = path.join(dir, e.name)
      if (e.isDirectory()) {
        subdirs.push(full)
        continue
      }
      if (!e.isFile()) continue
      let buf: string
      try {
        buf = await fs.readFile(full, 'utf-8')
      } catch {
        continue
      }
      const fileRel = path.join(baseRel, path.relative(r.fsPath, full))
      const fileLines = buf.replace(/\r\n/g, '\n').split('\n')
      for (let i = 0; i < fileLines.length; i++) {
        if (!matcher(fileLines[i]!)) continue
        totalMatches++
        filesWithMatches.add(fileRel)
        if (truncated) continue
        const row = `${fileRel}:${i + 1}: ${fileLines[i]!}`
        if (
          rows.length + 1 > LINE_LIMIT ||
          renderedChars + row.length + 1 > CHAR_LIMIT
        ) {
          truncated = true
          continue
        }
        rows.push(row)
        renderedChars += row.length + 1
      }
    }
    for (let i = subdirs.length - 1; i >= 0; i--) stack.push(subdirs[i]!)
  }

  const keywordsLabel = JSON.stringify(keywordsRaw)
  if (totalMatches === 0) {
    return `[${r.relative}/ keywords=${keywordsLabel} matches=0]`
  }
  const header =
    `[${r.relative}/ keywords=${keywordsLabel} matches=${totalMatches} ` +
    `files=${filesWithMatches.size}` +
    (truncated ? `, shown=${rows.length}/${totalMatches}]` : `]`)
  return `${header}\n${rows.join('\n')}`
}

export interface FsCreateArgs {
  path: string
}

/**
 * Sentinel content for newly-created files. Acts as the unique anchor
 * for the first follow-up `fs__edit_file` call — the model replaces
 * this exact line with real content. Required because `fs__edit_file`
 * needs a non-empty `old` snippet to anchor on; without the seed a
 * brand-new (empty) file would have nothing to find.
 */
export const FS_CREATE_FILE_SEED = '// new file in progress ...\n'

/**
 * Placeholder copy dropped into the body of a freshly-created HTML
 * file. Doubles as the unique anchor the first follow-up
 * `fs__edit_file` call replaces with real markup.
 */
export const FS_CREATE_HTML_SEED = `<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>New document</title>
    <style>
      body {
        background-color: white;
        font-family: serif;
        font-size: 1rem;
        color: black;
        margin: 2rem;
      }
    </style>
  </head>
  <body>New document in progress ...</body>
</html>
`

/** True for files we render in the built-in browser preview rather
 *  than opening as a source text tab. */
function isHtmlPath(fsPath: string): boolean {
  const ext = path.extname(fsPath).toLowerCase()
  return ext === '.html' || ext === '.htm'
}

// A single reused webview panel acts as the "built-in browser" for
// AI-created HTML files. Reused across creates so a new document
// replaces the previous preview instead of stacking tabs. The
// save-watcher keeps the rendered page in sync as follow-up
// `fs__edit_file` calls build the document up (each one saves the
// buffer, firing onDidSaveTextDocument).
let htmlPreviewPanel: vscode.WebviewPanel | undefined
let htmlPreviewUri: vscode.Uri | undefined
let htmlPreviewWatcher: vscode.Disposable | undefined

/**
 * Open (or refocus) the built-in browser preview on an HTML file and
 * point it at `uri`. Renders the file's current contents in a webview
 * panel beside the chat, and live-refreshes whenever that file is
 * saved so the user watches the document take shape.
 */
async function openHtmlPreview(uri: vscode.Uri): Promise<void> {
  const title = `Preview: ${path.basename(uri.fsPath)}`
  if (!htmlPreviewPanel) {
    htmlPreviewPanel = vscode.window.createWebviewPanel(
      'l4.htmlPreview',
      title,
      // Beside the chat, without stealing focus from the conversation.
      { viewColumn: vscode.ViewColumn.Active, preserveFocus: true },
      {
        enableScripts: true,
        retainContextWhenHidden: true,
        localResourceRoots:
          vscode.workspace.workspaceFolders?.map((f) => f.uri) ?? [],
      }
    )
    htmlPreviewPanel.onDidDispose(() => {
      htmlPreviewPanel = undefined
      htmlPreviewUri = undefined
      htmlPreviewWatcher?.dispose()
      htmlPreviewWatcher = undefined
    })
    // Manual saves (Cmd+S) also refresh — the edit tool path refreshes
    // explicitly via refreshHtmlPreviewIfShowing, but this keeps the
    // preview honest for any other writer too.
    htmlPreviewWatcher = vscode.workspace.onDidSaveTextDocument((doc) =>
      refreshHtmlPreviewIfShowing(doc.uri)
    )
  }
  htmlPreviewUri = uri
  htmlPreviewPanel.title = title
  htmlPreviewPanel.webview.html = await readFileText(uri)
  htmlPreviewPanel.reveal(vscode.ViewColumn.Active, true)
}

/** Read a workspace file as UTF-8 text. */
async function readFileText(uri: vscode.Uri): Promise<string> {
  const bytes = await vscode.workspace.fs.readFile(uri)
  return Buffer.from(bytes).toString('utf8')
}

/**
 * Reload the built-in browser preview from disk when it is currently
 * showing `uri`. No-op when the preview panel is closed or pointed at a
 * different file — so an edit to an HTML file that happens to have a
 * live preview tab open refreshes it, and an edit to any other file
 * does nothing.
 */
async function refreshHtmlPreviewIfShowing(uri: vscode.Uri): Promise<void> {
  if (
    htmlPreviewPanel &&
    htmlPreviewUri &&
    uri.toString() === htmlPreviewUri.toString()
  ) {
    htmlPreviewPanel.webview.html = await readFileText(uri)
  }
}

/**
 * Create a file seeded with FS_CREATE_FILE_SEED. The model fills it
 * via follow-up `fs__edit_file` calls — the first one replaces the
 * seed line, subsequent ones add sections incrementally. Keeps the
 * create surface narrow and avoids the "model calls create with the
 * whole file inline" pattern that wastes tokens / risks max_tokens
 * truncation mid-payload.
 */
export async function fsCreateFile(args: FsCreateArgs): Promise<string> {
  const r = resolveWorkspacePath(args.path)
  try {
    await fs.access(r.fsPath)
    throw new Error(`File already exists: ${r.relative}`)
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code !== 'ENOENT') {
      // Either the file exists (handled above) or something else — rethrow.
      if ((err as Error).message?.startsWith('File already exists:')) throw err
    }
    // ENOENT → proceed to create.
  }
  // Route through VSCode's WorkspaceEdit so the LSP picks up the new
  // file via its normal didOpen path (otherwise a silent Node write
  // doesn't get didChange/didOpen events, and l4__evaluate can
  // return stale results for a freshly-created file).
  // HTML files get a full (plain-white) document skeleton so the
  // built-in browser preview renders a real page from the start;
  // everything else gets the one-line code seed.
  const isHtml = isHtmlPath(r.fsPath)
  const seed = isHtml ? FS_CREATE_HTML_SEED : FS_CREATE_FILE_SEED
  await fs.mkdir(path.dirname(r.fsPath), { recursive: true })
  const edit = new vscode.WorkspaceEdit()
  edit.createFile(r.uri, { overwrite: false })
  edit.insert(r.uri, new vscode.Position(0, 0), seed)
  const ok = await vscode.workspace.applyEdit(edit)
  if (!ok) {
    throw new Error(
      `fs__create_file: VSCode refused to create ${r.relative} (readonly workspace or similar).`
    )
  }
  // Persist to disk so subsequent non-VSCode readers (including our
  // own fs__read_file / fs.readFile) see the new content immediately.
  const doc = await vscode.workspace.openTextDocument(r.uri)
  if (doc.isDirty) await doc.save()
  if (isHtml) {
    // Open the rendered page in the built-in browser preview rather
    // than a source tab — and live-refresh it as follow-up edits land.
    await openHtmlPreview(r.uri)
  } else {
    // Surface the new file as a visible tab so the user sees what the
    // model just created without having to expand the tool-call row and
    // click. `preserveFocus: true` keeps the cursor wherever the user
    // was — usually the chat input — instead of stealing focus into the
    // editor mid-conversation.
    await vscode.window.showTextDocument(doc, {
      preview: false,
      preserveFocus: true,
    })
  }
  // Skip the appendL4Diagnostics tail — a freshly-created file has no
  // real content to type-check, and the Edit tool the model uses next
  // will run diagnostics naturally.
  const anchor = isHtml
    ? '\n```html\n' + FS_CREATE_HTML_SEED + '\n```'
    : '"' + FS_CREATE_FILE_SEED.trimEnd() + '"'
  return `Created ${r.relative} - seeded with ${anchor}`
}

export interface FsEditArgs {
  path: string
  /** Exact text to replace. When `startLine` is omitted (or 0),
   *  must appear EXACTLY ONCE in the file. When `startLine` is set,
   *  the first occurrence strictly after that line is taken — no
   *  global uniqueness required. */
  old: string
  /** Replacement text. */
  new: string
  /** Optional 1-based line number to anchor the search. When set,
   *  the edit targets the first occurrence of `old` that starts on
   *  a line STRICTLY AFTER `startLine` (so earlier identical
   *  snippets are skipped). Default 0 = search the whole file.
   *  Leave generous slack (~5-10 lines) below the expected line —
   *  multi-round turns shift line numbers as prior edits land. */
  startLine?: number
}

/**
 * String-anchored find/replace. Fails loudly when `old` is missing or
 * ambiguous. Line numbers would be brittle across multi-round turns;
 * unique text anchors fail fast. Matches the pattern used by Cursor,
 * Claude Code, Aider.
 *
 * Routed through `workspace.applyEdit` so the L4 language server sees
 * the change via didChange (keeps l4__evaluate fresh) and the
 * edit joins the VSCode undo stack. We save the buffer afterwards so
 * disk-based readers (`fs__read_file`, other extensions) see the new
 * content immediately.
 */
export async function fsEditFile(args: FsEditArgs): Promise<string> {
  const r = resolveWorkspacePath(args.path)
  if (typeof args.old !== 'string') {
    throw new Error(
      "fs__edit_file: 'old' must be a non-empty string identifying the snippet to replace."
    )
  }
  // Whole-file replace is intentionally not supported. A single
  // tool_use carrying the entire file body is the classic max_tokens
  // truncation trap — Anthropic cuts the args JSON mid-string,
  // toolCalls comes back empty, the edit silently never lands. For a
  // freshly-created file: anchor the first edit on the seed line
  // (`FS_CREATE_FILE_SEED`); for an existing file: replace surgical
  // sections in multiple smaller fs__edit_file calls.
  if (args.old.length === 0) {
    throw new Error(
      `fs__edit_file: 'old' must be a non-empty anchor snippet. Whole-file replacement is not supported — for a freshly-created file replace the seed line "${FS_CREATE_FILE_SEED.trimEnd()}" first, then build up content via additional fs__edit_file calls. For large rewrites, split into multiple smaller edits.`
    )
  }
  // Same defensive check we use in fs__create_file: fail fast when
  // the model omitted `new` before any LSP edit machinery spins up.
  // Empty-string replacements are valid (it's how we delete a
  // snippet), so the typeof check allows "" but rejects undefined.
  if (typeof args.new !== 'string') {
    throw new Error(
      "fs__edit_file: 'new' is required (as a string). Pass an empty string to delete the 'old' snippet."
    )
  }
  // Edit MUST NOT auto-create. If the path doesn't resolve to an
  // existing file, fail loudly with a message that points the model
  // at fs__create_file. openTextDocument can otherwise be coerced
  // (e.g. on `untitled:` schemes) into producing an in-memory buffer
  // that then races with the model's expectations.
  try {
    await fs.access(r.fsPath)
  } catch {
    throw new Error(
      `fs__edit_file: ${r.relative} does not exist. Use fs__create_file to create it first, then call fs__edit_file to add content.`
    )
  }
  const doc = await vscode.workspace.openTextDocument(r.uri)
  // Preserve the file's line-ending style across the edit. The model
  // always sends LF in `args.old` / `args.new`; if the document is
  // CRLF we normalize both to CRLF so (a) byte offsets computed from
  // the search match the live document text (positionAt walks the
  // raw buffer including \r), and (b) the inserted snippet stays
  // uniformly CRLF instead of dropping LF runs into a CRLF file.
  // Without this, a single edit to a CRLF file silently flattened
  // the whole file to LF — the LF-only `args.new` was spliced in
  // as-is and applyEdit + save preserved the result wholesale.
  // `replace(/\r?\n/g, …)` accepts whichever form the model sent.
  const docEol = doc.eol === vscode.EndOfLine.CRLF ? '\r\n' : '\n'
  const oldNormalized = args.old.replace(/\r?\n/g, docEol)
  const newNormalized = args.new.replace(/\r?\n/g, docEol)
  // Search the raw document text (no LF normalization) so offsets
  // line up 1:1 with what positionAt expects below.
  const currentText = doc.getText()

  // When `startLine` is set, drop the lines at/before it from the
  // search window. The replace still applies to the real document
  // so we track the absolute offset that corresponds to the
  // window's start. Without startLine, the window is the whole
  // file and the `old` snippet must be unique in it; with startLine
  // set we just take the first match in the window (earlier
  // identical snippets are intentionally skipped).
  const anchorLine =
    typeof args.startLine === 'number' && Number.isFinite(args.startLine)
      ? Math.max(0, Math.floor(args.startLine))
      : 0
  let searchOffset = 0
  if (anchorLine > 0) {
    const allLines = currentText.split(docEol)
    if (anchorLine >= allLines.length) {
      throw new Error(
        `fs__edit_file: startLine=${args.startLine} is past the end of ${r.relative} (${allLines.length} lines).`
      )
    }
    // Offset of the line AFTER the anchor — `anchorLine` lines plus
    // that many line terminators (\n or \r\n depending on the file).
    searchOffset =
      allLines.slice(0, anchorLine).reduce((n, l) => n + l.length, 0) +
      anchorLine * docEol.length
  }
  const searchWindow = currentText.slice(searchOffset)

  if (anchorLine === 0) {
    const occurrences = countOccurrences(searchWindow, oldNormalized)
    if (occurrences === 0) {
      throw new Error(
        `fs__edit_file: the 'old' snippet was not found in ${r.relative}`
      )
    }
    if (occurrences > 1) {
      throw new Error(
        `fs__edit_file: the 'old' snippet appears ${occurrences} times in ${r.relative}. Include more surrounding lines to make it unique, or pass startLine=<line> to anchor.`
      )
    }
  }

  // Compute the Range to replace. Using the live document's positionAt
  // keeps the offset-to-(line,col) mapping authoritative — works the
  // same way the LSP sees text.
  const hit = searchWindow.indexOf(oldNormalized)
  if (hit === -1) {
    throw new Error(
      anchorLine === 0
        ? `fs__edit_file: 'old' snippet not found in ${r.relative}`
        : `fs__edit_file: 'old' snippet not found in ${r.relative} after line ${anchorLine}`
    )
  }
  const startOffset = searchOffset + hit
  const endOffset = startOffset + oldNormalized.length
  const range = new vscode.Range(
    doc.positionAt(startOffset),
    doc.positionAt(endOffset)
  )
  const edit = new vscode.WorkspaceEdit()
  edit.replace(r.uri, range, newNormalized)
  const ok = await vscode.workspace.applyEdit(edit)
  if (!ok) {
    throw new Error(
      `fs__edit_file: VSCode refused to apply edit to ${r.relative} (file may be read-only).`
    )
  }
  if (doc.isDirty) await doc.save()
  // If this file is an HTML doc currently shown in the built-in browser
  // preview, reload the rendered page so it tracks the edit.
  if (isHtmlPath(r.fsPath)) await refreshHtmlPreviewIfShowing(r.uri)
  // `[<path> <start>-<end>]` prefix carries the post-edit line range
  // so the chat row can render it as a muted "Lines 23-45" suffix
  // (matches fs__read_file's surfacing). No `/total` segment here —
  // total file-line count isn't useful for an edit row, and its
  // absence tells the webview parser this is an edit-anchor range
  // (always shown) rather than a read range (suppressed when it
  // covers the whole file).
  const editStartLine = range.start.line + 1
  const editEndLine = editStartLine + args.new.split('\n').length - 1
  return withDocsHintOnError(
    await appendL4Diagnostics(
      r,
      `[${r.relative} ${editStartLine}-${editEndLine}] (Edited ${args.old.split('\n').length} → ${args.new.split('\n').length} lines)`
    )
  )
}

/**
 * Append a `search_l4_docs` hint when the diagnostics block reports at
 * least one error. Detected by the header that
 * `fetchL4Diagnostics` produces — the count summary contains
 * "N error" / "N errors" only when the error count is non-zero.
 *
 * Scoped to fs__edit_file: we only nudge the model toward
 * doc-search after a code edit landed, not on every read or create.
 */
function withDocsHintOnError(text: string): string {
  if (!/\b\d+ errors?\b/.test(text)) return text
  return `${text}\n\nUse \`search_l4_docs\` tool to learn L4 syntax and keyword use`
}

export interface FsDeleteArgs {
  path: string
}

export async function fsDeleteFile(args: FsDeleteArgs): Promise<string> {
  const r = resolveWorkspacePath(args.path)
  // Use VSCode's FS so the Trash is honored (user can recover).
  await vscode.workspace.fs.delete(r.uri, {
    recursive: false,
    useTrash: true,
  })
  return `Moved ${r.relative} to trash`
}

function countOccurrences(haystack: string, needle: string): number {
  if (!needle) return 0
  let count = 0
  let idx = 0
  while ((idx = haystack.indexOf(needle, idx)) !== -1) {
    count++
    idx += needle.length
  }
  return count
}

/**
 * Resolve a tool-call's `path` argument to a workspace-rooted URI for
 * the register layer to open in the editor. Used by the AiFileOpen
 * RPC handler.
 */
export function resolveFileUri(p: string): vscode.Uri | null {
  try {
    return resolveWorkspacePath(p).uri
  } catch {
    return null
  }
}

/**
 * Snapshot the current on-disk contents of a file referenced by a
 * tool-call argument. Returns `''` if the file does not exist yet.
 * The tool dispatcher calls this BEFORE running fs__create / fs__edit
 * so the "before" side of the applied-diff view has something to show.
 */
export async function resolveCurrentContents(p: string): Promise<string> {
  const r = resolveWorkspacePath(p)
  try {
    return (await fs.readFile(r.fsPath, 'utf-8')).replace(/\r\n/g, '\n')
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === 'ENOENT') return ''
    throw err
  }
}

/** Workspace-relative display path for a resolved uri. */
export function workspaceRelative(uri: vscode.Uri): string {
  const folders = vscode.workspace.workspaceFolders ?? []
  for (const f of folders) {
    if (uri.fsPath === f.uri.fsPath) return path.basename(uri.fsPath)
    if (uri.fsPath.startsWith(f.uri.fsPath + path.sep)) {
      return path.relative(f.uri.fsPath, uri.fsPath)
    }
  }
  return uri.fsPath
}
