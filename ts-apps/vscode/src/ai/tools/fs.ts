import * as vscode from 'vscode'
import * as path from 'path'
import { promises as fs, existsSync } from 'fs'
import { fetchL4Diagnostics } from './lsp.js'

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
   *  Default 1. Applied after `pattern` when both are set. */
  startLine?: number
  /** 1-based inclusive end line. Default: read to the end of the
   *  output, capped at `startLine + 99` (100-line ceiling). */
  endLine?: number
  /** Optional keyword filter. Narrows the output to lines
   *  matching this pattern — parsed as a case-insensitive regex,
   *  falls back to literal substring when the regex doesn't
   *  compile. For files, matching lines are returned with 2 lines
   *  of surrounding context; for directories, matching entries
   *  only. Use this to jump to a named section or symbol without
   *  paging through preceding lines. */
  pattern?: string
}

/** Hard caps per response. Picked so a single call surfaces enough
 *  to read a typical function or section without letting a 30 kLOC
 *  file blow up the agent's context window. The model pages via
 *  `startLine`/`endLine` when a target is bigger. Applied uniformly
 *  to files and directories. */
const LINE_LIMIT = 100
const CHAR_LIMIT = 4000
const PATTERN_CONTEXT_LINES = 2

/**
 * For `.l4` files, append the exact same diagnostics payload the
 * `lsp__diagnostics` tool would produce for this path. Runs on every
 * `fs__read_file` / `fs__create_file` / `fs__edit_file` so the model
 * doesn't need a separate round-trip to confirm the file compiles.
 * Shape stays consistent with `lsp__diagnostics` so agent prompts can
 * parse one JSON format regardless of which tool surfaced it. Failures
 * are swallowed — the primary tool result is more important than the
 * diagnostic annotation.
 */
async function appendL4Diagnostics(
  r: ResolvedPath,
  body: string
): Promise<string> {
  if (!r.fsPath.toLowerCase().endsWith('.l4')) return body
  try {
    const diagnostics = await fetchL4Diagnostics(r.uri)
    return `${body}\n\n${diagnostics}`
  } catch {
    return body
  }
}

/** Skip these at any depth to keep listings useful — they're almost
 *  always noise the model doesn't want to reason about. */
const DIR_IGNORES = new Set(['.git', 'node_modules', '.DS_Store'])

/**
 * Read a file (one source line per output line) or list a directory
 * (one entry per output line, `name/` for subdirs, `name` for files,
 * dirs sorted first). Same shape for both so the model learns one
 * rule. Pagination via `startLine`/`endLine`, search via `pattern`.
 *
 * Response: a compact header line describing what came back, then
 * the selected lines. Header examples:
 *   [<path> 1-40/40]                               — full file/dir
 *   [<path> 1-100/489, next startLine=101]         — paginated
 *   [<path> pattern="..." matches=3 chunks=2/2]    — grep result
 *   [<path> pattern="..." matches=0]               — no hits
 *
 * For grep mode, matching lines are prefixed `>>>` and context
 * lines `   ` so the model can distinguish hits from surroundings;
 * non-contiguous chunks are separated by `---`.
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

  // Materialise the target as a list of lines regardless of kind.
  // Files: one source line per entry. Directories: one entry per
  // entry, with a trailing `/` for subdirs. Diagnostics are NOT
  // auto-appended for .l4 reads any more — earlier versions did
  // this and trained the model to over-read; use lsp__diagnostics
  // explicitly.
  let lines: string[]
  const isDir = stat.isDirectory()
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

  // Pattern (grep mode) takes precedence — once a pattern is set
  // the output is keyword-focused and startLine/endLine narrow
  // WITHIN that focus.
  if (typeof args.pattern === 'string' && args.pattern.length > 0) {
    return grepLines(
      r,
      lines,
      args.pattern,
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
  // mid-line; the header's next-startLine stays at the same line
  // so the model can retry with a narrower window.
  let lineClipped = false
  if (body.length > CHAR_LIMIT) {
    body = body.slice(0, CHAR_LIMIT)
    lineClipped = true
  }
  const fitsInOneCall = startLine === 1 && endLine === total && !lineClipped
  if (fitsInOneCall) {
    return `[${r.relative} 1-${total}/${total}]\n${body}`
  }
  const nextStartLine = lineClipped ? startLine : endLine + 1
  const hasMore = endLine < total || lineClipped
  const header = hasMore
    ? `[${r.relative} ${startLine}-${endLine}/${total}, next startLine=${nextStartLine}]`
    : `[${r.relative} ${startLine}-${endLine}/${total}]`
  return `${header}\n${body}`
}

/**
 * Grep mode: filter `lines` down to those matching `pattern`,
 * then apply the same caps as `sliceLines`. For files, each match
 * carries PATTERN_CONTEXT_LINES of surrounding context (overlapping
 * chunks merged); for directories, matching entries are returned
 * verbatim (context is meaningless for a listing).
 *
 * When `startLine`/`endLine` are also set, they further narrow the
 * pattern's match window to file lines inside that range — lets
 * the model search within a specific section.
 *
 * Output:
 *   [<path> pattern="…" matches=N chunks=K/total]
 *   >>> 42: matching line
 *       43: context
 *   ---
 *       86: context
 *   >>> 87: another match
 */
function grepLines(
  r: ResolvedPath,
  lines: string[],
  patternRaw: string,
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

  let matcher: (line: string) => boolean
  try {
    const re = new RegExp(patternRaw, 'i')
    matcher = (line) => re.test(line)
  } catch {
    const needle = patternRaw.toLowerCase()
    matcher = (line) => line.toLowerCase().includes(needle)
  }

  const matchIdx: number[] = []
  for (let i = rangeStart; i < rangeEnd; i++) {
    if (matcher(lines[i]!)) matchIdx.push(i)
  }
  const patternLabel = JSON.stringify(patternRaw)
  if (matchIdx.length === 0) {
    return `[${r.relative} pattern=${patternLabel} matches=0]`
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
      `[${r.relative} pattern=${patternLabel} matches=${matchIdx.length}` +
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
    `[${r.relative} pattern=${patternLabel} matches=${matchIdx.length} ` +
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

export interface FsCreateArgs {
  path: string
}

/**
 * Sentinel content for newly-created files. The model fills the file
 * via follow-up `fs__edit_file` calls — anchoring on this exact line
 * guarantees a unique target, which avoids the "whole-file replace"
 * fallback path that's harder for the model to reason about.
 */
export const FS_CREATE_FILE_SEED = '// new file content\n'

/**
 * Create a file seeded with FS_CREATE_FILE_SEED. The model fills it
 * via follow-up `fs__edit_file` calls (anchored on the seed line, or
 * via `old: ""` for a whole-file rewrite). Keeps the create surface
 * narrow and avoids the "model calls create with the whole file
 * inline" pattern that wastes tokens.
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
  // doesn't get didChange/didOpen events, and lsp__diagnostics can
  // return stale results for a freshly-created file).
  await fs.mkdir(path.dirname(r.fsPath), { recursive: true })
  const edit = new vscode.WorkspaceEdit()
  edit.createFile(r.uri, { overwrite: false })
  edit.insert(r.uri, new vscode.Position(0, 0), FS_CREATE_FILE_SEED)
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
  // Skip the appendL4Diagnostics tail — a freshly-created file has no
  // real content to type-check, and the Edit tool the model uses next
  // will run diagnostics naturally.
  return `Created ${r.relative} (seeded with "${FS_CREATE_FILE_SEED.trimEnd()}").`
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
 * the change via didChange (keeps lsp__diagnostics fresh) and the
 * edit joins the VSCode undo stack. We save the buffer afterwards so
 * disk-based readers (`fs__read_file`, other extensions) see the new
 * content immediately.
 */
export async function fsEditFile(args: FsEditArgs): Promise<string> {
  const r = resolveWorkspacePath(args.path)
  if (typeof args.old !== 'string') {
    throw new Error(
      "fs__edit_file: 'old' must be a string. Pass '' to replace the entire file (typical right after fs__create_file)."
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
  // `old === ""` is the explicit "fill / overwrite" verb. Replace the
  // entire current file with `new`. Pairs naturally with
  // fs__create_file, which seeds an empty file the model then fills
  // with one whole-file edit. Implemented as a real replace (not
  // applyEdit replace over a 0-length range, which would APPEND), so
  // an existing non-empty file is genuinely overwritten — that's the
  // contract callers need so a "rewrite this file end-to-end" call is
  // possible without two hops.
  if (args.old.length === 0) {
    const fullRange = new vscode.Range(
      new vscode.Position(0, 0),
      doc.positionAt(doc.getText().length)
    )
    const edit = new vscode.WorkspaceEdit()
    edit.replace(r.uri, fullRange, args.new)
    const ok = await vscode.workspace.applyEdit(edit)
    if (!ok) {
      throw new Error(
        `fs__edit_file: VSCode refused to apply the edit to ${r.relative} (file may be read-only).`
      )
    }
    if (doc.isDirty) await doc.save()
    return withDocsHintOnError(
      await appendL4Diagnostics(
        r,
        `Wrote ${r.relative} (${args.new.split('\n').length} lines)`
      )
    )
  }
  const currentText = doc.getText().replace(/\r\n/g, '\n')

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
    const allLines = currentText.split('\n')
    if (anchorLine >= allLines.length) {
      throw new Error(
        `fs__edit_file: startLine=${args.startLine} is past the end of ${r.relative} (${allLines.length} lines).`
      )
    }
    // Offset of the line AFTER the anchor — `anchorLine` lines plus
    // that many newlines.
    searchOffset =
      allLines.slice(0, anchorLine).reduce((n, l) => n + l.length, 0) +
      anchorLine
  }
  const searchWindow = currentText.slice(searchOffset)

  if (anchorLine === 0) {
    const occurrences = countOccurrences(searchWindow, args.old)
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
  const hit = searchWindow.indexOf(args.old)
  if (hit === -1) {
    throw new Error(
      anchorLine === 0
        ? `fs__edit_file: the 'old' snippet was not found in ${r.relative}`
        : `fs__edit_file: the 'old' snippet was not found in ${r.relative} after line ${anchorLine}`
    )
  }
  const startOffset = searchOffset + hit
  const endOffset = startOffset + args.old.length
  const range = new vscode.Range(
    doc.positionAt(startOffset),
    doc.positionAt(endOffset)
  )
  const edit = new vscode.WorkspaceEdit()
  edit.replace(r.uri, range, args.new)
  const ok = await vscode.workspace.applyEdit(edit)
  if (!ok) {
    throw new Error(
      `fs__edit_file: VSCode refused to apply the edit to ${r.relative} (file may be read-only).`
    )
  }
  if (doc.isDirty) await doc.save()
  return withDocsHintOnError(
    await appendL4Diagnostics(
      r,
      `Edited ${r.relative} (${args.old.split('\n').length} → ${args.new.split('\n').length} lines changed)`
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
  return `${text}\n\nUse \`search_l4_docs\` tool to learn more about L4 syntax and keyword use.`
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
