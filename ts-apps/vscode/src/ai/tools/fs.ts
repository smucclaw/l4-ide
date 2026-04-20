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
  /** For directory paths: offset into the sorted entries list. Use the
   *  `nextFrom` value from a previous page to walk larger folders. */
  from?: number
}

/** Hard cap per directory-listing response. A single page covers most
 *  real workspaces; more than this shunts into `nextFrom` paging. */
const DIR_PAGE_LIMIT = 100

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
  if (stat.isDirectory()) {
    return await fsListDirectory(r, args.from ?? 0)
  }
  const buf = await fs.readFile(r.fsPath, 'utf-8')
  // Newline-normalize so downstream tools that rely on \n splits don't
  // get confused by Windows CRLF.
  const body = buf.replace(/\r\n/g, '\n')
  return appendL4Diagnostics(r, body)
}

async function fsListDirectory(
  r: ResolvedPath,
  fromRaw: number
): Promise<string> {
  const raw = await fs.readdir(r.fsPath, { withFileTypes: true })
  const filtered = raw.filter((e) => !DIR_IGNORES.has(e.name))
  // Directories first, then files, alphabetical within each group.
  filtered.sort((a, b) => {
    const rank = (e: typeof a): number => (e.isDirectory() ? 0 : 1)
    const d = rank(a) - rank(b)
    return d !== 0 ? d : a.name.localeCompare(b.name)
  })
  const total = filtered.length
  const from = Math.max(0, Math.min(Math.floor(fromRaw), total))
  const slice = filtered.slice(from, from + DIR_PAGE_LIMIT)
  const entries = slice.map((e) => ({
    name: e.name,
    kind: e.isDirectory() ? ('directory' as const) : ('file' as const),
  }))
  const nextFrom = from + entries.length
  const hasMore = nextFrom < total
  return JSON.stringify(
    {
      kind: 'directory',
      path: r.relative || '.',
      total,
      from,
      count: entries.length,
      entries,
      hasMore,
      nextFrom: hasMore ? nextFrom : null,
    },
    null,
    2
  )
}

export interface FsCreateArgs {
  path: string
  content: string
}

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
  edit.insert(r.uri, new vscode.Position(0, 0), args.content)
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
  return appendL4Diagnostics(
    r,
    `Created ${r.relative} (${args.content.length} chars)`
  )
}

export interface FsEditArgs {
  path: string
  /** Exact text to replace; must appear EXACTLY ONCE. */
  old: string
  /** Replacement text. */
  new: string
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
  if (!args.old) {
    throw new Error(`fs__edit_file: 'old' must be a non-empty string`)
  }
  const doc = await vscode.workspace.openTextDocument(r.uri)
  const currentText = doc.getText().replace(/\r\n/g, '\n')
  const occurrences = countOccurrences(currentText, args.old)
  if (occurrences === 0) {
    throw new Error(
      `fs__edit_file: the 'old' snippet was not found in ${r.relative}`
    )
  }
  if (occurrences > 1) {
    throw new Error(
      `fs__edit_file: the 'old' snippet appears ${occurrences} times in ${r.relative}. Include more surrounding lines to make it unique.`
    )
  }
  // Compute the Range to replace. Using the live document's positionAt
  // keeps the offset-to-(line,col) mapping authoritative — works the
  // same way the LSP sees text.
  const startOffset = currentText.indexOf(args.old)
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
  return appendL4Diagnostics(
    r,
    `Edited ${r.relative} (${args.old.split('\n').length} → ${args.new.split('\n').length} lines changed)`
  )
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
  return `Moved ${r.relative} to Trash`
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
