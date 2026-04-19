import * as vscode from 'vscode'
import * as path from 'path'
import { promises as fs } from 'fs'

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
    throw new Error('No workspace is open')
  }
  // Choose a base folder: prefer the active file's folder; otherwise
  // the first workspace folder.
  const active = vscode.window.activeTextEditor?.document.uri
  const activeFolder = active
    ? vscode.workspace.getWorkspaceFolder(active)
    : undefined
  const base = activeFolder ?? folders[0]!
  const absolute = path.isAbsolute(p) ? p : path.resolve(base.uri.fsPath, p)
  // Ensure the resolved path is inside SOME workspace folder.
  const inside = folders.some(
    (f) =>
      absolute === f.uri.fsPath || absolute.startsWith(f.uri.fsPath + path.sep)
  )
  if (!inside) {
    throw new Error(
      `Path is outside the workspace: ${p}. Only workspace-relative paths are allowed.`
    )
  }
  return {
    relative:
      path.relative(base.uri.fsPath, absolute) || path.basename(absolute),
    uri: vscode.Uri.file(absolute),
    fsPath: absolute,
  }
}

export interface FsReadArgs {
  path: string
}

export async function fsReadFile(args: FsReadArgs): Promise<string> {
  const r = resolveWorkspacePath(args.path)
  const buf = await fs.readFile(r.fsPath, 'utf-8')
  // Newline-normalize so downstream tools that rely on \n splits don't
  // get confused by Windows CRLF.
  return buf.replace(/\r\n/g, '\n')
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
  await fs.mkdir(path.dirname(r.fsPath), { recursive: true })
  await fs.writeFile(r.fsPath, args.content, 'utf-8')
  return `Created ${r.relative} (${args.content.length} chars)`
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
 */
export async function fsEditFile(args: FsEditArgs): Promise<string> {
  const r = resolveWorkspacePath(args.path)
  const raw = await fs.readFile(r.fsPath, 'utf-8')
  const normalized = raw.replace(/\r\n/g, '\n')
  if (!args.old) {
    throw new Error(`fs__edit_file: 'old' must be a non-empty string`)
  }
  const occurrences = countOccurrences(normalized, args.old)
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
  const updated = normalized.replace(args.old, args.new)
  await fs.writeFile(r.fsPath, updated, 'utf-8')
  return `Edited ${r.relative} (${args.old.split('\n').length} → ${args.new.split('\n').length} lines changed)`
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
 * Best-effort "preview diff" payload: what the file would look like
 * after a proposed edit or create. The UI uses this for cmd+click
 * diff-open before applying the change. If the file doesn't exist
 * yet (create flow), `current` is empty string.
 */
export async function previewProposedContent(
  toolName: string,
  args: unknown
): Promise<{ relativePath: string; current: string; proposed: string } | null> {
  try {
    if (toolName === 'fs__create_file') {
      const a = args as FsCreateArgs
      const r = resolveWorkspacePath(a.path)
      let current = ''
      try {
        current = await fs.readFile(r.fsPath, 'utf-8')
      } catch {
        // File doesn't exist — creating fresh.
      }
      return { relativePath: r.relative, current, proposed: a.content }
    }
    if (toolName === 'fs__edit_file') {
      const a = args as FsEditArgs
      const r = resolveWorkspacePath(a.path)
      const current = (await fs.readFile(r.fsPath, 'utf-8')).replace(
        /\r\n/g,
        '\n'
      )
      const proposed = current.replace(a.old, a.new)
      return { relativePath: r.relative, current, proposed }
    }
  } catch {
    return null
  }
  return null
}
