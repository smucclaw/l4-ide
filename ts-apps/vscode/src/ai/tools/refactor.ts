import * as vscode from 'vscode'
import { resolveFileUri, workspaceRelative } from './fs.js'

/**
 * L4 refactor tool — single AI-facing entry point with an `action`
 * discriminator. Today we ship `rename`; more refactors (extract,
 * inline, …) plug in as additional cases without growing the tool
 * surface area or having the model learn new tool names.
 *
 * Currently supported actions:
 *  - `rename`: rename an identifier across the file AND every file
 *    that IMPORTs it. Drives the LSP's existing references provider
 *    (jl4-lsp/app/LSP/L4/Handlers.hs SMethod_TextDocumentReferences,
 *    which already unions matches across reverse-import deps) and
 *    applies the substitution to every returned Location via one
 *    WorkspaceEdit. Preserves backtick quoting per-occurrence; force-
 *    wraps when the new name contains characters outside [A-Za-z0-9_].
 *
 * No native LSP rename is implemented server-side — driving the
 * references provider gives us cross-file scope (including transitive
 * IMPORTers) without having to add a textDocument/rename handler to
 * jl4-lsp first.
 */

export type L4RefactorAction = 'rename'

export interface L4RefactorArgs {
  action: L4RefactorAction
  path: string
  // `rename` action parameters
  oldName?: string
  newName?: string
}

interface L4RenameArgs {
  path: string
  oldName: string
  newName: string
}

/**
 * Dispatch a refactor action. Validates the action discriminator
 * up-front so an unknown action produces a clear error rather than a
 * silent miss on a missing parameter further down. Throws on failure;
 * the tool dispatcher wraps the throw into a `{ok: false}` tool
 * result for the model.
 */
export async function l4Refactor(args: L4RefactorArgs): Promise<string> {
  if (!args || typeof args !== 'object') {
    throw new Error('l4__refactor: arguments object is required')
  }
  if (typeof args.action !== 'string' || args.action.length === 0) {
    throw new Error(
      "l4__refactor: 'action' is required. Supported actions: rename."
    )
  }
  switch (args.action) {
    case 'rename':
      if (
        typeof args.oldName !== 'string' ||
        typeof args.newName !== 'string'
      ) {
        throw new Error(
          "l4__refactor: action='rename' requires 'oldName' and 'newName'."
        )
      }
      return l4Rename({
        path: args.path,
        oldName: args.oldName,
        newName: args.newName,
      })
    default:
      throw new Error(
        `l4__refactor: unknown action "${args.action}". Supported actions: rename.`
      )
  }
}

const BARE_IDENT_RE = /^[A-Za-z_][A-Za-z0-9_]*$/

/** Escape a string for use inside a JS RegExp pattern. */
function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
}

/** Locate the first source-text occurrence of `oldName` in `doc`,
 *  trying the backticked form first then the bare-identifier form.
 *  Returns the position to send to the references provider — for
 *  backticked names that's the column INSIDE the backticks so the
 *  LSP's lookup at that SrcPos lands on the name token. */
function findFirstOccurrence(
  doc: vscode.TextDocument,
  oldName: string
): vscode.Position | null {
  const text = doc.getText()
  const esc = escapeRegExp(oldName)
  // Backticked form first — handles names with spaces or punctuation.
  const backtickMatch = new RegExp('`' + esc + '`').exec(text)
  if (backtickMatch) {
    // +1 to step inside the opening backtick onto the identifier itself.
    return doc.positionAt(backtickMatch.index + 1)
  }
  // Bare identifier — require word boundaries so a request to rename
  // `foo` doesn't accidentally start from `foobar`.
  if (BARE_IDENT_RE.test(oldName)) {
    const wordMatch = new RegExp('\\b' + esc + '\\b').exec(text)
    if (wordMatch) return doc.positionAt(wordMatch.index)
  }
  return null
}

/** Decide the literal replacement for a single occurrence: preserve
 *  backtick quoting from the source, and add backticks when the new
 *  name itself can't survive bare (e.g. contains a space). */
function renderReplacement(
  existing: string,
  newName: string,
  newNameNeedsBackticks: boolean
): string {
  const wrapped = existing.startsWith('`') && existing.endsWith('`')
  if (wrapped || newNameNeedsBackticks) return '`' + newName + '`'
  return newName
}

async function l4Rename(args: L4RenameArgs): Promise<string> {
  if (!args || typeof args !== 'object') {
    throw new Error('l4__refactor (rename): arguments object is required')
  }
  if (typeof args.oldName !== 'string' || args.oldName.length === 0) {
    throw new Error(
      'l4__refactor (rename): oldName is required (non-empty string)'
    )
  }
  if (typeof args.newName !== 'string' || args.newName.length === 0) {
    throw new Error(
      'l4__refactor (rename): newName is required (non-empty string)'
    )
  }
  // Strip any leading/trailing backticks the caller may have included —
  // we wrap them back on per-occurrence, so accepting `` `foo` `` and
  // `foo` interchangeably keeps the tool forgiving.
  const oldName = args.oldName.replace(/^`+|`+$/g, '')
  const newName = args.newName.replace(/^`+|`+$/g, '')
  if (!oldName || !newName) {
    throw new Error(
      'l4__refactor (rename): oldName / newName must contain at least one non-backtick character'
    )
  }
  if (oldName === newName) {
    return `l4__refactor (rename): oldName equals newName ("${oldName}") — nothing to do.`
  }
  // L4 disallows a bare backtick inside a quoted identifier (see
  // jl4-core/src/L4/Lexer.hs `quoted`). Reject before we apply edits
  // that would silently produce an un-lexable file.
  if (newName.includes('`')) {
    throw new Error(
      'l4__refactor (rename): newName must not contain a backtick character.'
    )
  }
  const newNameNeedsBackticks = !BARE_IDENT_RE.test(newName)

  const uri = resolveFileUri(args.path)
  if (!uri) {
    throw new Error(`l4__refactor (rename): cannot resolve path: ${args.path}`)
  }
  let doc: vscode.TextDocument
  try {
    doc = await vscode.workspace.openTextDocument(uri)
  } catch (err) {
    throw new Error(
      `l4__refactor (rename): cannot open ${workspaceRelative(uri)}: ${err instanceof Error ? err.message : String(err)}`
    )
  }

  const position = findFirstOccurrence(doc, oldName)
  if (!position) {
    throw new Error(
      `l4__refactor (rename): identifier "${oldName}" not found in ${workspaceRelative(uri)}. Pass the identifier exactly as it appears in the source (without backticks).`
    )
  }

  // VSCode's reference provider proxies to the jl4-lsp
  // textDocument/references handler, which already unions matches
  // across all reverse-dependency modules — so a rename anchored on a
  // definition in `domain.l4` finds occurrences in every file that
  // `IMPORT`s it.
  let locations: vscode.Location[] | undefined
  try {
    locations = await vscode.commands.executeCommand<vscode.Location[]>(
      'vscode.executeReferenceProvider',
      uri,
      position
    )
  } catch (err) {
    throw new Error(
      `l4__refactor (rename): references lookup failed: ${err instanceof Error ? err.message : String(err)}`
    )
  }
  if (!locations || locations.length === 0) {
    throw new Error(
      `l4__refactor (rename): no references for "${oldName}" — make sure the file type-checks (the references provider runs on the resolved module). Use l4__evaluate to check.`
    )
  }

  // Group locations by URI so we can sort each file's edits last-first
  // (offsets stay valid as we mutate from the bottom up) and so we
  // open each target doc exactly once for the existing-text lookup.
  const byUri = new Map<string, vscode.Location[]>()
  for (const loc of locations) {
    const key = loc.uri.toString()
    let list = byUri.get(key)
    if (!list) {
      list = []
      byUri.set(key, list)
    }
    list.push(loc)
  }

  const edit = new vscode.WorkspaceEdit()
  const filesEdited: string[] = []
  let totalEdits = 0
  let skipped = 0

  for (const [uriStr, locs] of byUri) {
    const targetUri = vscode.Uri.parse(uriStr)
    const targetDoc = await vscode.workspace.openTextDocument(targetUri)
    // Sort descending by (line, character) so later edits don't shift
    // earlier ones inside the same file before they apply.
    locs.sort((a, b) => {
      const lineDiff = b.range.start.line - a.range.start.line
      if (lineDiff !== 0) return lineDiff
      return b.range.start.character - a.range.start.character
    })
    let fileEdits = 0
    for (const loc of locs) {
      const existing = targetDoc.getText(loc.range)
      // Defensive: the references provider should only return ranges
      // whose text matches `oldName` (with or without backticks), but
      // skip anything that doesn't — better to under-rename than to
      // clobber an unrelated token if the LSP ever returns a stale
      // range.
      const trimmed = existing.replace(/^`+|`+$/g, '')
      if (trimmed !== oldName) {
        skipped++
        continue
      }
      edit.replace(
        targetUri,
        loc.range,
        renderReplacement(existing, newName, newNameNeedsBackticks)
      )
      totalEdits++
      fileEdits++
    }
    if (fileEdits > 0) filesEdited.push(workspaceRelative(targetUri))
  }

  if (totalEdits === 0) {
    throw new Error(
      `l4__refactor (rename): references provider returned ${locations.length} location(s) but none matched "${oldName}" verbatim — refusing to apply edits.`
    )
  }
  const ok = await vscode.workspace.applyEdit(edit)
  if (!ok) {
    throw new Error(
      'l4__refactor (rename): VSCode refused to apply the WorkspaceEdit (file may be read-only).'
    )
  }
  // Persist every dirty doc so subsequent type-checks and any
  // disk-based readers (fs__read_file, other extensions) see the new
  // names immediately.
  for (const uriStr of byUri.keys()) {
    try {
      const d = await vscode.workspace.openTextDocument(
        vscode.Uri.parse(uriStr)
      )
      if (d.isDirty) await d.save()
    } catch {
      // best-effort
    }
  }

  filesEdited.sort()
  const fileSummary = filesEdited.join(', ')
  const tail =
    skipped > 0 ? ` Skipped ${skipped} location(s) with mismatching text.` : ''
  return `Renamed "${oldName}" → "${newName}" — ${totalEdits} occurrence${totalEdits === 1 ? '' : 's'} across ${filesEdited.length} file${filesEdited.length === 1 ? '' : 's'} (${fileSummary}).${tail}`
}

/**
 * VSCode command entry-point: prompts the user for a new name based on
 * the identifier under the cursor, then drives `l4Rename`. Registered
 * as `l4.renameIdentifier` so it's reachable from the command palette
 * (and bindable to a keystroke by the user).
 */
export async function commandRenameIdentifier(): Promise<void> {
  const editor = vscode.window.activeTextEditor
  if (!editor || editor.document.languageId !== 'l4') {
    void vscode.window.showInformationMessage(
      'Open an L4 file first, then place the cursor on the identifier to rename.'
    )
    return
  }
  const doc = editor.document
  const pos = editor.selection.active
  const oldName = identifierAtPosition(doc, pos)
  if (!oldName) {
    void vscode.window.showInformationMessage(
      'No identifier under the cursor. Place the cursor on a name (or backtick-quoted name) and try again.'
    )
    return
  }
  const newName = await vscode.window.showInputBox({
    prompt: `Rename "${oldName}" to:`,
    value: oldName,
    validateInput: (v) => {
      const trimmed = (v ?? '').trim().replace(/^`+|`+$/g, '')
      if (!trimmed) return 'New name cannot be empty.'
      if (trimmed.includes('`'))
        return 'Backticks are not allowed inside the name.'
      return null
    },
  })
  if (newName === undefined) return
  const normalisedNew = newName.trim().replace(/^`+|`+$/g, '')
  if (!normalisedNew || normalisedNew === oldName) return
  try {
    const result = await l4Refactor({
      action: 'rename',
      path: doc.uri.fsPath,
      oldName,
      newName: normalisedNew,
    })
    void vscode.window.showInformationMessage(result)
  } catch (err) {
    void vscode.window.showErrorMessage(
      err instanceof Error ? err.message : String(err)
    )
  }
}

/** Extract the L4 identifier at `pos`. Handles both backticked names
 *  (`\`foo bar\``) and bare identifiers. Returns the bare name (no
 *  surrounding backticks) — the rest of the rename pipeline always
 *  wraps as needed. */
function identifierAtPosition(
  doc: vscode.TextDocument,
  pos: vscode.Position
): string | null {
  const lineText = doc.lineAt(pos.line).text
  const col = pos.character
  // Backticked form: scan for a pair of backticks that bracket `col`.
  // We look for the nearest ` to the left and the nearest ` to the
  // right, on the same line — quoted names can't span newlines.
  const left = lineText.lastIndexOf('`', col - 1)
  const right = left >= 0 ? lineText.indexOf('`', left + 1) : -1
  if (left >= 0 && right > left && col >= left && col <= right + 1) {
    const inner = lineText.slice(left + 1, right)
    if (inner.length > 0) return inner
  }
  // Bare identifier — expand left/right while we stay inside [A-Za-z0-9_].
  const isIdent = (c: string): boolean => /[A-Za-z0-9_]/.test(c)
  let start = col
  while (start > 0 && isIdent(lineText[start - 1]!)) start--
  let end = col
  while (end < lineText.length && isIdent(lineText[end]!)) end++
  if (end === start) return null
  return lineText.slice(start, end)
}
