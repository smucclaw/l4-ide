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

/** Cheap, lexer-shape-independent validation: rejects inputs that can
 *  never be valid identifier text in L4 surface syntax regardless of
 *  the file they target — annotations (`@…`), directives (`#…`), line
 *  comments (`--…`), path/URL-shaped strings, multi-line input, and
 *  stray interior backticks. We deliberately DO NOT do a keyword check
 *  here: that's the lexer's call, and it depends on the file state, so
 *  it lives in `classifyAnchorToken` (uses the LSP's semantic tokens).
 *  Returns the bare name (backticks stripped); the caller re-wraps per
 *  occurrence. */
function validateRefactorName(
  raw: unknown,
  role: 'oldName' | 'newName'
): string {
  if (typeof raw !== 'string' || raw.length === 0) {
    throw new Error(
      `l4__refactor (rename): ${role} is required (non-empty string)`
    )
  }
  const name = raw.replace(/^`+|`+$/g, '')
  if (name.length === 0) {
    throw new Error(
      `l4__refactor (rename): ${role} must contain at least one non-backtick character`
    )
  }
  // L4's lexer disallows backticks inside a quoted identifier (see
  // jl4-core/src/L4/Lexer.hs `quoted`). Reject before we emit text that
  // wouldn't lex.
  if (name.includes('`')) {
    throw new Error(
      `l4__refactor (rename): ${role} must not contain an interior backtick character.`
    )
  }
  if (/[\r\n]/.test(name)) {
    throw new Error(
      `l4__refactor (rename): ${role} must be a single line — got ${JSON.stringify(name)}.`
    )
  }
  // '@' prefixes L4 annotations: @ref, @ref-src, @ref-map, @nlg, @desc,
  // @export. These attach to declarations; they aren't identifiers
  // themselves. Reject so a model that pastes "@export" or a
  // markdown-style "@l4-ide/doc/.../foo.md" gets a clear error.
  if (name.startsWith('@')) {
    throw new Error(
      `l4__refactor (rename): ${role} ${JSON.stringify(name)} starts with '@' — that prefix marks an L4 annotation (@ref, @desc, @export, @nlg), not an identifier. Pass the identifier name as it appears in source.`
    )
  }
  // '#' prefixes L4 directives: #EVAL, #EVALTRACE, #CHECK, #TRACE, #ASSERT.
  if (name.startsWith('#')) {
    throw new Error(
      `l4__refactor (rename): ${role} ${JSON.stringify(name)} starts with '#' — that prefix marks an L4 directive (#EVAL, #CHECK, …), not an identifier.`
    )
  }
  // '--' opens a line comment.
  if (name.startsWith('--')) {
    throw new Error(
      `l4__refactor (rename): ${role} ${JSON.stringify(name)} starts with '--' — that's an L4 line comment, not an identifier.`
    )
  }
  // Path / URL shapes — the model occasionally lifts a doc filename
  // (e.g. "@l4-ide/doc/tutorials/exporting-rules.md") into this slot.
  if (name.includes('/') || name.includes('\\')) {
    throw new Error(
      `l4__refactor (rename): ${role} ${JSON.stringify(name)} contains a path separator ('/' or '\\'). Pass the identifier as it appears in the source file, not a file path or URL.`
    )
  }
  return name
}

/** LSP/VSCode semantic-token types that count as identifier-like — the
 *  things a rename can legitimately target. jl4-lsp emits `variable`
 *  for plain identifiers and refines to `function`/`class`/`enum`/
 *  `type`/`typeParameter`/`interface` in typed contexts (see
 *  jl4-lsp/src/LSP/L4/SemanticTokens.hs). Everything else — `keyword`,
 *  `comment`, `string`, `number`, `operator`, `macro` (directive),
 *  `decorator` (annotation) — is not a rename target. */
const IDENTIFIER_TOKEN_TYPES: ReadonlySet<string> = new Set([
  'variable',
  'function',
  'class',
  'enum',
  'enumMember',
  'interface',
  'struct',
  'type',
  'typeParameter',
  'parameter',
  'property',
  'method',
  'namespace',
  'event',
  'label',
])

/** Friendlier names for the non-identifier categories jl4-lsp emits.
 *  Maps the LSP type name to "what the model probably gave us". */
const NON_IDENTIFIER_LABELS: Readonly<Record<string, string>> = {
  keyword: 'an L4 keyword',
  comment: 'inside a comment',
  string: 'inside a string literal',
  number: 'a numeric literal',
  operator: 'an operator',
  macro: 'an L4 directive (#EVAL/#CHECK/…)',
  decorator: 'an L4 annotation (@ref/@desc/@export/…)',
}

/** "Is the cursor on a renameable identifier?" — used by
 *  `commandRenameIdentifier` to short-circuit with a friendly message
 *  before opening the rename input box. Combines a cheap text-shape
 *  pre-filter (`identifierAtPosition`) with an LSP-side classification
 *  via `classifyAnchorToken`. Returns:
 *    - `false` when the position clearly isn't an identifier
 *    - `true`  when the LSP confirms an identifier-like token
 *    - `null`  when the LSP can't classify (provider not ready, file
 *              hasn't type-checked yet) — the caller treats `null` the
 *              same as `true` so the command isn't blocked while the
 *              LSP is warming up. */
async function isAtRenameableIdentifier(
  doc: vscode.TextDocument,
  pos: vscode.Position
): Promise<boolean | null> {
  if (identifierAtPosition(doc, pos) === null) return false
  const tokenType = await classifyAnchorToken(doc.uri, pos)
  if (tokenType === null) return null
  return IDENTIFIER_TOKEN_TYPES.has(tokenType)
}

/** Walk the delta-encoded semantic tokens (VSCode SemanticTokens.data:
 *  5-tuples of [deltaLine, deltaStartChar, length, tokenTypeIdx,
 *  modifierMask]) to find the token whose interval contains `pos`, and
 *  return its type name via the legend.  Returns `null` if no token
 *  covers the position or if semantic tokens are unavailable (e.g. the
 *  file hasn't type-checked yet, the provider isn't ready, or the
 *  command isn't supported in this host).  A `null` return is treated
 *  as "no lexer-side check possible" — the downstream references
 *  provider then catches non-identifier anchors with its own error. */
async function classifyAnchorToken(
  uri: vscode.Uri,
  pos: vscode.Position
): Promise<string | null> {
  let legend: vscode.SemanticTokensLegend | undefined
  let tokens: vscode.SemanticTokens | undefined
  try {
    legend = await vscode.commands.executeCommand<
      vscode.SemanticTokensLegend | undefined
    >('vscode.provideDocumentSemanticTokensLegend', uri)
    tokens = await vscode.commands.executeCommand<
      vscode.SemanticTokens | undefined
    >('vscode.provideDocumentSemanticTokens', uri)
  } catch {
    return null
  }
  if (!legend || !tokens || !tokens.data || tokens.data.length === 0) {
    return null
  }
  const data = tokens.data
  let line = 0
  let char = 0
  for (let i = 0; i + 4 < data.length; i += 5) {
    const deltaLine = data[i]!
    const deltaStart = data[i + 1]!
    const length = data[i + 2]!
    const typeIdx = data[i + 3]!
    if (deltaLine === 0) {
      char += deltaStart
    } else {
      line += deltaLine
      char = deltaStart
    }
    if (line === pos.line) {
      if (char <= pos.character && pos.character < char + length) {
        return legend.tokenTypes[typeIdx] ?? null
      }
    } else if (line > pos.line) {
      break
    }
  }
  return null
}

/** Escape a string for use inside a JS RegExp pattern. */
function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
}

/** Enumerate every source-text occurrence of `oldName` in `doc` as a
 *  position picked STRICTLY INSIDE the name token (mid-character).
 *  Backticked occurrences come first, then bare-identifier occurrences,
 *  in the order they appear in the file.
 *
 *  Why mid-character: jl4-lsp's `lookupReference` does an interval
 *  search at the supplied SrcPos and returns refs for EVERY symbol
 *  whose interval contains that pos. If we point at the first or last
 *  character of the name, we sit on the boundary of any enclosing AST
 *  node that starts/ends there too — the LSP then unions in those
 *  outer nodes' references, which arrive as ranges that don't match
 *  the name we're renaming. Anchoring one column past the start (and,
 *  for backticked names, past the opening backtick) keeps us inside
 *  just the name's interval.
 *
 *  Multiple candidates matter because the textual scan happily hits
 *  occurrences inside `--`-comments or string literals before reaching
 *  the real declaration. The caller classifies each candidate via the
 *  LSP and stops on the first identifier-like hit. */
function findOccurrences(
  doc: vscode.TextDocument,
  oldName: string
): vscode.Position[] {
  const text = doc.getText()
  const esc = escapeRegExp(oldName)
  const positions: vscode.Position[] = []
  // Backticked form first — handles names with spaces or punctuation.
  const backtickRe = new RegExp('`' + esc + '`', 'g')
  for (let m = backtickRe.exec(text); m !== null; m = backtickRe.exec(text)) {
    // Step past the opening backtick AND one char into the name so the
    // cursor isn't on the boundary of either the backtick token or the
    // identifier's first character.
    const offset = oldName.length >= 2 ? 2 : 1
    positions.push(doc.positionAt(m.index + offset))
  }
  // Bare identifier — require word boundaries so a request to rename
  // `foo` doesn't accidentally start from `foobar`.
  if (BARE_IDENT_RE.test(oldName)) {
    const wordRe = new RegExp('\\b' + esc + '\\b', 'g')
    for (let m = wordRe.exec(text); m !== null; m = wordRe.exec(text)) {
      // +1 puts us one column inside the identifier; for a single-char
      // name fall back to the start (no interior to point at).
      const offset = oldName.length >= 2 ? 1 : 0
      positions.push(doc.positionAt(m.index + offset))
    }
  }
  return positions
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
  // Validate both names against L4's lexical rules up front: rejects
  // annotations (@ref, @desc, @export), directives (#EVAL, #CHECK),
  // line comments (--…), path/URL-shaped inputs, reserved keywords,
  // and stray backticks — each with a specific error message instead
  // of a downstream "identifier not found".
  const oldName = validateRefactorName(args.oldName, 'oldName')
  const newName = validateRefactorName(args.newName, 'newName')
  if (oldName === newName) {
    return `l4__refactor (rename): oldName equals newName ("${oldName}") — nothing to do.`
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

  // The textual scan happily matches occurrences inside `--`-comments
  // and string literals before reaching the real declaration. Walk
  // every candidate and ask the LSP's semantic-tokens classifier per
  // position; keep the first one that classifies as an identifier (or
  // that the LSP can't classify yet — null falls through, so the
  // references provider still gets a shot when semantic tokens aren't
  // warm). Only error out when no candidate is identifier-shaped.
  const candidates = findOccurrences(doc, oldName)
  if (candidates.length === 0) {
    throw new Error(
      `l4__refactor (rename): identifier "${oldName}" not found in ${workspaceRelative(uri)}. Pass the identifier exactly as it appears in the source (without backticks).`
    )
  }
  let position: vscode.Position | null = null
  let lastNonIdentLabel: string | null = null
  for (const candidate of candidates) {
    const tokenType = await classifyAnchorToken(uri, candidate)
    if (tokenType === null || IDENTIFIER_TOKEN_TYPES.has(tokenType)) {
      position = candidate
      break
    }
    lastNonIdentLabel = NON_IDENTIFIER_LABELS[tokenType] ?? `'${tokenType}'`
  }
  if (!position) {
    const label = lastNonIdentLabel ?? 'not an identifier'
    throw new Error(
      `l4__refactor (rename): every textual occurrence of "${oldName}" in ${workspaceRelative(uri)} is ${label} — no identifier-typed occurrence to anchor on. Pass the name of a value, type, or function defined in the file.`
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
      if (trimmed !== oldName) continue
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
  // L4's references provider walks the anchor file's DEPENDENCIES
  // (imports) and its reverse-deps (importers), not sibling importers.
  // If the rename only touched the anchor file, hint that anchoring on
  // the DEFINING file would cover all importers.
  const hint =
    filesEdited.length === 1
      ? ` If "${oldName}" is also used in files that don't import "${filesEdited[0]}", re-run with \`path\` set to the file where "${oldName}" is DEFINED — the rename then propagates to every importer.`
      : ''
  return `Renamed "${oldName}" → "${newName}" — ${totalEdits} occurrence${totalEdits === 1 ? '' : 's'} across ${filesEdited.length} file${filesEdited.length === 1 ? '' : 's'} (${fileSummary}).${hint}`
}

/**
 * VSCode command entry-point: prompts the user for a new name based on
 * the identifier under the cursor, then drives `l4Rename`. Registered
 * as `l4.renameIdentifier` so it's reachable from the command palette
 * (and bindable to a keystroke by the user).
 */
export async function commandRenameIdentifier(): Promise<void> {
  const notOnIdentifier = (): void =>
    void vscode.window.showInformationMessage(
      'Cursor position is not targeting a valid L4 identifier.'
    )

  const editor = vscode.window.activeTextEditor
  if (!editor || editor.document.languageId !== 'l4') {
    notOnIdentifier()
    return
  }
  const doc = editor.document
  const pos = editor.selection.active
  const oldName = identifierAtPosition(doc, pos)
  if (!oldName) {
    notOnIdentifier()
    return
  }
  // Confirm via the LSP that the cursor isn't on a keyword/directive/
  // annotation that just happens to look identifier-shaped to the text
  // heuristic. `null` means the LSP can't tell yet (file not
  // type-checked) — fall through and let the rename pipeline make the
  // final call.
  const isIdent = await isAtRenameableIdentifier(doc, pos)
  if (isIdent === false) {
    notOnIdentifier()
    return
  }
  const newName = await vscode.window.showInputBox({
    prompt: `Rename "${oldName}" to (backticks added automatically when the new name contains spaces or punctuation):`,
    value: oldName,
    validateInput: (v) => {
      const trimmed = (v ?? '').trim().replace(/^`+|`+$/g, '')
      if (!trimmed) return 'New name cannot be empty.'
      if (trimmed.includes('`'))
        return 'Backticks are not allowed inside the name — type it bare; quoting is added on write.'
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
