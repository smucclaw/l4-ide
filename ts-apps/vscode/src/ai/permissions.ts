import * as vscode from 'vscode'

/**
 * Per-category permission gate for client-side tools.
 *
 * Phase 1 plan called for braver defaults than a typical "ask for
 * everything" posture: the user invoked the AI to do work, so gating
 * every file read with a confirmation is friction theater. Only
 * operations with serious blast radius default to Ask — today that's
 * `fs.delete` and any newly-added third-party MCP server.
 *
 * Permission values are persisted in VSCode settings under
 * `legaleseAi.permissions.*` so they survive reloads and flow through
 * settings-sync.
 */
export type PermissionValue = 'never' | 'ask' | 'always'

export type PermissionCategory =
  | 'fs.read'
  | 'fs.create'
  | 'fs.edit'
  | 'fs.delete'
  | 'lsp.evaluate'
  | 'l4.evaluate'
  | 'mcp.l4Rules'
  | 'meta.askUser'
  | 'meta.statusUpdate'

const CATEGORY_SETTING: Record<PermissionCategory, string> = {
  'fs.read': 'legaleseAi.permissions.readFiles',
  'fs.create': 'legaleseAi.permissions.createFiles',
  'fs.edit': 'legaleseAi.permissions.editFiles',
  'fs.delete': 'legaleseAi.permissions.deleteFiles',
  'lsp.evaluate': 'legaleseAi.permissions.lspDiagnostics',
  'l4.evaluate': 'legaleseAi.permissions.evaluateL4',
  'mcp.l4Rules': 'legaleseAi.permissions.runDeployedRules',
  'meta.askUser': 'legaleseAi.permissions.askUser',
  'meta.statusUpdate': 'legaleseAi.permissions.statusUpdate',
}

const DEFAULTS: Record<PermissionCategory, PermissionValue> = {
  'fs.read': 'always',
  'fs.create': 'always',
  'fs.edit': 'always',
  // Destructive. Always confirm unless the user explicitly opts out.
  'fs.delete': 'ask',
  'lsp.evaluate': 'always',
  'l4.evaluate': 'always',
  'mcp.l4Rules': 'always',
  // `meta__ask_user` has no side effects — it IS the user prompt.
  'meta.askUser': 'always',
  // `meta__post_status_update` only writes a line of prose into the
  // assistant bubble. Always allowed; never prompt.
  'meta.statusUpdate': 'always',
}

export function getPermission(category: PermissionCategory): PermissionValue {
  const setting = CATEGORY_SETTING[category]
  const raw = vscode.workspace
    .getConfiguration()
    .get<string>(setting)
    ?.toLowerCase()
  if (raw === 'never' || raw === 'ask' || raw === 'always') return raw
  return DEFAULTS[category]
}

export async function setPermission(
  category: PermissionCategory,
  value: PermissionValue
): Promise<void> {
  const setting = CATEGORY_SETTING[category]
  await vscode.workspace
    .getConfiguration()
    .update(setting, value, vscode.ConfigurationTarget.Global)
}

/**
 * Categorize a tool call into a permission bucket. Tools live under
 * `<category>__<name>` (with `__` as the separator) so prefix-routing
 * gives us the right category for free.
 */
export function categoryForTool(toolName: string): PermissionCategory | null {
  if (toolName === 'fs__read_file') return 'fs.read'
  if (toolName === 'fs__create_file') return 'fs.create'
  if (toolName === 'fs__edit_file') return 'fs.edit'
  if (toolName === 'fs__delete_file') return 'fs.delete'
  if (toolName === 'lsp__diagnostics') return 'lsp.evaluate'
  if (toolName === 'l4__evaluate') return 'l4.evaluate'
  if (toolName === 'meta__ask_user') return 'meta.askUser'
  if (toolName === 'meta__post_status_update') return 'meta.statusUpdate'
  if (toolName.startsWith('l4-rules__')) return 'mcp.l4Rules'
  return null
}
