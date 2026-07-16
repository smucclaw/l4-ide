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
  | 'l4.evaluate'
  | 'l4.refactor'
  | 'mcp.l4Rules'
  | 'mcp.vscode'
  | 'meta.askUser'
  | 'meta.statusUpdate'

const CATEGORY_SETTING: Record<PermissionCategory, string> = {
  'fs.read': 'legaleseAi.permissions.readFiles',
  'fs.create': 'legaleseAi.permissions.createFiles',
  'fs.edit': 'legaleseAi.permissions.editFiles',
  'fs.delete': 'legaleseAi.permissions.deleteFiles',
  'l4.evaluate': 'legaleseAi.permissions.evaluateL4',
  'l4.refactor': 'legaleseAi.permissions.refactorL4',
  'mcp.l4Rules': 'legaleseAi.permissions.runDeployedRules',
  'mcp.vscode': 'legaleseAi.permissions.vscodeMcp',
  'meta.askUser': 'legaleseAi.permissions.askUser',
  'meta.statusUpdate': 'legaleseAi.permissions.statusUpdate',
}

const DEFAULTS: Record<PermissionCategory, PermissionValue> = {
  'fs.read': 'always',
  'fs.create': 'always',
  'fs.edit': 'always',
  // Destructive. Always confirm unless the user explicitly opts out.
  'fs.delete': 'ask',
  'l4.evaluate': 'always',
  // Refactors write to multiple files (the target + every importer
  // for cross-file actions like rename). Cross-file blast radius
  // deserves the same default treatment as fs.edit — runs without
  // prompting, but the user can flip it to `ask` in settings if they
  // want a confirmation per refactor.
  'l4.refactor': 'always',
  'mcp.l4Rules': 'always',
  // Which MCP servers/tools the model can reach is governed by the
  // per-server and per-tool toggles in the sidebar's MCP section, so
  // calls to what survived those toggles run unattended. The category
  // still exists (settable to ask/never via VS Code settings) for
  // users who want call-time confirmation on top.
  'mcp.vscode': 'always',
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
  if (toolName === 'l4__evaluate') return 'l4.evaluate'
  if (toolName === 'l4__refactor') return 'l4.refactor'
  if (toolName === 'meta__ask_user') return 'meta.askUser'
  if (toolName === 'meta__post_status_update') return 'meta.statusUpdate'
  if (toolName.startsWith('l4-rules__')) return 'mcp.l4Rules'
  if (toolName.startsWith('vsmcp__')) return 'mcp.vscode'
  return null
}
