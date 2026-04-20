import type { AiLogger } from './logger.js'
import {
  fsCreateFile,
  fsDeleteFile,
  fsEditFile,
  fsReadFile,
  resolveCurrentContents,
  resolveFileUri,
  workspaceRelative,
} from './tools/fs.js'
import { lspDiagnostics } from './tools/lsp.js'
import { l4Evaluate } from './tools/l4-evaluate.js'
import { MCP_L4_RULES_PREFIX, type McpToolClient } from './mcp-client.js'
import {
  categoryForTool,
  getPermission,
  type PermissionValue,
} from './permissions.js'
import type { Uri } from 'vscode'

/** Result returned to the proxy as the content of a `role:"tool"` message. */
export type ToolResult =
  | { ok: true; output: string }
  | { ok: false; error: string; code?: 'denied' | 'unknown_tool' | 'failed' }

export interface ToolCall {
  callId: string
  name: string
  argsJson: string
}

/**
 * Front door for every tool_call that comes back from the proxy. It
 *  1. picks the right permission category,
 *  2. consults the user's permission settings,
 *  3. either runs the tool, refuses it, or requests explicit approval,
 *  4. returns a `ToolResult` that the chat-service folds into the
 *     follow-up request to the proxy.
 *
 * `requestApproval` is injected by the ChatService so the dispatcher
 * doesn't have to know about the messenger. It resolves with the
 * user's choice ('allow' | 'deny'). 'alwaysAllow' is handled in the
 * chat-service layer (it bumps the setting, then re-runs via this
 * dispatcher with the new permission value).
 */
export interface ToolDispatcherOptions {
  logger: AiLogger
  requestApproval: (call: ToolCall) => Promise<'allow' | 'deny'>
  notifyStatus: (
    callId: string,
    status: 'pending-approval' | 'running' | 'done' | 'error',
    detail?: { result?: string; error?: string }
  ) => void
  mcp: McpToolClient
}

/** Snapshot captured before a fs__edit_file / fs__create_file runs, so
 *  the applied-diff view can show an accurate "before" side once the
 *  write lands on disk. */
export interface AppliedEditSnapshot {
  callId: string
  uri: Uri
  relativePath: string
  /** File contents before the tool ran. `''` for a created-new file. */
  before: string
}

export class ToolDispatcher {
  /** Pre-edit snapshots keyed by callId, populated for fs__edit_file
   *  and fs__create_file. Lives for the extension's lifetime — small
   *  values, turnover is low. Used by the register layer to resolve
   *  cmd+click on a tool row into a "before/after" diff. */
  private readonly edits = new Map<string, AppliedEditSnapshot>()

  constructor(private readonly opts: ToolDispatcherOptions) {}

  /** Resolve a tool-call's target file to its current URI. Read /
   *  create / edit / delete all take `{ path }`. */
  resolveFile(call: ToolCall): Uri | null {
    try {
      const args = JSON.parse(call.argsJson || '{}') as { path?: string }
      if (!args.path) return null
      return resolveFileUri(args.path)
    } catch {
      return null
    }
  }

  /** Look up the pre-edit snapshot for a tool call. Returns null if
   *  the tool wasn't file-mutating or ran before we started tracking. */
  snapshotFor(callId: string): AppliedEditSnapshot | undefined {
    return this.edits.get(callId)
  }

  /**
   * Dispatch a single tool call end-to-end. On return, the caller can
   * use `.output` (or `.error`) as the content for a `role:"tool"`
   * message in the follow-up chat request.
   */
  async run(call: ToolCall): Promise<ToolResult> {
    const category = categoryForTool(call.name)
    if (!category) {
      this.opts.logger.warn(`tool/unknown: ${call.name}`)
      return {
        ok: false,
        code: 'unknown_tool',
        error: `Unknown tool: ${call.name}`,
      }
    }
    const permission = getPermission(category)
    this.opts.logger.info(
      `tool/dispatch ${call.name} (category=${category}, permission=${permission})`
    )
    if (permission === 'never') {
      this.opts.notifyStatus(call.callId, 'error', {
        error: 'Denied by user settings',
      })
      return {
        ok: false,
        code: 'denied',
        error: `User has disallowed the "${category}" category. Update the setting in the Legalese AI settings to enable it.`,
      }
    }
    if (permission === 'ask') {
      this.opts.notifyStatus(call.callId, 'pending-approval')
      const decision = await this.opts.requestApproval(call)
      if (decision === 'deny') {
        this.opts.notifyStatus(call.callId, 'error', { error: 'Denied' })
        return {
          ok: false,
          code: 'denied',
          error: 'The user denied this tool call.',
        }
      }
    }
    this.opts.notifyStatus(call.callId, 'running')
    try {
      const args = parseArgs(call.argsJson)
      // Snapshot the file BEFORE a mutating tool runs so the UI can
      // render the actual applied diff afterwards (not a "proposed"
      // preview that may never have been committed).
      await this.snapshotIfMutating(call, args)
      const output = await this.execute(call.name, args, call.argsJson)
      this.opts.notifyStatus(call.callId, 'done', { result: output })
      return { ok: true, output }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      this.opts.logger.warn(`tool/failed ${call.name}: ${msg}`)
      this.opts.notifyStatus(call.callId, 'error', { error: msg })
      return { ok: false, code: 'failed', error: msg }
    }
  }

  private async snapshotIfMutating(
    call: ToolCall,
    args: unknown
  ): Promise<void> {
    if (call.name !== 'fs__edit_file' && call.name !== 'fs__create_file') {
      return
    }
    const { path: p } = args as { path?: string }
    if (!p) return
    const uri = resolveFileUri(p)
    if (!uri) return
    try {
      const before = await resolveCurrentContents(p)
      this.edits.set(call.callId, {
        callId: call.callId,
        uri,
        relativePath: workspaceRelative(uri),
        before,
      })
    } catch {
      // Snapshot is best-effort; without it the diff view falls back
      // to an empty "before" side.
    }
  }

  private async execute(
    name: string,
    args: unknown,
    argsJson: string
  ): Promise<string> {
    if (name.startsWith(MCP_L4_RULES_PREFIX)) {
      return this.opts.mcp.callTool(name, argsJson)
    }
    switch (name) {
      case 'fs__read_file':
        return fsReadFile(args as { path: string; from?: number })
      case 'fs__create_file':
        return fsCreateFile(args as { path: string; content: string })
      case 'fs__edit_file':
        return fsEditFile(args as { path: string; old: string; new: string })
      case 'fs__delete_file':
        return fsDeleteFile(args as { path: string })
      case 'lsp__diagnostics':
        return lspDiagnostics(args as { path: string; source?: string })
      case 'l4__evaluate':
        return l4Evaluate(args as { path: string; timeoutMs?: number })
      case 'meta__ask_user':
        // Stub: the interactive question panel is still in flight (see
        // AI_CHAT_PLAN Phase 2). Declaring the tool + returning a stable
        // result keeps the upstream LLM call from failing when the model
        // emits a meta__ask_user tool_call it learned about from the
        // ai-proxy system prompt. Once the UI lands, this returns the
        // user's actual answer.
        return JSON.stringify({
          answered: false,
          note: 'The interactive question panel is not yet wired up. Do not retry meta__ask_user — ask your clarifying question directly in your assistant text so the user can reply in chat, or make your best-informed decision and proceed.',
          echoedArgs: args,
        })
      default:
        throw new Error(`No executor for tool: ${name}`)
    }
  }
}

function parseArgs(json: string): unknown {
  if (!json || !json.trim()) return {}
  try {
    return JSON.parse(json)
  } catch (err) {
    throw new Error(
      `Invalid tool arguments JSON: ${err instanceof Error ? err.message : String(err)}`
    )
  }
}

export type { PermissionValue }
