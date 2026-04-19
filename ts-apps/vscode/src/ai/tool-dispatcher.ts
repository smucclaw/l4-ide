import type { AiLogger } from './logger.js'
import {
  fsCreateFile,
  fsDeleteFile,
  fsEditFile,
  fsReadFile,
  previewProposedContent,
} from './tools/fs.js'
import {
  categoryForTool,
  getPermission,
  type PermissionValue,
} from './permissions.js'

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
    status: 'running' | 'done' | 'error',
    detail?: { result?: string; error?: string }
  ) => void
  getProposedDiff: (call: ToolCall) => Promise<{
    relativePath: string
    current: string
    proposed: string
  } | null>
}

export class ToolDispatcher {
  constructor(private readonly opts: ToolDispatcherOptions) {}

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
      this.opts.notifyStatus(call.callId, 'running', {
        result: 'awaiting approval',
      })
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
      const output = await this.execute(call.name, args)
      this.opts.notifyStatus(call.callId, 'done', { result: output })
      return { ok: true, output }
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err)
      this.opts.logger.warn(`tool/failed ${call.name}: ${msg}`)
      this.opts.notifyStatus(call.callId, 'error', { error: msg })
      return { ok: false, code: 'failed', error: msg }
    }
  }

  /**
   * Cheap lookup used by the UI layer to populate the diff overlay
   * BEFORE execution. Called from register.ts for `file/openDiff`.
   */
  previewProposedDiff(call: ToolCall): Promise<{
    relativePath: string
    current: string
    proposed: string
  } | null> {
    return this.opts.getProposedDiff(call)
  }

  private async execute(name: string, args: unknown): Promise<string> {
    switch (name) {
      case 'fs__read_file':
        return fsReadFile(args as { path: string })
      case 'fs__create_file':
        return fsCreateFile(args as { path: string; content: string })
      case 'fs__edit_file':
        return fsEditFile(args as { path: string; old: string; new: string })
      case 'fs__delete_file':
        return fsDeleteFile(args as { path: string })
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

/** Re-exported so the ChatService can surface permission info. */
export { previewProposedContent }
export type { PermissionValue }
