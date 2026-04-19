import * as vscode from 'vscode'
import type { AiChatMessage } from 'jl4-client-rpc'
import type { VSCodeL4LanguageClient } from '../vscode-l4-language-client.js'
import { GetExportedFunctionsRequestType } from 'jl4-client-rpc'

/**
 * Build the first-turn `<workspace-exports>` system message listing the
 * L4 functions the model should know about. Sent as a separate system
 * message so it doesn't invalidate the proxy's cached L4 reference.
 *
 * Phase 1 keeps this narrow: scan the active file (if any) via the LSP
 * for its exports. Expanding to a multi-file index can come later.
 */
export async function buildWorkspaceBootstrapMessage(
  client: VSCodeL4LanguageClient
): Promise<AiChatMessage | null> {
  const editor = vscode.window.activeTextEditor
  const lines: string[] = []
  lines.push('<workspace-exports>')

  if (
    editor &&
    (editor.document.languageId === 'l4' ||
      editor.document.languageId === 'jl4')
  ) {
    try {
      const response = await client.sendRequest(
        GetExportedFunctionsRequestType,
        {
          verDocId: {
            uri: editor.document.uri.toString(),
            version: editor.document.version,
          },
        }
      )
      const fns = response?.functions ?? []
      if (fns.length === 0) {
        lines.push('(no L4 exports defined in the active file yet)')
      } else {
        for (const fn of fns) {
          const props = fn.parameters?.properties ?? {}
          const params = Object.entries(props)
            .map(([name, spec]) => `${name}: ${formatParamType(spec)}`)
            .join(', ')
          const ret = fn.returnType ?? 'Any'
          lines.push(`- ${fn.name}(${params}) -> ${ret}`)
        }
      }
    } catch {
      lines.push('(exports unavailable — language server not ready)')
    }
  } else {
    lines.push('(no L4 file open)')
  }

  lines.push('</workspace-exports>')
  return { role: 'system', content: lines.join('\n') }
}

/**
 * Best-effort type stringifier for a `FunctionParameter`. The LSP sends
 * JSON-Schema-ish shapes; most useful for the AI is just the `type`
 * field, falling back to `Any` when untyped or a compound we'd rather
 * not serialize inline.
 */
function formatParamType(spec: unknown): string {
  if (!spec || typeof spec !== 'object') return 'Any'
  const rec = spec as Record<string, unknown>
  if (typeof rec.type === 'string') return rec.type
  return 'Any'
}
