import * as vscode from 'vscode'
import { l4Evaluate, type L4EvaluateArgs } from './ai/tools/l4-evaluate.js'

/**
 * Language model tools contributed to the editor's own AI (Copilot
 * agent mode, and anything else that drives `vscode.lm.tools`).
 *
 * These are the L4 capabilities that only exist inside the extension —
 * they need the live jl4-lsp session, so they can't come through the
 * MCP proxy (which fronts deployed rules on the jl4-service). Copilot
 * agent mode invokes them autonomously; users can also `#`-reference
 * them (see `toolReferenceName` in package.json).
 *
 * Declared in `contributes.languageModelTools`; the schema and
 * descriptions live there, only the implementations live here.
 */

/**
 * Names of the tools this module registers. The `@legalese` chat
 * participant excludes these when it merges `vscode.lm.tools` into its
 * own tool list — it already ships the same capability as a built-in
 * (`l4__evaluate`), and offering the model both shapes of the same tool
 * just wastes context and invites inconsistent call patterns.
 */
export const LM_TOOL_NAMES: ReadonlySet<string> = new Set(['l4_evaluate'])

export function registerLanguageModelTools(
  outputChannel: vscode.OutputChannel
): vscode.Disposable {
  // `vscode.lm.registerTool` landed in 1.95 (our engines floor), but
  // forks built on older bases may still lack it — degrade silently.
  if (typeof vscode.lm?.registerTool !== 'function') {
    outputChannel.appendLine(
      '[lm-tools] Language model tools API unavailable on this host — skipping'
    )
    return { dispose: () => undefined }
  }

  const evaluateTool: vscode.LanguageModelTool<L4EvaluateArgs> = {
    prepareInvocation: (options) => ({
      invocationMessage: `Evaluating L4 directives in ${options.input.path}`,
    }),
    invoke: async (options) => {
      let text: string
      try {
        text = await l4Evaluate(options.input)
      } catch (err) {
        // Return errors as content instead of throwing: the model can
        // act on "file not in workspace" / "failed to open"; a thrown
        // error just aborts its turn.
        text = err instanceof Error ? err.message : String(err)
      }
      return new vscode.LanguageModelToolResult([
        new vscode.LanguageModelTextPart(text),
      ])
    },
  }

  outputChannel.appendLine('[lm-tools] Registered l4_evaluate')
  return vscode.lm.registerTool('l4_evaluate', evaluateTool)
}
