import type {
  LspResponse,
  EvalAppRequestParams,
  EvalAppResult,
} from './custom-protocol.js'
import { IAmDisposable } from './utils.js'

/**
 * A wrapper around the relevant IDE's language client, for the L4 language.
 *
 * Examples of other PL LSPs that similarly put the relevant IDE's language client in a wrapper object:
 * - https://github.com/leanprover/vscode-lean4/blob/44bf3e6ce3a151ab457c061e16be2508066a518d/vscode-lean4/src/leanclient.ts#L77
 * - https://github.com/Shopify/ruby-lsp/blob/b2a3ac3279418b87df32eac4c9eb49a8b212ac0e/vscode/src/client.ts#L350
 * - https://github.com/Dart-Code/Dart-Code/blob/6baf7cad43849bbb4e50fba1f96351a20534c4bd/src/extension/analysis/analyzer.ts#L38
 *
 * What we have now is quite simple. As the above examples demonstrate, there is a lot more that can be done with this wrapper object.
 */
export interface L4LanguageClient extends IAmDisposable {
  start(): Promise<void>

  sendEvalAppRequest(
    params: EvalAppRequestParams
  ): Promise<LspResponse<EvalAppResult>>
}
