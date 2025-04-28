/**
 * Custom extensions to the LSP for JL4
 *
 * TODO:
 * - Add the other viz requests (will be done in another PR),
 *   since the intent is to have all the protocol extensions collected here.
 *
 * Examples of other languages that extend the LSP:
 * - https://github.com/rust-lang/rust-analyzer/blob/master/editors/code/src/lsp_ext.ts
 * - https://github.com/Dart-Code/Dart-Code/blob/master/src/shared/analysis/lsp/custom_protocol.ts
 */

import {
  // NotificationType,
  RequestType,
} from 'vscode-languageclient'
import { EvalAppRequestParams, EvalAppResult } from '@repo/viz-expr'

export type LspResult<T> = T | null

/**
 * Request type for evaluating an App expr with actual arguments on the backend
 */
export const EvalAppRequestType = new RequestType<
  EvalAppRequestParams,
  LspResult<EvalAppResult>,
  void
>('jl4/evalApp')
