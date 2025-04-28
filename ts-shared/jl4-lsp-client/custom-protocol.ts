/**
 * Custom extensions to the LSP for JL4
 *
 * TODO:
 * - Look into whether shld add the other viz requests here;
 *   not sure if that'd make sense since the viz stuff is done through commands
 *   (as opposed to custom methods).
 *
 * Examples of other languages that extend the LSP:
 * - https://github.com/rust-lang/rust-analyzer/blob/master/editors/code/src/lsp_ext.ts
 * - https://github.com/Dart-Code/Dart-Code/blob/master/src/shared/analysis/lsp/custom_protocol.ts
 */

import { RequestType } from 'vscode-languageclient'
import { EvalAppRequestParams, EvalAppResult } from '@repo/viz-expr'
export { EvalAppRequestParams, EvalAppResult }

export type LspResult<T> = T | null

/**
 * Request type for evaluating an App expr with actual arguments on the backend
 */
export const EvalAppRequestType = new RequestType<
  EvalAppRequestParams,
  LspResult<EvalAppResult>,
  void
>('l4/evalApp')
