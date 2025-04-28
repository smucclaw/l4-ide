/**
 * Custom extensions to the LSP for JL4
 *
 * TODO:
 * - Add the other viz requests (will be done in another PR),
 *   since the intent is to have all the protocol extensions collected here.
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
