/**
 * Custom extensions to the LSP for JL4
 *
 * TODO:
 * - It may be better to make our own RequestType,
 *   instead of reusing the vscode-affiliated types,
 *   especially since their types are weird in some places.
 * - Look into whether shld add the other viz requests here;
 *   not sure if that'd make sense since the viz stuff is done through commands
 *   (as opposed to custom methods).
 *
 * Examples of other languages that extend the LSP:
 * - https://github.com/rust-lang/rust-analyzer/blob/master/editors/code/src/lsp_ext.ts
 * - https://github.com/Dart-Code/Dart-Code/blob/master/src/shared/analysis/lsp/custom_protocol.ts
 */

import { NotificationType, RequestType } from 'vscode-jsonrpc'
import {
  EvalAppRequestParams,
  EvalAppResult,
  InlineExprsRequestParams,
  InlineExprsSuccess,
} from '@repo/viz-expr'
export {
  EvalAppRequestParams,
  EvalAppResult,
  InlineExprsRequestParams,
  InlineExprsSuccess,
}

/****************************************
        L4 RPC Protocol Types
*****************************************/

// Request type
export type L4RpcRequestType<P extends object, R> = RequestType<P, R, void>

export function makeL4RpcRequestType<P extends object, R>(
  method: string
): L4RpcRequestType<P, R> {
  return new RequestType<P, R, void>(method)
}

// LspResponse
export type LspResponse<T> = T | null

// Notification type
export type L4RpcNotificationType<P extends object> = NotificationType<P>

export function makeL4RpcNotificationType<P extends object>(
  method: string
): L4RpcNotificationType<P> {
  return new NotificationType<P>(method)
}

/****************************************
    Specific protocol extensions
*****************************************/

/**
 * Request type for evaluating an App expr with actual arguments on the backend
 */
export const EvalAppRequestType = makeL4RpcRequestType<
  EvalAppRequestParams,
  EvalAppResult
>('l4/evalApp')

/**
 * Request type for inlining exprs
 */
export const InlineExprsRequestType = makeL4RpcRequestType<
  InlineExprsRequestParams,
  InlineExprsSuccess
>('l4/inlineExprs')
