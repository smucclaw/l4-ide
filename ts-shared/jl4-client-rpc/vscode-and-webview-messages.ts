import type { L4RpcRequestType } from './custom-protocol.js'
import { makeL4RpcRequestType } from './custom-protocol.js'
import type { NotificationType, RequestType } from 'vscode-messenger-common'
import { RenderAsLadderInfo } from '@repo/viz-expr'

/*************************************************************
     Conversion helpers
**************************************************************/

export function toWebviewMessengerRequestType<P extends object, R>(
  requestType: L4RpcRequestType<P, R>
): RequestType<P, R> {
  return {
    method: requestType.method,
  }
}

export function fromWebviewMessengerRequestType<P extends object, R>(
  requestType: RequestType<P, R>
): L4RpcRequestType<P, R> {
  return makeL4RpcRequestType(requestType.method)
}

/*************************************************************
   Render FunDecl in Ladder Visualizer Request and Response
                for VSCode Webview
**************************************************************/

// TODO: The RenderAsLadder etc types should also use
// our wrapper L4RpcRequestType type --- that we are using vscode-messenger for commn between webview and extension is an internal implementational detail

/** This is the 'please visualize this fun decl' request for the VSCode webview.
 * Using a request so that the extension can know whether the webview received it.
 * See also Wrapper / Protocol interfaces in viz-expr.ts
 */
export const RenderAsLadder: RequestType<
  RenderAsLadderInfo,
  RenderAsLadderResponse
> = {
  method: 'renderAsLadder',
}

export type RenderAsLadderResponse = { $type: 'ok' } | { $type: 'error' }

export function makeRenderAsLadderSuccessResponse(): RenderAsLadderResponse {
  return { $type: 'ok' }
}

export function makeRenderAsLadderFailureResponse(): RenderAsLadderResponse {
  return { $type: 'error' }
}

/*************************************
  On webview frontend initialization
**************************************/

export interface WebviewFrontendIsReadyMessage {
  $type: 'webviewReady'
}

export const WebviewFrontendIsReadyNotification: NotificationType<WebviewFrontendIsReadyMessage> =
  {
    method: 'webviewFrontendIsReady',
  }
