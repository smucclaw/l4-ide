import type {
  L4RpcRequestType,
  L4RpcNotificationType,
} from './custom-protocol.js'
import {
  makeL4RpcRequestType,
  makeL4RpcNotificationType,
} from './custom-protocol.js'
import { RenderAsLadderInfo } from '@repo/viz-expr'
import type { NotificationType, RequestType } from 'vscode-messenger-common'

/*************************************************************
              On webview frontend initialization
**************************************************************/

export interface WebviewFrontendIsReadyMessage {
  $type: 'webviewReady'
}

export const WebviewFrontendIsReadyNotification: NotificationType<WebviewFrontendIsReadyMessage> =
  {
    method: 'webviewFrontendIsReady',
  }

/*************************************************************
            Render FunDecl in Ladder Visualizer 
                Request and Response
                  for VSCode Webview
**************************************************************/

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

/*************************************************************
            For the vscode Webview to
            tell, via the Ladder Backend API,
            the VSCode Extension to
            forward a LSP client request
**************************************************************/

/** Returns the (vscode-webview-messenger) Request type for webview to get extension to forward a request to the language server. */
export const makeLspRelayRequestType = <P extends object, R>(): RequestType<
  ClientRequestParams<P, R>,
  R
> => ({
  method: 'sendClientRequest',
})

/** Payload for {@link makeLspRelayRequestType}:
 * contains all the info needed for the extension to send a request to the language server. */
export interface ClientRequestParams<P extends object, R> {
  requestType: L4RpcRequestType<P, R>
  params: P
}

/*******************************************************************************
 Convert between L4 RPC types and vscode-messenger's Request/Notification types
********************************************************************************/

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

export function toWebviewMessengerNotificationType<P extends object>(
  notificationType: L4RpcNotificationType<P>
): NotificationType<P> {
  return {
    method: notificationType.method,
  }
}

export function fromWebviewMessengerNotificationType<P extends object>(
  notificationType: NotificationType<P>
): L4RpcNotificationType<P> {
  return makeL4RpcNotificationType(notificationType.method)
}
