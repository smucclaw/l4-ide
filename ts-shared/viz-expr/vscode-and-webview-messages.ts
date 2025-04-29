// It's not clear that the `viz-expr` pacakge is the best place for the vscode x webview message types.
// But we can think about this again when there are more message types.

import type { NotificationType, RequestType } from 'vscode-messenger-common'
import { RenderAsLadderInfo } from './viz-expr.js'

/*************************************************************
   Render FunDecl in Ladder Visualizer Request and Response
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
