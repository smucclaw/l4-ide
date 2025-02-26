import type { NotificationType, RequestType } from 'vscode-messenger-common'

import { VisualizeDecisionLogicIRInfo } from './viz-expr.js'

/**
 * Using a request so that the extension can know whether the webview received it.
 * See also Wrapper / Protocol interfaces in viz-expr.ts
 */
export const VisualizeDecisionLogicRequest: RequestType<
  VisualizeDecisionLogicIRInfo,
  VisualizeDecisionLogicResponse
> = {
  method: 'visualizeDecisionLogic',
}

export type VisualizeDecisionLogicResponse =
  | { $type: 'ok' }
  | { $type: 'error' }

export function makeSuccessVisualizeResponse(): VisualizeDecisionLogicResponse {
  return { $type: 'ok' }
}

export function makeFailureVisualizeResponse(): VisualizeDecisionLogicResponse {
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
