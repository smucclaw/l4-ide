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
            Render Directive Result in Inspector Panel
                Request and Response
                  for VSCode Webview
**************************************************************/

import type { DirectiveResult, SrcPos } from './custom-protocol.js'

export interface AddInspectorResultMessage {
  directiveId: string
  srcPos: SrcPos
  result: DirectiveResult
  lineContent: string
}

export const AddInspectorResult: RequestType<
  AddInspectorResultMessage,
  { $type: 'ok' | 'scrolled' }
> = {
  method: 'addInspectorResult',
}

export interface RemoveInspectorResultMessage {
  directiveId: string
}

export const RemoveInspectorResult: NotificationType<RemoveInspectorResultMessage> =
  {
    method: 'removeInspectorResult',
  }

export interface UpdateInspectorResultMessage {
  directiveId: string
  result: DirectiveResult
}

export const UpdateInspectorResult: NotificationType<UpdateInspectorResultMessage> =
  {
    method: 'updateInspectorResult',
  }

export interface SyncInspectorResultsMessage {
  /** When set, only sections belonging to this file are synced; others are left untouched. */
  uri?: string
  results: Array<{
    directiveId: string
    prettyText: string
    success: boolean | null
    lineContent: string
  }>
}

export const SyncInspectorResults: NotificationType<SyncInspectorResultsMessage> =
  {
    method: 'syncInspectorResults',
  }

/*************************************************************
            Toggle Simplify for Ladder Visualization
**************************************************************/

export interface ToggleSimplifyMessage {
  shouldSimplify: boolean
}

export const ToggleSimplify: NotificationType<ToggleSimplifyMessage> = {
  method: 'toggleSimplify',
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

/*************************************************************
            Deploy Sidebar Messages
**************************************************************/

import type {
  ExportedFunctionInfo,
  GetExportedFunctionsParams,
  GetExportedFunctionsResponse,
} from './custom-protocol.js'

/** Sidebar asks extension for exported functions from the active file */
export const GetSidebarExportedFunctions: RequestType<
  GetExportedFunctionsParams,
  GetExportedFunctionsResponse
> = {
  method: 'getSidebarExportedFunctions',
}

/** Connection status response from extension to sidebar */
export interface GetSidebarConnectionStatusResponse {
  serviceUrl: string
  connected: boolean
  status: 'connected' | 'not-configured' | 'connecting' | 'error'
  isLegaleseCloud: boolean
  error?: string
}

/** Sidebar asks extension for current connection status */
export const GetSidebarConnectionStatus: RequestType<
  void,
  GetSidebarConnectionStatusResponse
> = {
  method: 'getSidebarConnectionStatus',
}

/** Sidebar requests login flow */
export const RequestSidebarLogin: NotificationType<void> = {
  method: 'requestSidebarLogin',
}

/** Sidebar requests logout */
export const RequestSidebarLogout: NotificationType<void> = {
  method: 'requestSidebarLogout',
}

/** Deployment info derived from /openapi.json */
export interface SidebarDeploymentInfo {
  deploymentId: string
  functions: ExportedFunctionInfo[]
}

/** Sidebar requests list of deployments */
export const ListSidebarDeployments: RequestType<
  void,
  { deployments: SidebarDeploymentInfo[] }
> = {
  method: 'listSidebarDeployments',
}

/** Sidebar requests deploy */
export interface SidebarDeployParams {
  deploymentId: string
  fileUri: string
}

export interface SidebarDeployResponse {
  success: boolean
  deploymentId?: string
  error?: string
}

export const RequestSidebarDeploy: RequestType<
  SidebarDeployParams,
  SidebarDeployResponse
> = {
  method: 'requestSidebarDeploy',
}

/** Sidebar requests undeploy */
export interface SidebarUndeployParams {
  deploymentId: string
}

export const RequestSidebarUndeploy: RequestType<
  SidebarUndeployParams,
  { success: boolean; error?: string }
> = {
  method: 'requestSidebarUndeploy',
}

/** Sidebar requests deployment OpenAPI spec (for breaking change detection) */
export const GetSidebarDeploymentOpenApi: RequestType<
  { deploymentId: string },
  { openapi: unknown }
> = {
  method: 'getSidebarDeploymentOpenApi',
}

/** Sidebar asks extension to open the service URL in browser */
export const RequestOpenServiceUrl: NotificationType<void> = {
  method: 'requestOpenServiceUrl',
}

/** Sidebar asks extension to open Legalese Cloud Console */
export const RequestOpenConsole: NotificationType<void> = {
  method: 'requestOpenConsole',
}

/** Sidebar asks extension to disconnect (clear credentials + service URL) */
export const RequestDisconnect: NotificationType<void> = {
  method: 'requestDisconnect',
}

/** Sidebar asks extension to refresh deployments */
export const RequestRefreshDeployments: NotificationType<void> = {
  method: 'requestRefreshDeployments',
}

/** Sidebar asks extension to show a VSCode notification */
export interface ShowNotificationParams {
  type: 'info' | 'warning' | 'error'
  message: string
}

export const ShowNotification: NotificationType<ShowNotificationParams> = {
  method: 'showNotification',
}

/** Extension pushes connection status changes to sidebar */
export const SidebarConnectionStatusChanged: NotificationType<GetSidebarConnectionStatusResponse> =
  {
    method: 'sidebarConnectionStatusChanged',
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
