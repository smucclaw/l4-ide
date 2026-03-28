import { ExtensionContext, workspace, window } from 'vscode'
import * as vscode from 'vscode'
import * as path from 'path'
import * as fs from 'fs'
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
} from 'vscode-languageclient/node.js'
import { createConverter as createCodeConverter } from 'vscode-languageclient/lib/common/codeConverter.js'
import type { PanelConfig } from './webview-panel.js'
import { PanelManager } from './webview-panel.js'

import { VSCodeL4LanguageClient } from './vscode-l4-language-client.js'

import { RenderAsLadderInfo, VersionedDocId } from '@repo/viz-expr'
import { Schema } from 'effect'
import type { WebviewTypeMessageParticipant } from 'vscode-messenger-common'
import { Messenger } from 'vscode-messenger'
import { DecisionServiceQueryPlanRequest } from '@repo/vscode-webview-rpc'
import {
  RenderAsLadder,
  WebviewFrontendIsReadyNotification,
  AddInspectorResult,
  RemoveInspectorResult,
  SyncInspectorResults,
  ToggleSimplify,
  EvalDirectiveResultRequestType,
  makeLspRelayRequestType,
  type DirectiveResult,
  type SrcPos,
} from 'jl4-client-rpc'
import {
  fetchQueryPlan,
  upsertFunctionFromSource,
  type DecisionServiceClient,
} from './decision-service-client.js'
import { cmdRenderResult } from './commands.js'
import {
  SidebarProvider,
  SIDEBAR_WEBVIEW_TYPE,
  initializeSidebarMessenger,
} from './sidebar-provider.js'
import { AuthManager } from './auth.js'
import { ServiceClient } from './service-client.js'

/***********************************************
     decode for RenderAsLadderInfo
     (aka the payload from lang server)
***********************************************/

const decode = Schema.decodeUnknownSync(RenderAsLadderInfo)

/***************************************
      Language Client
****************************************/

let client: VSCodeL4LanguageClient
const code2ProtocolConverter = createCodeConverter()

/***************************************
       Webview Panel
****************************************/

const PANEL_CONFIG: PanelConfig = {
  viewType: 'l4Viz',
  title: 'L4 Decision Graph',
  position: vscode.ViewColumn.Beside,
}

const INSPECTOR_PANEL_CONFIG: PanelConfig = {
  viewType: 'l4Inspector',
  title: 'L4 Inspector',
  position: vscode.ViewColumn.Beside,
  htmlSubpath: 'inspector',
  onDispose: async () => {
    // No special cleanup needed for inspector
  },
}

const vizWebviewFrontend: WebviewTypeMessageParticipant = {
  type: 'webview',
  webviewType: 'l4Viz',
}

const inspectorWebviewFrontend: WebviewTypeMessageParticipant = {
  type: 'webview',
  webviewType: 'l4Inspector',
}

/***************************************
      Set up webview messenger
****************************************/

/** Stored args from the last successful l4.visualize invocation, used for simplify toggle */
let lastVizArgs: unknown[] | null = null

function initializeWebviewMessenger(
  outputChannel: vscode.OutputChannel,
  panelManager: PanelManager
) {
  /** Messenger for VSCode extension to communicate with webview */
  const webviewMessenger = new Messenger({ debugLog: true })
  const lastUpsertByDocUri = new Map<
    string,
    { version: number; fnName: string }
  >()

  // Set up listeners
  // -- Listen for whether webview frontend has initialized
  webviewMessenger.onNotification(WebviewFrontendIsReadyNotification, () => {
    panelManager.markFrontendAsReady()
    outputChannel.appendLine(`Ext: got frontend is ready notification!`)
  })

  // -- Listen for LSP client relay requests from webview
  webviewMessenger.onRequest(
    makeLspRelayRequestType<object, unknown>(),
    async (clientReqParams) => {
      outputChannel.appendLine(
        `Ext: Received LSP relay request from webview:\n${JSON.stringify(clientReqParams)}`
      )
      const response = await client.sendRequest(
        clientReqParams.requestType,
        clientReqParams.params
      )
      outputChannel.appendLine(
        `Response from server:\n${JSON.stringify(response)}`
      )
      outputChannel.appendLine(
        '--------------------------------------------------'
      )
      return response
    }
  )

  // -- Listen for decision-service query-plan requests from webview
  webviewMessenger.onRequest(
    DecisionServiceQueryPlanRequest,
    async (params) => {
      const decisionServiceUrl: string =
        workspace.getConfiguration('jl4').get('decisionServiceUrl') ??
        'http://localhost:8001'
      const client: DecisionServiceClient = { baseUrl: decisionServiceUrl }

      const docUri = vscode.Uri.parse(params.docUri)
      const existing = vscode.workspace.textDocuments.find(
        (d) => d.uri.toString() === params.docUri
      )
      const doc = existing ?? (await vscode.workspace.openTextDocument(docUri))

      const prev = lastUpsertByDocUri.get(params.docUri)
      if (
        !prev ||
        prev.version !== doc.version ||
        prev.fnName !== params.fnName
      ) {
        outputChannel.appendLine(
          `Ext: upserting decision-service function ${params.fnName} from ${params.docUri} v${doc.version}`
        )
        await upsertFunctionFromSource(client, params.fnName, doc.getText())
        lastUpsertByDocUri.set(params.docUri, {
          version: doc.version,
          fnName: params.fnName,
        })
      }

      return await fetchQueryPlan(client, params.fnName, params.bindings)
    }
  )

  // -- Listen for simplify toggle from webview
  webviewMessenger.onNotification(ToggleSimplify, async (msg) => {
    outputChannel.appendLine(`Ext: Toggle simplify to ${msg.shouldSimplify}`)
    if (lastVizArgs && lastVizArgs.length >= 3) {
      // Replay the last visualization with the new simplify flag
      const newArgs = [...lastVizArgs]
      newArgs[2] = msg.shouldSimplify
      await vscode.commands.executeCommand('l4.visualize', ...newArgs)
    }
  })

  return webviewMessenger
}

/***************************************
      Set up inspector messenger
****************************************/

function initializeInspectorMessenger(
  outputChannel: vscode.OutputChannel,
  panelManager: PanelManager,
  onSectionRemoved: (directiveId: string) => void
) {
  const inspectorMessenger = new Messenger({ debugLog: true })

  inspectorMessenger.onNotification(WebviewFrontendIsReadyNotification, () => {
    panelManager.markFrontendAsReady()
    outputChannel.appendLine(`Ext: Inspector frontend is ready`)
  })

  inspectorMessenger.onNotification(RemoveInspectorResult, (msg) => {
    onSectionRemoved(msg.directiveId)
  })

  return inspectorMessenger
}

/***************************************
      Find bundled binary
****************************************/

/**
 * Finds the jl4-lsp executable to use.
 * Priority:
 * 1. User-configured path via jl4.serverExecutablePath setting
 * 2. Bundled binary in the extension (platform-specific)
 * 3. Fall back to 'jl4-lsp' on PATH
 */
function findServerExecutable(
  context: ExtensionContext,
  outputChannel: vscode.OutputChannel
): string {
  // Check user configuration first
  const configuredPath: string | undefined = workspace
    .getConfiguration('jl4')
    .get('serverExecutablePath')

  if (configuredPath && configuredPath.trim() !== '') {
    outputChannel.appendLine(
      `[client] Using configured server path: ${configuredPath}`
    )
    return configuredPath
  }

  // Try to find bundled binary
  const bundledPath = findBundledBinary(context, outputChannel)
  if (bundledPath) {
    outputChannel.appendLine(
      `[client] Using bundled server binary: ${bundledPath}`
    )
    return bundledPath
  }

  // Fall back to PATH
  outputChannel.appendLine(
    '[client] No bundled binary found, falling back to jl4-lsp on PATH'
  )
  return 'jl4-lsp'
}

/**
 * Looks for a bundled jl4-lsp binary in the extension directory.
 * The binary should be in: <extension>/bin/<platform>-<arch>/jl4-lsp[.exe]
 *
 * Supported platforms:
 * - darwin-arm64 (macOS Apple Silicon - also works on Intel Macs via Rosetta 2)
 * - win32-x64 (Windows x64)
 * - linux-x64 (Linux x64)
 * - linux-arm64 (Linux ARM64)
 */
function findBundledBinary(
  context: ExtensionContext,
  outputChannel: vscode.OutputChannel
): string | undefined {
  const platform = process.platform
  const arch = process.arch

  // Map Node.js platform/arch to our naming convention
  const platformArch = `${platform}-${arch}`
  const exeName = platform === 'win32' ? 'jl4-lsp.exe' : 'jl4-lsp'

  // Look in the extension's bin directory
  const binPath = path.join(context.extensionPath, 'bin', platformArch, exeName)

  outputChannel.appendLine(`[client] Looking for bundled binary at: ${binPath}`)

  if (fs.existsSync(binPath)) {
    // Ensure the binary is executable (on Unix-like systems)
    if (platform !== 'win32') {
      try {
        fs.accessSync(binPath, fs.constants.X_OK)
      } catch {
        outputChannel.appendLine(
          `[client] Binary found but not executable: ${binPath}`
        )
        return undefined
      }
    }
    return binPath
  }

  outputChannel.appendLine(
    `[client] No bundled binary found for platform: ${platformArch}`
  )
  return undefined
}

/***************************************
      Activate
****************************************/

export async function activate(context: ExtensionContext) {
  const langId = 'l4'
  const langName = 'jl4 LSP'
  const outputChannel: vscode.OutputChannel = window.createOutputChannel(
    langName,
    langId
  )

  const serverCmd = findServerExecutable(context, outputChannel)
  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  const serverOptions: ServerOptions = {
    run: { command: serverCmd },
    debug: {
      command: serverCmd,
    },
  }

  // Initialize panelManager and webviewMessenger
  const panelManager = new PanelManager(PANEL_CONFIG)
  const inspectorPanelManager = new PanelManager(INSPECTOR_PANEL_CONFIG)
  const webviewMessenger = initializeWebviewMessenger(
    outputChannel,
    panelManager
  )

  // Track open inspector sections so we can push live updates when the file changes.
  // Key: directiveId (e.g. "file:///foo.l4:42:1")
  // Value: info needed to re-request the result from the LSP
  const openInspectorSections = new Map<
    string,
    { uri: string; srcPos: SrcPos; directiveType: string; lineContent: string }
  >()

  const inspectorMessenger = initializeInspectorMessenger(
    outputChannel,
    inspectorPanelManager,
    (directiveId) => openInspectorSections.delete(directiveId)
  )

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: langId, pattern: '**/*' }],
    diagnosticCollectionName: langName,
    revealOutputChannelOn: RevealOutputChannelOn.Never,
    outputChannel,
    outputChannelName: langName,
    middleware: {
      executeCommand: async (command, args, next) => {
        const editor = vscode.window.activeTextEditor
        if (editor) {
          // why do we copy the args via .slice(0)?
          // No clue, the sample does it.
          // Maybe to avoid accidental mutation?
          if (args.length === 0) {
            args = args.slice(0)
            args.push(editor.document.uri.toString())
          }

          outputChannel.appendLine('')
          outputChannel.appendLine(
            '<executeCommand>--------------------------------------------------'
          )
          outputChannel.appendLine(`called with args\n${JSON.stringify(args)}`)
          outputChannel.appendLine('')

          const responseFromLangServer: unknown = await next(command, args)

          if (responseFromLangServer === null) {
            outputChannel.appendLine(
              'language server returned null, doing nothing'
            )
            return
          }

          outputChannel.appendLine('')
          outputChannel.appendLine(
            `Received command response\n${JSON.stringify(responseFromLangServer)}`
          )
          outputChannel.appendLine('')

          const ladderInfo: RenderAsLadderInfo = decode(responseFromLangServer)
          lastVizArgs = args

          panelManager.render(context, editor.document.uri)
          webviewMessenger.registerWebviewPanel(panelManager.getPanel())
          await panelManager.getWebviewFrontendIsReadyPromise()

          const response = await webviewMessenger.sendRequest(
            RenderAsLadder,
            vizWebviewFrontend,
            ladderInfo
          )
          if (response.$type === 'error') {
            outputChannel.appendLine(`Error in render viz in webview request`)
          } else if (response.$type === 'ok') {
            outputChannel.appendLine(
              `Request to render viz in webview succeeded`
            )
          }
        }

        outputChannel.appendLine(
          '--------------------------------------------------</executeCommand>'
        )
        // TODO: else show pop up to client
      },
      didChange: async (event, next) => {
        // we sent a visualisation command whenever we change any code
        // we do this after invoking the callback to avoid blocking the editor
        // on the command invokation
        await next(event)

        const verDocId: VersionedDocId =
          code2ProtocolConverter.asVersionedTextDocumentIdentifier(
            event.document
          )
        await vscode.commands.executeCommand('l4.visualize', verDocId)
      },
    },
  }

  outputChannel.appendLine(
    `[client] Starting server from the client: ${serverCmd}`
  )

  // Create the language client and start the client.
  client = new VSCodeL4LanguageClient(
    new LanguageClient(langId, langName, serverOptions, clientOptions)
  )

  // Register the l4.renderResult command
  context.subscriptions.push(
    vscode.commands.registerCommand(
      cmdRenderResult,
      async (
        verDocId: { uri: string; version: number },
        srcPos: SrcPos,
        directiveType: string
      ) => {
        outputChannel.appendLine(
          `[inspector] Track result: ${directiveType} at line ${srcPos.line}`
        )

        try {
          // Send custom LSP request to get the directive result
          const result = await client.sendRequest(
            EvalDirectiveResultRequestType,
            { verDocId, srcPos, directiveType }
          )

          if (!result) {
            outputChannel.appendLine(
              `[inspector] No result returned from server`
            )
            return
          }

          const directiveId = `${verDocId.uri}:${srcPos.line}:${srcPos.column}`
          const editor = vscode.window.activeTextEditor
          const lineContent =
            editor?.document.lineAt(srcPos.line - 1).text ?? ''

          // Track the section so it receives live updates when the file changes
          openInspectorSections.set(directiveId, {
            uri: verDocId.uri,
            srcPos,
            directiveType,
            lineContent,
          })

          // Create/reveal the inspector panel
          if (editor) {
            inspectorPanelManager.render(context, editor.document.uri)
          } else {
            inspectorPanelManager.render(
              context,
              vscode.Uri.parse(verDocId.uri)
            )
          }
          inspectorMessenger.registerWebviewPanel(
            inspectorPanelManager.getPanel()
          )
          await inspectorPanelManager.getWebviewFrontendIsReadyPromise()

          // Send the result to the inspector webview
          const response = await inspectorMessenger.sendRequest(
            AddInspectorResult,
            inspectorWebviewFrontend,
            {
              directiveId,
              srcPos,
              result: result as DirectiveResult,
              lineContent,
            }
          )

          if (response.$type === 'scrolled') {
            outputChannel.appendLine(`[inspector] Scrolled to existing result`)
          } else {
            outputChannel.appendLine(`[inspector] Added new result section`)
          }
        } catch (err) {
          outputChannel.appendLine(
            `[inspector] Error: ${err instanceof Error ? err.message : String(err)}`
          )
        }
      }
    )
  )

  // Refresh token colors in webviews when the user changes their color theme
  context.subscriptions.push(
    vscode.window.onDidChangeActiveColorTheme(() => {
      panelManager.refreshTokenColors()
      inspectorPanelManager.refreshTokenColors()
    })
  )

  // Initialize auth + service client
  const auth = new AuthManager(context.secrets, outputChannel)
  const serviceClient = new ServiceClient(auth)

  // Register URI handler for legalese.cloud login callback
  context.subscriptions.push(
    vscode.window.registerUriHandler({
      handleUri: (uri) => auth.handleAuthCallback(uri),
    })
  )
  context.subscriptions.push(auth)

  // Initialize sidebar
  const sidebarMessenger = new Messenger({ debugLog: true })
  initializeSidebarMessenger(
    sidebarMessenger,
    client,
    auth,
    serviceClient,
    outputChannel
  )

  const sidebarProvider = new SidebarProvider(
    context,
    client,
    sidebarMessenger,
    auth,
    serviceClient,
    outputChannel
  )
  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(
      SIDEBAR_WEBVIEW_TYPE,
      sidebarProvider,
      { webviewOptions: { retainContextWhenHidden: true } }
    )
  )

  // Refresh sidebar token colors on theme change (alongside panels)
  context.subscriptions.push(
    vscode.window.onDidChangeActiveColorTheme(() => {
      sidebarProvider.refreshTokenColors()
    })
  )

  // Push active L4 file to sidebar when editor changes
  function notifySidebarActiveFile(editor: vscode.TextEditor | undefined) {
    if (editor && editor.document.languageId === 'l4') {
      sidebarProvider.notifyActiveFile(
        editor.document.uri.toString(),
        editor.document.version
      )
    } else {
      sidebarProvider.clearActiveFile()
    }
  }

  // When the sidebar webview first loads, send the current active file
  sidebarMessenger.onNotification(WebviewFrontendIsReadyNotification, () => {
    outputChannel.appendLine('[sidebar] Webview is ready')
    notifySidebarActiveFile(vscode.window.activeTextEditor)
    sidebarProvider.refreshTokenColors()
  })

  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor(notifySidebarActiveFile)
  )

  // Start the client. This will also launch the server
  await client.start()

  // After evaluation completes, the LSP sends l4/directiveResultsUpdated with all
  // current directive results. Forward them to the inspector webview as SyncInspectorResults.
  // This replaces the old onDidChangeDiagnostics + per-directive request approach,
  // eliminating the race condition where evaluation hadn't completed yet.
  context.subscriptions.push(
    client.onNotification(
      'l4/directiveResultsUpdated',
      (params: {
        uri: string
        results: Array<{
          directiveId: string
          prettyText: string
          success: boolean | null
          lineContent: string
        }>
      }) => {
        // Refresh sidebar — the LSP just finished compiling this file,
        // so exported functions may have changed
        const editor = vscode.window.activeTextEditor
        if (editor && editor.document.uri.toString() === params.uri) {
          notifySidebarActiveFile(editor)
        }

        // Only update inspector if the panel is open
        const panel = inspectorPanelManager.getPanel()
        if (!panel) return

        // Prepend the URI so directiveIds match the VS Code webview's "uri:line:col" format
        const results = params.results.map((r) => ({
          ...r,
          directiveId: `${params.uri}:${r.directiveId}`,
        }))

        inspectorMessenger.sendNotification(
          SyncInspectorResults,
          inspectorWebviewFrontend,
          {
            uri: params.uri,
            results,
          }
        )
      }
    )
  )
}

export async function deactivate(): Promise<void> {
  if (!client) {
    return undefined
  }
  await client.dispose()
}
