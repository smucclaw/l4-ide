import type { L4RpcRequestType } from './custom-protocol.js'
import { IAmDisposable } from './utils.js'

/**
 * A wrapper around the relevant IDE's language client, for the L4 language.
 *
 * Examples of other PL LSPs that similarly put the relevant IDE's language client in a wrapper object:
 * - https://github.com/leanprover/vscode-lean4/blob/44bf3e6ce3a151ab457c061e16be2508066a518d/vscode-lean4/src/leanclient.ts#L77
 * - https://github.com/Shopify/ruby-lsp/blob/b2a3ac3279418b87df32eac4c9eb49a8b212ac0e/vscode/src/client.ts#L350
 * - https://github.com/Dart-Code/Dart-Code/blob/6baf7cad43849bbb4e50fba1f96351a20534c4bd/src/extension/analysis/analyzer.ts#L38
 *
 * What we have now is quite simple. As the above examples demonstrate, there is a lot more that can be done with this wrapper object.
 *
 * We use a lower-level `sendRequest` method here,
 * but wrap over it with higher-level methods in interfaces like {@link L4BackendConnection}, which the frontend will use.
 * I.e., in L4BackendConnection we won't have a `sendClientRequest` method;
 * we'll instead just have higher-level wrappers like `evalApp`.
 * These higher-level wrappers will ultimately end up calling
 * `L4LanguageClient.sendRequest`; but the lower-level primitive won't be exposed in L4BackendConnection, and hence won't be exposed to the frontend.
 */
export interface L4LanguageClient extends IAmDisposable {
  start(): Promise<void>

  /** Make a request to the LSP server. */
  sendRequest<P extends object, R>(
    type: L4RpcRequestType<P, R>,
    params: P
  ): Promise<R | null>
}
