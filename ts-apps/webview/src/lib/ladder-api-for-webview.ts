import type {
  L4RpcRequestType,
  LspResponse,
  LadderBackendApi,
} from 'jl4-client-rpc'
import { makeLspRelayRequestType } from 'jl4-client-rpc'
import { Messenger } from 'vscode-messenger-webview'
import { HOST_EXTENSION } from 'vscode-messenger-common'

/** Implementation of {@link LadderBackendApi} that is used by the VSCode webview. */
export class LadderApiForWebview implements LadderBackendApi {
  constructor(
    /** Internal messenger for communication between VSCode extension and webview */
    private readonly messenger: Messenger
  ) {}

  async sendClientRequest<P extends object, R>(
    type: L4RpcRequestType<P, R>,
    params: P
  ): Promise<LspResponse<R>> {
    return await this.messenger.sendRequest(
      makeLspRelayRequestType<P, R>(),
      HOST_EXTENSION,
      { requestType: type, params }
    )
  }
}
