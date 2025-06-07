import type { L4RpcRequestType, LadderBackendApi } from 'jl4-client-rpc'
import { makeLspRelayRequestType } from 'jl4-client-rpc'
import { Messenger } from 'vscode-messenger-webview'
import { HOST_EXTENSION } from 'vscode-messenger-common'
import type { RenderAsLadderInfo } from '@repo/viz-expr'

/** Implementation of {@link LadderBackendApi} that is used by the VSCode webview. */
export class LadderApiForWebview implements LadderBackendApi {
  constructor(
    /** Internal messenger for communication between VSCode extension and webview */
    private readonly messenger: Messenger,

    /** Helper to re-create the LadderFlow component.
     * This should be closed over LadderEnv.
     * This is basically the 'updating state in parent from child with a callback prop from parent' pattern */
    private readonly makeLadderFlow: (
      ladderInfo: RenderAsLadderInfo
    ) => Promise<void>
  ) {}

  async sendClientRequest<P extends object, R>(
    type: L4RpcRequestType<P, R>,
    params: P
  ): Promise<R | null> {
    return await this.messenger.sendRequest(
      makeLspRelayRequestType<P, R>(),
      HOST_EXTENSION,
      { requestType: type, params }
    )
  }

  /** Update the LadderFlow component */
  async updateViz(ladderInfo: RenderAsLadderInfo): Promise<void> {
    await this.makeLadderFlow(ladderInfo)
  }
}
