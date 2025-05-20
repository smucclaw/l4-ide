import type {
  L4RpcRequestType,
  LspResponse,
  LadderBackendApi,
} from 'jl4-client-rpc'
import type { MonacoL4LanguageClient } from '$lib/monaco-l4-language-client'
import type { RenderAsLadderInfo } from '@repo/viz-expr'

/** Implementation of {@link LadderBackendApi} that is used by the Monaco/jl4-web version. */
export class LadderApiForMonaco implements LadderBackendApi {
  constructor(
    /** Internal language client for direct communication with the language server */
    private readonly languageClient: MonacoL4LanguageClient,

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
  ): Promise<LspResponse<R>> {
    return await this.languageClient.sendRequest(type, params)
  }

  /** Update the LadderFlow component */
  async updateViz(ladderInfo: RenderAsLadderInfo): Promise<void> {
    await this.makeLadderFlow(ladderInfo)
  }
}
