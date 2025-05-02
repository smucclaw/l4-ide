import type {
  L4RpcRequestType,
  LspResponse,
  LadderBackendApi,
} from 'jl4-client-rpc'
import type { MonacoL4LanguageClient } from '$lib/monaco-l4-language-client'

/** Implementation of {@link LadderBackendApi} that is used by the Monaco/jl4-web version. */
export class LadderApiForMonaco implements LadderBackendApi {
  constructor(
    /** Internal language client for direct communication with the language server */
    private readonly languageClient: MonacoL4LanguageClient
  ) {}

  async sendClientRequest<P extends object, R>(
    type: L4RpcRequestType<P, R>,
    params: P
  ): Promise<LspResponse<R>> {
    return await this.languageClient.sendRequest(type, params)
  }
}
