import { MonacoLanguageClient } from 'monaco-languageclient'
import type {
  L4LanguageClient,
  L4RpcRequestType,
  LspResponse,
} from 'jl4-lsp-client'

export class MonacoL4LanguageClient implements L4LanguageClient {
  // I don't like this ctor, for reasons outlined in the VSCode version; it's just the pragmatic thing to do right now.
  constructor(private readonly client: MonacoLanguageClient) {}

  async start(): Promise<void> {
    return this.client.start()
  }

  async sendEvalAppRequest(
    params: EvalAppRequestParams
  ): Promise<LspResponse<EvalAppResult>> {
    return this.client.sendRequest(EvalAppRequestType, params)
  }

  async dispose(): Promise<void> {
    await this.client.dispose()
  }
}
