import { MonacoLanguageClient } from 'monaco-languageclient'
import type { L4LanguageClient, LspResponse } from 'jl4-lsp-client'
import {
  EvalAppRequestType,
  EvalAppRequestParams,
  EvalAppResult,
} from 'jl4-lsp-client'

export class MonacoL4LanguageClient implements L4LanguageClient {
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
