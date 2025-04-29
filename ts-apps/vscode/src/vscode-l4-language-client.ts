import { LanguageClient } from 'vscode-languageclient/node.js'
import {
  type LspResponse,
  L4LanguageClient,
  EvalAppRequestType,
  EvalAppRequestParams,
  EvalAppResult,
} from 'jl4-lsp-client'

export class VSCodeL4LanguageClient implements L4LanguageClient {
  /* TODO: Add a static `make` method that initializes the client and server options in the future, 
  so that the initialization would just be
  ```
  const client = VSCodeL4LanguageClient.make()
  ```
  instead of
  ```
  const client = new VSCodeL4LanguageClient(
    new VS.LanguageClient(langId, langName, serverOptions, clientOptions)
  )
  ```
  */

  constructor(
    // private readonly vsceContext: VS.ExtensionContext,
    private readonly client: LanguageClient
    // private readonly outputChannel: VS.OutputChannel
  ) {}

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
