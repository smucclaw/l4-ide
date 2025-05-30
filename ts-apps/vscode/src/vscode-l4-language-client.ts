import { LanguageClient } from 'vscode-languageclient/node.js'
import { type L4RpcRequestType, L4LanguageClient } from 'jl4-client-rpc'

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

  async sendRequest<P extends object, R>(
    type: L4RpcRequestType<P, R>,
    params: P
  ): Promise<R | null> {
    return this.client.sendRequest(type.method, params)
  }

  async dispose(): Promise<void> {
    await this.client.dispose()
  }
}
