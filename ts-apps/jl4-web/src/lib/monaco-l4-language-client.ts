import { MonacoLanguageClient } from 'monaco-languageclient'
import type { L4LanguageClient, L4RpcRequestType } from 'jl4-client-rpc'

export class MonacoL4LanguageClient implements L4LanguageClient {
  // I don't like this ctor, for reasons outlined in the VSCode version; it's just the pragmatic thing to do right now.
  constructor(private readonly client: MonacoLanguageClient) {}

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
