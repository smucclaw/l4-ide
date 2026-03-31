import * as http from 'node:http'
import * as vscode from 'vscode'
import type { AuthManager } from './auth.js'

/**
 * Local MCP proxy server.
 *
 * Runs on localhost, accepts unauthenticated MCP JSON-RPC requests, and
 * forwards them to the remote jl4-service with the user's credentials.
 * This lets AI tools (Copilot, Cursor, Claude Desktop) access L4 rules
 * without needing their own auth configuration.
 */
export class McpProxy implements vscode.Disposable {
  private server: http.Server | null = null
  private port: number = 0
  private outputChannel: vscode.OutputChannel
  private mcpRegistration: vscode.Disposable | undefined

  constructor(
    private readonly auth: AuthManager,
    outputChannel: vscode.OutputChannel
  ) {
    this.outputChannel = outputChannel
  }

  /**
   * Check if the remote jl4-service has MCP support and start/stop
   * the local proxy accordingly. Call this whenever deployments change.
   */
  async refresh(): Promise<void> {
    const serviceUrl = this.auth.getEffectiveServiceUrl()
    if (!serviceUrl) {
      this.stop()
      return
    }

    try {
      const headers = await this.auth.getAuthHeaders()
      const resp = await fetch(`${serviceUrl}/.well-known/mcp/manifest`, {
        headers,
      })
      if (resp.ok) {
        if (!this.server) {
          await this.start()
        }
      } else {
        this.stop()
      }
    } catch {
      this.stop()
    }
  }

  /** Start the local HTTP server. */
  private async start(): Promise<void> {
    if (this.server) return

    this.server = http.createServer((req, res) => {
      this.handleRequest(req, res)
    })

    await new Promise<void>((resolve, reject) => {
      this.server!.listen(0, '127.0.0.1', () => {
        const addr = this.server!.address()
        if (addr && typeof addr === 'object') {
          this.port = addr.port
        }
        resolve()
      })
      this.server!.on('error', reject)
    })

    this.outputChannel.appendLine(
      `[mcp-proxy] Local MCP server started on http://127.0.0.1:${this.port}/.mcp`
    )

    // Register with VS Code's MCP system so Copilot and other
    // VS Code AI extensions can discover the server automatically.
    try {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const lm = vscode.lm as any
      if (typeof lm?.registerMcpServerDefinition === 'function') {
        this.mcpRegistration = lm.registerMcpServerDefinition({
          label: 'L4 Legal Rules',
          url: `http://127.0.0.1:${this.port}/.mcp`,
        })
        this.outputChannel.appendLine('[mcp-proxy] Registered with VS Code MCP')
      }
    } catch {
      // API not available in this VS Code version — proxy still works via URL
    }
  }

  /** Stop the local HTTP server. */
  stop(): void {
    if (!this.server) return
    this.mcpRegistration?.dispose()
    this.mcpRegistration = undefined
    this.server.close()
    this.server = null
    this.port = 0
    this.outputChannel.appendLine('[mcp-proxy] Local MCP server stopped')
  }

  /** The local MCP endpoint URL, or undefined if not running. */
  getLocalUrl(): string | undefined {
    if (!this.server || !this.port) return undefined
    return `http://127.0.0.1:${this.port}/.mcp`
  }

  /** Handle an incoming HTTP request. */
  private handleRequest(
    req: http.IncomingMessage,
    res: http.ServerResponse
  ): void {
    // Only accept POST /.mcp
    if (req.method !== 'POST' || !req.url?.startsWith('/.mcp')) {
      res.writeHead(404, { 'Content-Type': 'application/json' })
      res.end(JSON.stringify({ error: 'Not found' }))
      return
    }

    const chunks: Buffer[] = []
    req.on('data', (chunk: Buffer) => chunks.push(chunk))
    req.on('end', () => {
      const body = Buffer.concat(chunks).toString('utf-8')
      this.forward(body, req.url ?? '/.mcp')
        .then((result) => {
          res.writeHead(200, { 'Content-Type': 'application/json' })
          res.end(result)
        })
        .catch((err) => {
          this.outputChannel.appendLine(
            `[mcp-proxy] Forward error: ${err instanceof Error ? err.message : String(err)}`
          )
          res.writeHead(502, { 'Content-Type': 'application/json' })
          res.end(
            JSON.stringify({
              jsonrpc: '2.0',
              error: { code: -32603, message: 'Failed to reach service' },
            })
          )
        })
    })
  }

  /** Forward a JSON-RPC request to the remote jl4-service. */
  private async forward(body: string, path: string): Promise<string> {
    const serviceUrl = this.auth.getEffectiveServiceUrl()
    if (!serviceUrl) {
      throw new Error('No service URL')
    }

    const headers = await this.auth.getAuthHeaders()
    const resp = await fetch(`${serviceUrl}${path}`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        ...headers,
      },
      body,
    })

    return resp.text()
  }

  dispose(): void {
    this.stop()
  }
}
