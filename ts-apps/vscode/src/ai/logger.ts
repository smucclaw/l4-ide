import * as vscode from 'vscode'

/**
 * Dedicated output channel for the Legalese AI tab. Kept separate from
 * the language server's channel so a noisy agent loop doesn't drown out
 * LSP debug output.
 */
export class AiLogger {
  private readonly channel: vscode.OutputChannel

  constructor(name = 'Legalese AI') {
    this.channel = vscode.window.createOutputChannel(name)
    // Immediately log so the channel appears in the Output dropdown;
    // some VSCode versions omit empty channels from the list.
    this.channel.appendLine(
      `[info] Legalese AI output channel ready (${new Date().toISOString()})`
    )
  }

  /** Reveal the output channel in the Output panel. */
  show(): void {
    this.channel.show(true)
  }

  info(message: string): void {
    this.channel.appendLine(`[info] ${message}`)
  }

  warn(message: string): void {
    this.channel.appendLine(`[warn] ${message}`)
  }

  error(message: string, err?: unknown): void {
    const detail = err instanceof Error ? err.message : err ? String(err) : ''
    this.channel.appendLine(`[error] ${message}${detail ? `: ${detail}` : ''}`)
  }

  debug(message: string): void {
    this.channel.appendLine(`[debug] ${message}`)
  }

  dispose(): void {
    this.channel.dispose()
  }
}
