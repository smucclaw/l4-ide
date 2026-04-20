import * as vscode from 'vscode'
import * as path from 'path'
import { promises as fs } from 'fs'
import type {
  AiConversation,
  AiConversationSummary,
  AiChatMessage,
} from 'jl4-client-rpc'
import type { AiLogger } from './logger.js'

/**
 * File-backed local mirror of conversation state for UI restoration.
 * The ai-proxy owns the canonical LLM history; this store is just for
 * rendering and history navigation. Layout:
 *
 *   {globalStorageUri}/ai/conversations/{userKey}/{id}.json           — live
 *   {globalStorageUri}/ai/conversations/{userKey}/{id}.deleted-{ms}.json — tombstoned
 *
 * `userKey` is derived from the signed-in Legalese Cloud user id so a
 * logout + login as a different user yields an entirely different
 * conversation list, even on the same machine. Self-hosted / signed-
 * out sessions bucket under `anonymous`. Deletion renames; a future
 * cleanup job reaps tombstones.
 */
export class ConversationStore {
  private readonly rootDir: string

  constructor(
    context: vscode.ExtensionContext,
    private readonly logger: AiLogger,
    private readonly getUserKey: () => string
  ) {
    this.rootDir = path.join(
      context.globalStorageUri.fsPath,
      'ai',
      'conversations'
    )
  }

  /** Per-user subdirectory resolved on every call so user switches
   *  take effect immediately without requiring the store to be
   *  reconstructed. */
  private baseDir(): string {
    const key = this.getUserKey()
    const safe = /^[a-zA-Z0-9_-]+$/.test(key) ? key : 'anonymous'
    return path.join(this.rootDir, safe)
  }

  async list(): Promise<AiConversationSummary[]> {
    const dir = this.baseDir()
    const files = await this.safeReaddir(dir)
    const summaries: AiConversationSummary[] = []
    for (const f of files) {
      if (!f.endsWith('.json') || f.includes('.deleted-')) continue
      try {
        const raw = await fs.readFile(path.join(dir, f), 'utf-8')
        const conv = JSON.parse(raw) as AiConversation
        summaries.push({
          id: conv.id,
          title: conv.title,
          model: conv.model,
          createdAt: conv.createdAt,
          lastActiveAt: conv.lastActiveAt,
          messageCount: conv.messages.length,
        })
      } catch (err) {
        this.logger.warn(
          `conv-store: skipping unreadable ${f}: ${err instanceof Error ? err.message : String(err)}`
        )
      }
    }
    // Newest first
    summaries.sort((a, b) => b.lastActiveAt.localeCompare(a.lastActiveAt))
    return summaries
  }

  async load(id: string): Promise<AiConversation | null> {
    if (!isSafeId(id)) return null
    try {
      const raw = await fs.readFile(this.filePath(id), 'utf-8')
      return JSON.parse(raw) as AiConversation
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code === 'ENOENT') return null
      this.logger.warn(
        `conv-store: load failed for ${id}: ${err instanceof Error ? err.message : String(err)}`
      )
      return null
    }
  }

  async save(conv: AiConversation): Promise<void> {
    if (!isSafeId(conv.id)) {
      throw new Error(`Invalid conversation id: ${conv.id}`)
    }
    const dir = this.baseDir()
    await fs.mkdir(dir, { recursive: true })
    const p = this.filePath(conv.id)
    const tmp = `${p}.tmp-${Math.random().toString(36).slice(2, 10)}`
    try {
      await fs.writeFile(tmp, JSON.stringify(conv))
      await fs.rename(tmp, p)
    } catch (err) {
      await fs.unlink(tmp).catch(() => undefined)
      throw err
    }
  }

  /** Rename-delete; no unlink. */
  async delete(id: string): Promise<boolean> {
    if (!isSafeId(id)) return false
    const src = this.filePath(id)
    const dst = path.join(this.baseDir(), `${id}.deleted-${Date.now()}.json`)
    try {
      await fs.rename(src, dst)
      return true
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code === 'ENOENT') return false
      this.logger.warn(
        `conv-store: delete failed for ${id}: ${err instanceof Error ? err.message : String(err)}`
      )
      return false
    }
  }

  /**
   * Append messages to an existing conversation (or create a blank one).
   * Keeps lastActiveAt fresh. Callers should batch appends per turn to
   * avoid hammering the disk.
   */
  async appendMessages(
    id: string,
    orgId: string,
    userId: string,
    model: string,
    titleHint: string,
    messages: AiChatMessage[]
  ): Promise<AiConversation> {
    let conv = await this.load(id)
    const now = new Date().toISOString()
    if (!conv) {
      conv = {
        id,
        orgId,
        userId,
        model,
        title: titleHint.slice(0, 80),
        createdAt: now,
        lastActiveAt: now,
        messages: [],
      }
    }
    conv.messages.push(...messages)
    conv.lastActiveAt = now
    await this.save(conv)
    return conv
  }

  async setTitle(id: string, title: string): Promise<void> {
    const conv = await this.load(id)
    if (!conv) return
    conv.title = title
    await this.save(conv)
  }

  private filePath(id: string): string {
    return path.join(this.baseDir(), `${id}.json`)
  }

  private async safeReaddir(dir: string): Promise<string[]> {
    try {
      return await fs.readdir(dir)
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code === 'ENOENT') return []
      throw err
    }
  }
}

function isSafeId(id: string): boolean {
  return /^[a-zA-Z0-9_-]+$/.test(id) && id.length < 128
}
