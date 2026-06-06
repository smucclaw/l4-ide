// localStorage-backed conversation INDEX. The browser only keeps a
// lightweight summary per conversation (id, title, model, timestamps,
// deployment) so the sidebar can list history offline; the full
// transcript is the server's responsibility and is fetched on demand
// (GET /v1/conversations/{id}) when a conversation is opened. History is
// scoped to a single deployment.

import type { AiConversationSummary, AiChatMessage } from 'jl4-client-rpc'

const CONV_PREFIX = 'webchat:conv:'

/** One stored index row. Superset of `AiConversationSummary` with the
 *  fields the sidebar needs; never holds message bodies. */
export interface ConversationIndexEntry {
  id: string
  title: string
  model: string
  createdAt: string
  lastActiveAt: string
  messageCount: number
  deploymentId?: string
}

function key(id: string): string {
  return `${CONV_PREFIX}${id}`
}

export function loadIndexEntry(id: string): ConversationIndexEntry | null {
  if (typeof localStorage === 'undefined') return null
  try {
    const raw = localStorage.getItem(key(id))
    return raw ? (JSON.parse(raw) as ConversationIndexEntry) : null
  } catch {
    return null
  }
}

export function saveIndexEntry(entry: ConversationIndexEntry): void {
  if (typeof localStorage === 'undefined') return
  try {
    localStorage.setItem(key(entry.id), JSON.stringify(entry))
  } catch {
    // ignore quota / privacy-mode failures
  }
}

/** Shallow-merge a patch onto an existing entry (e.g. bump
 *  `lastActiveAt` / `model` after a turn). No-op if the entry is gone. */
export function updateIndexEntry(
  id: string,
  patch: Partial<ConversationIndexEntry>
): void {
  const existing = loadIndexEntry(id)
  if (!existing) return
  saveIndexEntry({ ...existing, ...patch, id: existing.id })
}

export function deleteIndexEntry(id: string): void {
  if (typeof localStorage === 'undefined') return
  try {
    localStorage.removeItem(key(id))
  } catch {
    // ignore
  }
}

/** Re-key an index entry (used when adopting the server-assigned id
 *  after the first turn's `metadata` frame). */
export function renameIndexEntry(fromId: string, toId: string): void {
  if (fromId === toId) return
  const entry = loadIndexEntry(fromId)
  if (!entry) return
  entry.id = toId
  saveIndexEntry(entry)
  deleteIndexEntry(fromId)
}

/** Summaries for the sidebar, newest first, scoped to one deployment. */
export function listSummaries(deploymentId: string): AiConversationSummary[] {
  if (typeof localStorage === 'undefined') return []
  const out: AiConversationSummary[] = []
  for (let i = 0; i < localStorage.length; i++) {
    const k = localStorage.key(i)
    if (!k || !k.startsWith(CONV_PREFIX)) continue
    try {
      const entry = JSON.parse(
        localStorage.getItem(k) as string
      ) as ConversationIndexEntry
      if (entry.deploymentId !== deploymentId) continue
      out.push({
        id: entry.id,
        title: entry.title || 'Untitled',
        model: entry.model,
        createdAt: entry.createdAt,
        lastActiveAt: entry.lastActiveAt,
        messageCount: entry.messageCount ?? 0,
        ...(entry.deploymentId ? { deploymentId: entry.deploymentId } : {}),
      })
    } catch {
      // skip malformed entries
    }
  }
  out.sort((a, b) => b.lastActiveAt.localeCompare(a.lastActiveAt))
  return out
}

/** First-user-message title, capped — mirrors the extension's fallback title. */
export function deriveTitle(messages: AiChatMessage[]): string {
  const firstUser = messages.find((m) => m.role === 'user')
  if (!firstUser) return 'New chat'
  const text =
    typeof firstUser.content === 'string'
      ? firstUser.content
      : Array.isArray(firstUser.content)
        ? (firstUser.content.find(
            (p): p is { type: 'text'; text: string } => p.type === 'text'
          )?.text ?? '')
        : ''
  const trimmed = text.trim().replace(/\s+/g, ' ')
  return trimmed.length > 80
    ? `${trimmed.slice(0, 80)}…`
    : trimmed || 'New chat'
}
