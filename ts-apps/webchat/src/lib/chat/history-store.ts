// localStorage-backed conversation store. This is the browser stand-in for the
// extension's globalStorage conversation-store + the server's EFS persistence:
// the full transcript lives client-side keyed by conversation id, while the
// server keeps its own copy keyed by the same id (so continuing a thread
// resumes server context). History is scoped to a single deployment.

import type {
  AiConversation,
  AiConversationSummary,
  AiChatMessage,
} from 'jl4-client-rpc'

const CONV_PREFIX = 'webchat:conv:'

function key(id: string): string {
  return `${CONV_PREFIX}${id}`
}

export function loadConversation(id: string): AiConversation | null {
  if (typeof localStorage === 'undefined') return null
  try {
    const raw = localStorage.getItem(key(id))
    return raw ? (JSON.parse(raw) as AiConversation) : null
  } catch {
    return null
  }
}

export function saveConversation(conv: AiConversation): void {
  if (typeof localStorage === 'undefined') return
  try {
    localStorage.setItem(key(conv.id), JSON.stringify(conv))
  } catch {
    // ignore quota / privacy-mode failures
  }
}

export function deleteConversation(id: string): void {
  if (typeof localStorage === 'undefined') return
  try {
    localStorage.removeItem(key(id))
  } catch {
    // ignore
  }
}

/** Re-key a conversation (used when adopting the server-assigned id after the
 *  first turn's `metadata` frame). */
export function renameConversation(fromId: string, toId: string): void {
  if (fromId === toId) return
  const conv = loadConversation(fromId)
  if (!conv) return
  conv.id = toId
  saveConversation(conv)
  deleteConversation(fromId)
}

/** Summaries for the sidebar, newest first, scoped to one deployment. */
export function listSummaries(deploymentId: string): AiConversationSummary[] {
  if (typeof localStorage === 'undefined') return []
  const out: AiConversationSummary[] = []
  for (let i = 0; i < localStorage.length; i++) {
    const k = localStorage.key(i)
    if (!k || !k.startsWith(CONV_PREFIX)) continue
    try {
      const conv = JSON.parse(
        localStorage.getItem(k) as string
      ) as AiConversation
      if (conv.deploymentId !== deploymentId) continue
      out.push({
        id: conv.id,
        title: conv.title || 'Untitled',
        model: conv.model,
        createdAt: conv.createdAt,
        lastActiveAt: conv.lastActiveAt,
        messageCount: conv.messages.length,
        ...(conv.deploymentId ? { deploymentId: conv.deploymentId } : {}),
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
