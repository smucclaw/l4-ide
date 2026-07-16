// Standalone chat-panel preferences for webchat.
//
// The extension persists these via VSCode settings sync over RPC; here there is
// no extension host, so we keep them in localStorage. `init()` accepts (and
// ignores) a messenger accessor to stay signature-compatible with the copied
// store/components, which call `aiPrefs.init(() => messenger)`.

const STORAGE_KEY = 'webchat:prefs'

interface Prefs {
  showReasoning: boolean
}

const DEFAULTS: Prefs = {
  showReasoning: false,
}

function load(): Prefs {
  if (typeof localStorage === 'undefined') return { ...DEFAULTS }
  try {
    const raw = localStorage.getItem(STORAGE_KEY)
    if (!raw) return { ...DEFAULTS }
    return { ...DEFAULTS, ...(JSON.parse(raw) as Partial<Prefs>) }
  } catch {
    return { ...DEFAULTS }
  }
}

class AiPrefs {
  showReasoning = $state(DEFAULTS.showReasoning)

  init(): void {
    const p = load()
    this.showReasoning = p.showReasoning
  }

  private persist(): void {
    if (typeof localStorage === 'undefined') return
    try {
      localStorage.setItem(
        STORAGE_KEY,
        JSON.stringify({ showReasoning: this.showReasoning })
      )
    } catch {
      // ignore quota / privacy-mode failures
    }
  }

  setShowReasoning(next: boolean): void {
    this.showReasoning = next
    this.persist()
  }
}

export const aiPrefs = new AiPrefs()
