import { HOST_EXTENSION } from 'vscode-messenger-common'
import type { Messenger } from 'vscode-messenger-webview'
import {
  AiPreferencesGet,
  AiPreferencesSet,
  type AiPreferences,
} from 'jl4-client-rpc'

// Chat-panel UI prefs. Persisted on the extension side via
// `vscode.workspace.getConfiguration` so Settings Sync carries
// them across machines — same mechanism as permissions.
//
// Adding a new pref:
//   1. Add a field to AiPreferences in jl4-client-rpc
//   2. Add a default below
//   3. Add a config-key mapping in the extension's register.ts

type MessengerAccessor = () => InstanceType<typeof Messenger> | null

const DEFAULTS: AiPreferences = {
  showReasoning: false,
}

class AiPrefs {
  showReasoning = $state(DEFAULTS.showReasoning)
  private getMessenger: MessengerAccessor = () => null

  init(getMessenger: MessengerAccessor): void {
    this.getMessenger = getMessenger
    void this.refresh()
  }

  private async refresh(): Promise<void> {
    const m = this.getMessenger()
    if (!m) return
    try {
      const res = await m.sendRequest(
        AiPreferencesGet,
        HOST_EXTENSION,
        undefined as never
      )
      this.showReasoning = res.values.showReasoning
    } catch {
      // leave defaults on RPC failure
    }
  }

  private push(values: Partial<AiPreferences>): void {
    const m = this.getMessenger()
    m?.sendNotification(AiPreferencesSet, HOST_EXTENSION, { values })
  }

  setShowReasoning(next: boolean): void {
    this.showReasoning = next
    this.push({ showReasoning: next })
  }
}

export const aiPrefs = new AiPrefs()
