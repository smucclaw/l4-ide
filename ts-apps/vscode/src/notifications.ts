import * as vscode from 'vscode'

const AUTO_DISMISS_MS = 4_000

/**
 * Shows a notification that auto-dismisses after 4 seconds.
 * Uses vscode.window.withProgress as that's the only API that supports auto-dismiss.
 */
export function showTimedInformationMessage(message: string): void {
  vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: message,
      cancellable: false,
    },
    () => new Promise((resolve) => setTimeout(resolve, AUTO_DISMISS_MS))
  )
}

export function showTimedWarningMessage(message: string): void {
  vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: '⚠️ ' + message,
      cancellable: false,
    },
    () => new Promise((resolve) => setTimeout(resolve, AUTO_DISMISS_MS))
  )
}
