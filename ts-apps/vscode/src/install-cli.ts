// This file reads runtime-only environment variables (LOCALAPPDATA on
// Windows) that are set by the end user's OS — not at turbo build time —
// so the turbo-inputs lint rule does not apply here.
/* eslint-disable turbo/no-undeclared-env-vars */

// L4 CLI installer.
//
// Handles discovery of the bundled `l4` binary shipped inside the VSIX
// (at <extensionPath>/bin/<platform-arch>/l4[.exe]) and exposes a
// single user-facing command that installs it onto the user's PATH:
//
//  - macOS / Linux: symlink into `~/.local/bin/l4`. If `~/.local/bin`
//    isn't on the user's login-shell PATH, we auto-append the PATH
//    export to their shell rc file (e.g. `~/.zshrc`). If no rc file
//    exists we fall back to a copy-pasteable snippet.
//
//  - Windows: copy the exe into `%LOCALAPPDATA%\Programs\l4\l4.exe` and
//    append that directory to the user-level PATH via `setx PATH`.
//    setx requires the user to restart terminals for the change to
//    take effect; we warn them explicitly.
//
// The module also exposes a dismissal-aware startup prompt so we can
// offer to install the CLI the first time a user opens an L4 project.

import * as vscode from 'vscode'
import * as fs from 'node:fs'
import * as path from 'node:path'
import * as os from 'node:os'
import { execFile } from 'node:child_process'
import { promisify } from 'node:util'

const execFileAsync = promisify(execFile)

const CLI_INSTALL_DISMISSED_KEY = 'l4.installCliDismissed'

/**
 * Locate the bundled `l4` binary for the current platform, if it exists.
 *
 * Looks for `<extensionPath>/bin/<platform>-<arch>/l4[.exe]`. Returns
 * undefined when the extension was packaged without the bundled binary
 * (the universal build) or when the platform isn't one we ship.
 */
export function findBundledCli(
  extensionPath: string,
  outputChannel: vscode.OutputChannel
): string | undefined {
  const platformArch = `${process.platform}-${process.arch}`
  const exeName = process.platform === 'win32' ? 'l4.exe' : 'l4'
  const binPath = path.join(extensionPath, 'bin', platformArch, exeName)

  outputChannel.appendLine(
    `[install-cli] Looking for bundled l4 at: ${binPath}`
  )

  if (!fs.existsSync(binPath)) {
    outputChannel.appendLine(
      `[install-cli] No bundled l4 found for platform: ${platformArch}`
    )
    return undefined
  }

  if (process.platform !== 'win32') {
    try {
      fs.accessSync(binPath, fs.constants.X_OK)
    } catch {
      outputChannel.appendLine(
        `[install-cli] Bundled l4 is not executable: ${binPath}`
      )
      return undefined
    }
  }

  return binPath
}

/**
 * Ask the user's login shell if `l4` is already on PATH.
 *
 * On Unix we spawn a login-interactive shell (`$SHELL -li -c 'which l4'`)
 * so the check reflects what the user actually sees in a new terminal,
 * not the VS Code extension host's augmented PATH.
 */
async function isL4OnPath(): Promise<boolean> {
  if (process.platform === 'win32') {
    try {
      const { stdout } = await execFileAsync('where', ['l4'])
      return stdout.trim().length > 0
    } catch {
      return false
    }
  }

  const shell = process.env.SHELL || '/bin/sh'
  try {
    const { stdout } = await execFileAsync(shell, ['-li', '-c', 'which l4'], {
      timeout: 5000,
    })
    return stdout.trim().length > 0
  } catch {
    return false
  }
}

/**
 * Install the bundled `l4` binary onto the user's PATH. Shows progress
 * and result messages via VS Code. Safe to invoke repeatedly — each
 * run refreshes the symlink / file so upgrades propagate automatically.
 *
 * @param silent When true, suppresses success toasts (used when the
 * install is performed as a side effect of the "Add L4 Tools to Claude
 * Code" flow — we don't want to double-toast the user).
 */
export async function installL4Cli(
  extensionPath: string,
  outputChannel: vscode.OutputChannel,
  opts: { silent?: boolean } = {}
): Promise<{ ok: boolean; installedAt?: string; message: string }> {
  const bundled = findBundledCli(extensionPath, outputChannel)
  if (!bundled) {
    const message =
      'No bundled l4 binary found in this extension. Build the extension with the platform-specific packaging job, or set jl4.serverExecutablePath and install l4 manually.'
    outputChannel.appendLine(`[install-cli] ${message}`)
    if (!opts.silent) {
      void vscode.window.showErrorMessage(message)
    }
    return { ok: false, message }
  }

  try {
    if (process.platform === 'win32') {
      return await installWindows(bundled, outputChannel, opts.silent ?? false)
    } else {
      return await installUnix(bundled, outputChannel, opts.silent ?? false)
    }
  } catch (err) {
    const message = `Failed to install l4 CLI: ${err instanceof Error ? err.message : String(err)}`
    outputChannel.appendLine(`[install-cli] ${message}`)
    if (!opts.silent) {
      void vscode.window.showErrorMessage(message)
    }
    return { ok: false, message }
  }
}

/**
 * Unix install path: symlink the bundled binary into `~/.local/bin/l4`.
 *
 * If `~/.local/bin` isn't on the user's login-shell PATH, we
 * automatically append the PATH export to their shell rc file
 * (e.g. `~/.zshrc` or `~/.bashrc`). If no rc file exists we fall
 * back to offering a copy-pasteable snippet.
 */
async function installUnix(
  src: string,
  outputChannel: vscode.OutputChannel,
  silent: boolean
): Promise<{ ok: true; installedAt: string; message: string }> {
  const home = os.homedir()
  const localBin = path.join(home, '.local', 'bin')
  const target = path.join(localBin, 'l4')

  fs.mkdirSync(localBin, { recursive: true })

  // Remove any existing symlink or regular file so we can replace it.
  try {
    const stat = fs.lstatSync(target)
    if (stat.isSymbolicLink() || stat.isFile()) {
      fs.unlinkSync(target)
    }
  } catch {
    // Not there — that's fine.
  }

  fs.symlinkSync(src, target)
  outputChannel.appendLine(`[install-cli] Symlinked ${target} -> ${src}`)

  const onPath = await isL4OnPath()

  if (onPath) {
    const message = `l4 installed at ${target} and is on your PATH. Open a new terminal and try \`l4 run path/to/file.l4\`.`
    if (!silent) {
      void vscode.window.showInformationMessage(
        `L4 CLI installed. Run \`l4 --help\` in a new terminal to get started.`
      )
    }
    return { ok: true, installedAt: target, message }
  }

  // ~/.local/bin is not on PATH — try to fix the user's shell rc file.
  const rcUpdated = ensureLocalBinInShellRc(home, outputChannel)
  let message: string

  if (rcUpdated) {
    message = `l4 installed at ${target}. Updated ${rcUpdated} to add ~/.local/bin to your PATH — restart your terminal for it to take effect.`
    if (!silent) {
      void vscode.window.showInformationMessage(
        `L4 CLI installed and PATH configured in ${path.basename(rcUpdated)}. Restart your terminal, then run \`l4 --help\`.`
      )
    }
  } else {
    message = `l4 installed at ${target}, but ~/.local/bin is not on your PATH. Add it with:\n\n  export PATH="$HOME/.local/bin:$PATH"\n\nto your shell rc file, then restart your terminal.`
    if (!silent) {
      const action = await vscode.window.showWarningMessage(
        `L4 CLI installed at ${target}, but ~/.local/bin is not on your PATH.`,
        'Copy PATH snippet',
        'OK'
      )
      if (action === 'Copy PATH snippet') {
        await vscode.env.clipboard.writeText(
          'export PATH="$HOME/.local/bin:$PATH"'
        )
        void vscode.window.showInformationMessage(
          'Copied. Paste into your shell rc file and restart your terminal.'
        )
      }
    }
  }

  return { ok: true, installedAt: target, message }
}

const PATH_EXPORT_LINE = 'export PATH="$HOME/.local/bin:$PATH"'
const PATH_EXPORT_COMMENT =
  '# Added by L4 extension — puts ~/.local/bin on PATH'

/**
 * Detect the user's shell rc file, and if it exists but doesn't already
 * contain a `~/.local/bin` PATH entry, append one.
 *
 * Returns the rc file path that was updated, or undefined if we couldn't
 * or shouldn't modify anything.
 */
function ensureLocalBinInShellRc(
  home: string,
  outputChannel: vscode.OutputChannel
): string | undefined {
  const shell = path.basename(process.env.SHELL || '')
  // Ordered by preference — check the user's actual shell first.
  const candidates: string[] = []
  if (shell === 'zsh') candidates.push('.zshrc')
  else if (shell === 'bash') candidates.push('.bashrc', '.bash_profile')
  else if (shell === 'fish') {
    // fish uses a different syntax; fall back to manual instructions.
    return undefined
  }
  // Always include common fallbacks after the primary.
  if (!candidates.includes('.zshrc')) candidates.push('.zshrc')
  if (!candidates.includes('.bashrc')) candidates.push('.bashrc')

  for (const name of candidates) {
    const rcPath = path.join(home, name)
    if (!fs.existsSync(rcPath)) continue

    const content = fs.readFileSync(rcPath, 'utf-8')
    // Already has ~/.local/bin on PATH — nothing to do.
    if (content.includes('.local/bin')) {
      outputChannel.appendLine(
        `[install-cli] ${name} already references .local/bin`
      )
      return undefined
    }

    const suffix = content.endsWith('\n') ? '' : '\n'
    fs.appendFileSync(
      rcPath,
      `${suffix}\n${PATH_EXPORT_COMMENT}\n${PATH_EXPORT_LINE}\n`
    )
    outputChannel.appendLine(`[install-cli] Appended PATH export to ${rcPath}`)
    return rcPath
  }

  outputChannel.appendLine('[install-cli] No shell rc file found to update')
  return undefined
}

/**
 * Windows install path: copy the bundled exe into `%LOCALAPPDATA%\Programs\l4`
 * and append that directory to the user PATH via `setx`.
 *
 * We intentionally copy rather than symlink — Windows file system
 * symlinks require developer mode or admin privileges; copying avoids
 * the whole permission puzzle.
 */
async function installWindows(
  src: string,
  outputChannel: vscode.OutputChannel,
  silent: boolean
): Promise<{ ok: true; installedAt: string; message: string }> {
  const localAppData =
    process.env.LOCALAPPDATA ?? path.join(os.homedir(), 'AppData', 'Local')
  const installDir = path.join(localAppData, 'Programs', 'l4')
  const target = path.join(installDir, 'l4.exe')

  fs.mkdirSync(installDir, { recursive: true })
  fs.copyFileSync(src, target)
  outputChannel.appendLine(`[install-cli] Copied exe to ${target}`)

  // Add the install directory to user PATH if it's not already there.
  // We read the current user PATH from the registry via `reg query`, not
  // from `process.env.PATH`, because the latter is the *merged* machine
  // + user PATH and setx only writes the user portion.
  let userPath = ''
  try {
    const { stdout } = await execFileAsync('reg', [
      'query',
      'HKCU\\Environment',
      '/v',
      'Path',
    ])
    // Parse `Path    REG_EXPAND_SZ    C:\foo;C:\bar`
    const match = stdout.match(/Path\s+REG(?:_EXPAND)?_SZ\s+(.*)$/im)
    if (match) userPath = match[1].trim()
  } catch {
    // No user Path entry yet. setx will create it.
  }

  const alreadyInPath = userPath
    .split(';')
    .map((s) => s.trim().toLowerCase())
    .includes(installDir.toLowerCase())

  let pathUpdated = false
  if (!alreadyInPath) {
    const newPath = userPath ? `${userPath};${installDir}` : installDir
    try {
      await execFileAsync('setx', ['PATH', newPath])
      pathUpdated = true
      outputChannel.appendLine(
        `[install-cli] Appended ${installDir} to user PATH`
      )
    } catch (err) {
      outputChannel.appendLine(
        `[install-cli] setx PATH failed: ${err instanceof Error ? err.message : String(err)}`
      )
    }
  }

  const message = pathUpdated
    ? `l4 installed at ${target}. PATH updated — restart any open terminals for the change to take effect.`
    : alreadyInPath
      ? `l4 installed at ${target}. Already on PATH.`
      : `l4 installed at ${target}, but PATH could not be updated automatically. Add ${installDir} to your PATH manually.`

  if (!silent) {
    void vscode.window.showInformationMessage(message)
  }

  return { ok: true, installedAt: target, message }
}

/**
 * Show the one-time "Install L4 CLI?" prompt, unless:
 *   - l4 is already on PATH (nothing to do), or
 *   - the user previously chose "Never" (respect the dismissal).
 *
 * Called from the startup flow after the Claude Code prompt has been
 * handled (or skipped). Opt-in: does nothing if the bundled binary
 * isn't available for this platform.
 */
export async function maybeOfferInstallL4Cli(
  context: vscode.ExtensionContext,
  outputChannel: vscode.OutputChannel
): Promise<void> {
  // If we can't install anyway, don't prompt.
  if (!findBundledCli(context.extensionPath, outputChannel)) return

  if (await isL4OnPath()) {
    outputChannel.appendLine(
      '[install-cli] l4 is already on PATH — skipping prompt'
    )
    return
  }

  if (context.globalState.get<boolean>(CLI_INSTALL_DISMISSED_KEY)) {
    outputChannel.appendLine(
      '[install-cli] Install prompt previously dismissed'
    )
    return
  }

  const action = await vscode.window.showInformationMessage(
    'Install the L4 CLI? This lets you run `l4 run file.l4`, etc. from any terminal.',
    'Install',
    'Not now',
    'Never'
  )

  if (action === 'Never') {
    await context.globalState.update(CLI_INSTALL_DISMISSED_KEY, true)
    return
  }
  if (action !== 'Install') return

  await installL4Cli(context.extensionPath, outputChannel)
}
