// L4 CLI installer.
//
// Handles discovery of the bundled `l4` binary shipped inside the VSIX
// (at <extensionPath>/bin/<platform-arch>/l4[.exe]) and exposes a
// single user-facing command that installs it onto the user's PATH:
//
//  - macOS / Linux: symlink into `~/.local/bin/l4`. `~/.local/bin` is on
//    PATH by default in most modern shells via the user-profile autoconf
//    pattern, but when it isn't, we surface a copy-pasteable snippet
//    instead of silently editing rc files.
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
 * Ask the shell if `l4` is already on PATH.
 *
 * Avoids `which` / `where` having different exit codes by swallowing
 * both and just checking for a non-empty stdout line.
 */
async function isL4OnPath(): Promise<boolean> {
  const cmd = process.platform === 'win32' ? 'where' : 'which'
  try {
    const { stdout } = await execFileAsync(cmd, ['l4'])
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
 * `~/.local/bin` is on PATH by default on macOS via the `path_helper`
 * mechanism only when `~/.local/bin` is listed in `/etc/paths.d/`; on
 * Linux distros with `systemd --user` sessions it's appended via
 * `~/.profile`. We don't rely on either — if `~/.local/bin` isn't in
 * PATH after the symlink, we show the user a one-liner they can paste
 * into their shell rc file themselves.
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
  const message = onPath
    ? `l4 installed at ${target} and is on your PATH. Open a new terminal and try \`l4 run path/to/file.l4\`.`
    : `l4 installed at ${target}, but ~/.local/bin is not on your PATH. Add it with:\n\n  export PATH="$HOME/.local/bin:$PATH"\n\nto your ~/.zshrc or ~/.bashrc, then restart your terminal.`

  if (!silent) {
    if (onPath) {
      void vscode.window.showInformationMessage(
        `L4 CLI installed. Run \`l4 --help\` in a new terminal to get started.`
      )
    } else {
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
          'Copied. Paste into ~/.zshrc or ~/.bashrc and restart your terminal.'
        )
      }
    }
  }

  return { ok: true, installedAt: target, message }
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
  // LOCALAPPDATA is read at VS Code runtime on the user's machine, not at
  // turbo build time, so the turbo inputs lint rule doesn't apply here.
  // eslint-disable-next-line turbo/no-undeclared-env-vars
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
    'Install the L4 CLI (`l4`) on your PATH? This lets you run `l4 run file.l4`, `l4 check file.l4`, etc. from any terminal.',
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
