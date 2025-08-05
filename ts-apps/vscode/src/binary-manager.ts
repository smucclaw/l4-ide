import { platform, arch } from 'os'
import { join } from 'path'
import { ExtensionContext, OutputChannel } from 'vscode'
import { existsSync, promises as fsPromises, constants } from 'fs'

export interface BinaryManifest {
  version: string
  binaries: Array<{
    platform: string
    target: string
    binary: string
  }>
}

export class BinaryManager {
  private context: ExtensionContext
  private manifest: BinaryManifest | null = null
  private outputChannel?: OutputChannel

  constructor(context: ExtensionContext, outputChannel?: OutputChannel) {
    this.context = context
    this.outputChannel = outputChannel
  }

  /**
   * Get the path to the bundled jl4-lsp binary for the current platform
   */
  async getBinaryPath(): Promise<string | null> {
    const manifest = await this.getManifest()
    if (!manifest) {
      this.log('No manifest found for binaries')
      return null
    }

    const currentPlatform = this.getCurrentPlatform()
    this.log(`Looking for binary for platform: ${currentPlatform}`)

    // Log available platforms for debugging
    const availablePlatforms = manifest.binaries.map((b) => b.platform)
    this.log(
      `Available platforms in manifest: ${availablePlatforms.join(', ')}`
    )

    const binaryInfo = manifest.binaries.find(
      (b) => b.platform === currentPlatform
    )

    if (!binaryInfo) {
      this.log(`No binary found for platform: ${currentPlatform}`)

      // Try to find a fallback binary
      const fallbackBinary = this.findFallbackBinary(manifest, currentPlatform)
      if (fallbackBinary) {
        this.log(
          `Using fallback binary for platform: ${fallbackBinary.platform}`
        )
        return this.validateAndReturnBinaryPath(fallbackBinary)
      }

      return null
    }

    return this.validateAndReturnBinaryPath(binaryInfo)
  }

  /**
   * Validate and return the binary path
   */
  private async validateAndReturnBinaryPath(binaryInfo: {
    platform: string
    target: string
    binary: string
  }): Promise<string | null> {
    const binaryPath = join(
      this.context.extensionPath,
      'static',
      'binaries',
      binaryInfo.platform,
      binaryInfo.binary
    )

    this.log(`Checking binary at path: ${binaryPath}`)

    if (!existsSync(binaryPath)) {
      this.log(`Binary not found at path: ${binaryPath}`)

      // Let's also check what's actually in the binaries directory
      await this.debugBinariesDirectory()
      return null
    }

    // Check if the binary is executable
    try {
      await fsPromises.access(binaryPath, constants.X_OK)
    } catch (error) {
      this.log(
        `Binary found but not executable: ${binaryPath} - Error: ${error}`
      )

      // Try to make it executable (works on Unix systems)
      if (platform() !== 'win32') {
        try {
          await fsPromises.chmod(binaryPath, 0o755)
          this.log(`Made binary executable: ${binaryPath}`)
        } catch (chmodError) {
          this.log(`Failed to make binary executable: ${chmodError}`)
          return null
        }
      } else {
        // On Windows, executable permission checking works differently
        // We'll just return the path and let the OS handle it
        this.log('On Windows, continuing despite executable check failure')
      }
    }

    this.log(`Found valid binary at: ${binaryPath}`)
    return binaryPath
  }

  /**
   * Find a fallback binary for similar platforms
   */
  private findFallbackBinary(
    manifest: BinaryManifest,
    currentPlatform: string
  ): { platform: string; target: string; binary: string } | null {
    const [os] = currentPlatform.split('-')

    // Try to find a binary for the same OS but different architecture
    const sameOsBinaries = manifest.binaries.filter((b) =>
      b.platform.startsWith(os + '-')
    )
    if (sameOsBinaries.length > 0) {
      this.log(
        `Found ${sameOsBinaries.length} binaries for OS ${os}: ${sameOsBinaries.map((b) => b.platform).join(', ')}`
      )
      return sameOsBinaries[0] // Return the first match
    }

    return null
  }

  /**
   * Debug what's actually in the binaries directory
   */
  private async debugBinariesDirectory(): Promise<void> {
    try {
      const binariesPath = join(
        this.context.extensionPath,
        'static',
        'binaries'
      )
      this.log(`Checking binaries directory: ${binariesPath}`)

      if (!existsSync(binariesPath)) {
        this.log(`Binaries directory does not exist: ${binariesPath}`)

        // Check if static directory exists
        const staticPath = join(this.context.extensionPath, 'static')
        if (!existsSync(staticPath)) {
          this.log(`Static directory does not exist: ${staticPath}`)

          // List what's in the extension path
          const extensionContents = await fsPromises.readdir(
            this.context.extensionPath
          )
          this.log(`Extension root contains: ${extensionContents.join(', ')}`)
        } else {
          const staticContents = await fsPromises.readdir(staticPath)
          this.log(`Static directory contains: ${staticContents.join(', ')}`)
        }
        return
      }

      const contents = await fsPromises.readdir(binariesPath)
      this.log(`Binaries directory contains: ${contents.join(', ')}`)

      // Check subdirectories
      for (const item of contents) {
        const itemPath = join(binariesPath, item)
        const stat = await fsPromises.stat(itemPath)
        if (stat.isDirectory()) {
          const subContents = await fsPromises.readdir(itemPath)
          this.log(`  ${item}/ contains: ${subContents.join(', ')}`)
        }
      }
    } catch (error) {
      this.log(`Error debugging binaries directory: ${error}`)
    }
  }

  /**
   * Get the manifest file with binary information
   */
  private async getManifest(): Promise<BinaryManifest | null> {
    if (this.manifest) {
      return this.manifest
    }

    try {
      const manifestPath = join(
        this.context.extensionPath,
        'static',
        'binaries',
        'manifest.json'
      )

      this.log(`Looking for manifest at: ${manifestPath}`)

      if (!existsSync(manifestPath)) {
        this.log('Binary manifest not found at path: ' + manifestPath)
        await this.debugBinariesDirectory()
        return null
      }

      const manifestContent = await fsPromises.readFile(manifestPath, 'utf8')

      this.manifest = JSON.parse(manifestContent) as BinaryManifest
      this.log(`Loaded manifest with ${this.manifest.binaries.length} binaries`)
      this.log(`Manifest version: ${this.manifest.version}`)
      return this.manifest
    } catch (error) {
      this.log('Failed to load binary manifest: ' + error)
      return null
    }
  }

  /**
   * Get the current platform identifier
   */
  private getCurrentPlatform(): string {
    const os = platform()
    const architecture = arch()

    // Map Node.js platform names to our binary naming convention
    const platformMap: Record<string, string> = {
      linux: 'linux',
      darwin: 'darwin',
      win32: 'win32',
    }

    const mappedOs = platformMap[os] || os

    // For macOS, we need to distinguish between x64 and arm64
    if (mappedOs === 'darwin') {
      return architecture === 'arm64' ? 'darwin-arm64' : 'darwin-x64'
    }

    // For Linux, also check ARM architecture
    if (mappedOs === 'linux' && architecture === 'arm64') {
      return 'linux-arm64'
    }

    // For other platforms, assume x64
    const platformId = `${mappedOs}-x64`
    this.log(
      `Detected platform: ${platformId} (OS: ${os}, Arch: ${architecture})`
    )
    return platformId
  }

  /**
   * Check if bundled binaries are available
   */
  async hasBundledBinaries(): Promise<boolean> {
    const binaryPath = await this.getBinaryPath()
    return binaryPath !== null
  }

  /**
   * Get list of available platforms
   */
  async getAvailablePlatforms(): Promise<string[]> {
    const manifest = await this.getManifest()
    return manifest?.binaries.map((b) => b.platform) || []
  }

  /**
   * Log a message to the output channel if available
   */
  private log(message: string): void {
    if (this.outputChannel) {
      this.outputChannel.appendLine(`[BinaryManager] ${message}`)
    } else {
      console.log(`[BinaryManager] ${message}`)
    }
  }
}
