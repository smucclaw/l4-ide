#!/usr/bin/env node

import { execSync } from 'child_process'
import { writeFileSync, mkdirSync, existsSync, statSync } from 'fs'
import { join, dirname } from 'path'
import { fileURLToPath } from 'url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = dirname(__filename)

// All target platforms
const platforms = [
  { os: 'linux', arch: 'x64', target: 'x86_64-unknown-linux-gnu' },
  { os: 'darwin', arch: 'x64', target: 'x86_64-apple-darwin' },
  { os: 'darwin', arch: 'arm64', target: 'aarch64-apple-darwin' },
  { os: 'win32', arch: 'x64', target: 'x86_64-pc-windows-msvc' }
]

const strip = "upx" || "strip";

// Output directory for binaries
const binariesDir = join(__dirname, '..', 'static', 'binaries')

async function buildAllPlatforms() {
  console.log('Building binaries for all platforms...')
  
  // Create binaries directory
  if (!existsSync(binariesDir)) {
    mkdirSync(binariesDir, { recursive: true })
  }

  const currentPlatform = await getCurrentPlatform()
  console.log(`Current platform: ${currentPlatform}`)
  
  const projectRoot = join(__dirname, '..', '..', '..')
  const builtBinaries = []
  
  for (const platform of platforms) {
    const platformName = `${platform.os}-${platform.arch}`
    console.log(`\n--- Building for ${platformName} ---`)
    
    try {
      const success = await buildForPlatform(projectRoot, platform, platformName, currentPlatform)
      if (success) {
        builtBinaries.push({
          platform: platformName,
          target: platform.target,
          binary: platform.os === 'win32' ? 'jl4-lsp.exe' : 'jl4-lsp'
        })
        console.log(`✓ Successfully built for ${platformName}`)
      } else {
        console.log(`⚠ Could not build for ${platformName}`)
      }
    } catch (error) {
      console.error(`✗ Failed to build for ${platformName}:`, error.message)
    }
  }
  
  // Create comprehensive manifest
  const manifest = {
    version: '1.1.0',
    buildInfo: {
      buildDate: new Date().toISOString(),
      hostPlatform: currentPlatform,
      totalPlatforms: platforms.length,
      successfulBuilds: builtBinaries.length
    },
    binaries: builtBinaries
  }
  
  writeFileSync(
    join(binariesDir, 'manifest.json'),
    JSON.stringify(manifest, null, 2)
  )
  
  console.log('\n=== Build Summary ===')
  console.log(`✓ Successfully built ${builtBinaries.length} out of ${platforms.length} platforms`)
  console.log(`Binaries available in: ${binariesDir}`)
  
  if (builtBinaries.length === 0) {
    console.error('✗ No binaries were successfully built!')
    process.exit(1)
  }
}

async function buildForPlatform(projectRoot, platform, platformName, currentPlatform) {
  const platformDir = join(binariesDir, platformName)
  if (!existsSync(platformDir)) {
    mkdirSync(platformDir, { recursive: true })
  }
  
  const binaryName = platform.os === 'win32' ? 'jl4-lsp.exe' : 'jl4-lsp'
  const targetPath = join(platformDir, binaryName)
  
  // Strategy 1: Native build (if current platform matches)
  if (platformName === currentPlatform) {
    console.log(`Using native build for current platform`)
    return await buildNative(projectRoot, platformDir, targetPath)
  }
  
  // Strategy 2: Cross-compilation attempts
  console.log(`Attempting cross-compilation for ${platform.target}`)
  
  // Try different cross-compilation approaches
  const strategies = [
    () => buildWithCrossCompilation(projectRoot, platform, targetPath),
    () => buildWithDocker(projectRoot, platform, targetPath),
    () => buildWithRosetta(projectRoot, platform, targetPath, currentPlatform),
    () => {
      // Fallback Strategy: Create a placeholder/script for manual building
      console.log(`Fallback: Creating build script for manual compilation`)
      createBuildScript(platformDir, platform)
      return false
    }
  ]
  
  for (let i = 0; i < strategies.length; i++) {
    try {
      console.log(`  Trying strategy ${i + 1}...`)
      const success = await strategies[i]()
      if (success) {
        return true
      }
    } catch (error) {
      console.log(`  Strategy ${i + 1} failed: ${error.message}`)
    }
  }
  return false;
}

async function buildNative(projectRoot, platformDir, targetPath) {
  try {
    console.log(`Building natively...`)
    
    // Build with cabal
    execSync(`cabal install exe:jl4-lsp --install-method=copy --installdir=${platformDir} --overwrite-policy=always`, { 
      cwd: join(projectRoot),
      stdio: 'inherit'
    })
    
    // Find the built binary
    if (! existsSync(targetPath)) {
      throw new Error('Could not find built binary')
    }
    
    await processBinary(targetPath)
    
    return true
  } catch (error) {
    console.log(`Native build failed: ${error.message}`)
    return false
  }
}

async function buildWithCrossCompilation(projectRoot, platform, targetPath) {
  console.log(`Trying GHC cross-compilation for ${platform.target}`)
  
  try {
    // Check if we have cross-compilation tools
    const ghcVersion = execSync('ghc --version', { encoding: 'utf8' })
    console.log(`Using GHC: ${ghcVersion.trim()}`)
    
    // Build with target specification
    let buildCmd = 'cabal build exe:jl4-lsp'
    let extraArgs = []
    
    // Platform-specific cross-compilation flags
    if (platform.os === 'darwin' && platform.arch === 'x64') {
      // Try building for x86_64 on arm64 mac
      extraArgs.push('--ghc-options="-arch x86_64"')
    } else if (platform.os === 'linux') {
      // Try static linking for Linux
      extraArgs.push('--ghc-options="-static -optl-static -optl-pthread"')
    }
    
    if (extraArgs.length > 0) {
      buildCmd += ' ' + extraArgs.join(' ')
    }
    
    execSync(buildCmd, { 
      cwd: join(projectRoot, 'jl4-lsp'),
      stdio: 'inherit'
    })
    
    const binaryPath = await findBinary(projectRoot)
    if (binaryPath && existsSync(binaryPath)) {
      execSync(`cp "${binaryPath}" "${targetPath}"`)
      await processBinary(targetPath)
      return true
    }
    
    return false
  } catch (error) {
    throw new Error(`Cross-compilation failed: ${error.message}`)
  }
}

async function buildWithDocker(projectRoot, platform, targetPath) {
  console.log(`Trying Docker build for ${platform.target}`)
  
  // Check if Docker is available
  try {
    execSync('docker --version', { stdio: 'pipe' })
  } catch (error) {
    throw new Error('Docker not available')
  }
  
  // Docker images for cross-compilation
  const dockerImages = {
    'x86_64-unknown-linux-gnu': 'haskell:9.6.6',
    'x86_64-apple-darwin': 'ghcr.io/cross-rs/x86_64-apple-darwin:main',
    'aarch64-apple-darwin': 'ghcr.io/cross-rs/aarch64-apple-darwin:main',
    'x86_64-pc-windows-msvc': 'ghcr.io/cross-rs/x86_64-pc-windows-msvc:main'
  }
  
  const dockerImage = dockerImages[platform.target]
  if (!dockerImage) {
    throw new Error(`No Docker image available for ${platform.target}`)
  }
  
  // This would require a more complex Docker setup
  throw new Error('Docker cross-compilation not yet implemented')
}

async function buildWithRosetta(projectRoot, platform, targetPath, currentPlatform) {
  // Only works on macOS for x86_64 targets
  if (currentPlatform !== 'darwin-arm64' || platform.os !== 'darwin' || platform.arch !== 'x64') {
    throw new Error('Rosetta only applicable for x64 on arm64 macOS')
  }
  
  console.log(`Trying Rosetta x86_64 build`)
  
  try {
    // Check if we can run x86_64 commands
    execSync('arch -x86_64 uname -m', { stdio: 'pipe' })
    
    // Try building with x86_64 architecture
    execSync('arch -x86_64 cabal build exe:jl4-lsp', { 
      cwd: join(projectRoot, 'jl4-lsp'),
      stdio: 'inherit'
    })
    
    const binaryPath = await findBinary(projectRoot)
    if (binaryPath && existsSync(binaryPath)) {
      execSync(`cp "${binaryPath}" "${targetPath}"`)
      await processBinary(targetPath)
      return true
    }
    
    return false
  } catch (error) {
    throw new Error(`Rosetta build failed: ${error.message}`)
  }
}

function createBuildScript(platformDir, platform) {
  const scriptName = platform.os === 'win32' ? 'build.bat' : 'build.sh'
  const binaryName = platform.os === 'win32' ? 'jl4-lsp.exe' : 'jl4-lsp'
  
  let script = ''
  
  if (platform.os === 'win32') {
    script = `@echo off
echo Building jl4-lsp for ${platform.target}
echo This requires a Windows machine with Haskell installed
echo.
echo Run these commands:
echo cabal build exe:jl4-lsp
echo copy dist-newstyle\\build\\x86_64-windows\\ghc-*\\jl4-lsp\\x\\jl4-lsp\\build\\jl4-lsp\\jl4-lsp.exe ${binaryName}
`
  } else {
    script = `#!/bin/bash
echo "Building jl4-lsp for ${platform.target}"
echo "This requires cross-compilation setup or native ${platform.os} machine"
echo ""
echo "Commands to run:"
echo "cabal build exe:jl4-lsp"
echo "cp \$(cabal list-bin exe:jl4-lsp) ${binaryName}"
echo "${strip} ${binaryName}"
echo "chmod +x ${binaryName}"
`
  }
  
  const scriptPath = join(platformDir, scriptName)
  writeFileSync(scriptPath, script)
  
  if (platform.os !== 'win32') {
    execSync(`chmod +x "${scriptPath}"`)
  }
  
  console.log(`Created build script: ${scriptPath}`)
  return true
}

async function processBinary(targetPath) {
  // Set executable permissions
  execSync(`chmod +x "${targetPath}"`)
  
  // Show original size
  try {
    const original = statSync(targetPath)
    console.log(`  Original size: ${original.size}`)

    execSync(`${strip} "${targetPath}"`)

    const stripped = statSync(targetPath);
    console.log(`  Stripped size: ${stripped.size}`)
  }
  catch (error) {
    console.log(`  Could not strip binary: ${error.message}`)
  }
}


async function findBinary(projectRoot) {
  try {
    console.log('Looking for jl4-lsp output binary ... trying cabal list-bin first')
    // Use cabal to get the exact path
    const output = execSync('cabal list-bin exe:jl4-lsp', { 
      cwd: join(projectRoot, 'jl4-lsp'),
      encoding: 'utf8'
    }).trim()
    
    if (existsSync(output)) {
      console.log(`found ${output}`)
      return output
    }
  } catch (error) {
    console.log('Could not use cabal list-bin, trying find command...')
  }
  
  // Fallback to find command
  const distDir = join(projectRoot, 'jl4-lsp', 'dist-newstyle')
  const findCmd = `find "${distDir}" -name "jl4-lsp" -type f -perm -u+x -print -quit`
  try {
    return execSync(findCmd, { encoding: 'utf8' }).trim()
  } catch (error) {
    return null
  }
}

async function getCurrentPlatform() {
  const { platform, arch } = await import('os')
  const os = platform()
  const architecture = arch()
  
  // Map Node.js platform names to our binary naming convention
  const platformMap = {
    'linux': 'linux',
    'darwin': 'darwin',
    'win32': 'win32'
  }
  
  const mappedOs = platformMap[os] || os
  
  // For macOS, we need to distinguish between x64 and arm64
  if (mappedOs === 'darwin') {
    return architecture === 'arm64' ? 'darwin-arm64' : 'darwin-x64'
  }
  
  // For other platforms, assume x64
  return `${mappedOs}-x64`
}

function getTargetForPlatform(platform) {
  const targetMap = {
    'linux-x64': 'x86_64-unknown-linux-gnu',
    'darwin-x64': 'x86_64-apple-darwin',
    'darwin-arm64': 'aarch64-apple-darwin',
    'win32-x64': 'x86_64-pc-windows-msvc'
  }
  return targetMap[platform] || platform
}

// Run the build
buildAllPlatforms().catch(console.error)
