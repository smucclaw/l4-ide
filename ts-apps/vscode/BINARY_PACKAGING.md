# Binary Packaging for VSCode Extension

This document explains how the jl4-lsp and jl4-core binaries are packaged into the VSCode extension for multi-platform support.

## Overview

The VSCode extension includes bundled binaries of the jl4-lsp language server to provide a seamless experience without requiring users to install Haskell or build the binaries themselves.

## Architecture

### Binary Management
- **BinaryManager**: A TypeScript class that handles binary selection and platform detection
- **Manifest-based**: Binaries are organized with a JSON manifest file that describes available platforms
- **Fallback Strategy**: If bundled binaries aren't available, falls back to system-installed binaries

### Platform Support
The extension supports attempt building for the following platforms:
- `linux-x64`: Linux x86_64
- `darwin-x64`: macOS Intel
- `darwin-arm64`: macOS Apple Silicon
- `win32-x64`: Windows x86_64

## Build Process

### Local Development
For local development, the extension builds the binary for the current platform:

```bash
npm run build-stripped-binary
```

This script:
1. Detects the current platform
2. Builds jl4-lsp using cabal
3. Strips the binary to reduce size
4. Copies the binary to `static/binaries/{platform}/`
5. Creates a manifest file

### Multi-Platform Builds
For broader platform support, it'll be better to use a GitHub Actions workflow:


## File Structure

```
ts-apps/vscode/
├── static/
│   └── binaries/
│       ├── manifest.json
│       ├── linux-x64/
│       │   └── jl4-lsp
│       ├── darwin-x64/
│       │   └── jl4-lsp
│       ├── darwin-arm64/
│       │   └── jl4-lsp
│       └── win32-x64/
│           └── jl4-lsp.exe
├── src/
│   ├── binary-manager.ts
│   └── extension.mts
└── scripts/
    ├── build-binaries.mjs
```

## Manifest Format

The `manifest.json` file describes available binaries:

```json
{
  "version": "1.1.0",
  "buildInfo": {
    "buildDate": "2025-08-03T02:28:09.747Z",
    "hostPlatform": "darwin-arm64",
    "totalPlatforms": 4,
    "successfulBuilds": 2
  },
  "binaries": [
    {
      "platform": "darwin-arm64",
      "target": "aarch64-apple-darwin",
      "binary": "jl4-lsp"
    },
    {
      "platform": "win32-x64",
      "target": "x86_64-pc-windows-msvc",
      "binary": "jl4-lsp.exe"
    }
  ]
}
```

## Binary Selection Logic

The extension uses the following priority order for binary selection:

1. **User Configuration**: If `jl4.serverExecutablePath` is set in VSCode settings
2. **Bundled Binary**: If a binary is available for the current platform
3. **System Binary**: Falls back to `jl4-lsp` command (must be in PATH)

## Development Workflow

### Adding a New Platform

1. Update the platform list in build scripts
2. Add the platform to GitHub Actions workflow
3. Update the BinaryManager platform detection
4. Test the build process

### Updating Binaries

1. Make changes to jl4-lsp or jl4-core
2. Push to main/develop branch
3. GitHub Actions will automatically rebuild binaries
4. Download new binaries for local development

### Testing

```bash
# Build extension with current platform binary
npm run build

# Package extension
npm run package

# Test in VSCode
code --install-extension l4-vscode-1.1.0.vsix
```

## Troubleshooting

### Binary Not Found
- Check that the binary was built successfully
- Verify the manifest.json file exists and is valid
- Ensure the binary has execute permissions

## Future Improvements

- [ ] Implement static linking for further size reduction
- [ ] Add binary compression (UPX, etc.)
- [ ] Implement automatic binary updates
- [ ] Add support for more platforms (ARM Linux, etc.)
- [ ] Cross-compilation for multi-platform builds in CI
