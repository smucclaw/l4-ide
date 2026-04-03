#!/bin/bash
# Conditionally build WASM if VITE_PREFER_WASM is enabled
#
# This script checks if WASM mode is enabled (via .env.local or environment)
# and builds the WASM module if needed.
#
# Usage: ./scripts/maybe-build-wasm.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WEB_APP_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
WASM_BUILD_SCRIPT="$PROJECT_ROOT/jl4-wasm/scripts/build-wasm.sh"

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Check if WASM mode is enabled
check_wasm_enabled() {
    # Check environment variable first
    if [ "$VITE_PREFER_WASM" = "true" ]; then
        return 0
    fi
    
    # Check .env.local file
    local ENV_LOCAL="$WEB_APP_DIR/.env.local"
    if [ -f "$ENV_LOCAL" ]; then
        if grep -q "^VITE_PREFER_WASM=true" "$ENV_LOCAL" 2>/dev/null; then
            return 0
        fi
    fi
    
    # Check .env.wasm file (alternative config)
    local ENV_WASM="$WEB_APP_DIR/.env.wasm"
    if [ -f "$ENV_WASM" ]; then
        if grep -q "^VITE_PREFER_WASM=true" "$ENV_WASM" 2>/dev/null; then
            return 0
        fi
    fi
    
    return 1
}

# Check if WASM files exist
wasm_files_exist() {
    local WASM_DIR="$WEB_APP_DIR/static/wasm"
    if [ -f "$WASM_DIR/jl4-core.wasm" ] && [ -f "$WASM_DIR/jl4-core.mjs" ]; then
        return 0
    fi
    return 1
}

# Main
main() {
    if ! check_wasm_enabled; then
        echo -e "${BLUE}[WASM] Skipping - VITE_PREFER_WASM is not enabled${NC}"
        exit 0
    fi
    
    echo -e "${BLUE}[WASM] WASM mode enabled${NC}"
    
    # Check if WASM files already exist
    if wasm_files_exist; then
        echo -e "${GREEN}[WASM] WASM files already exist, skipping build${NC}"
        echo -e "${YELLOW}[WASM] To force rebuild, delete static/wasm/ and run again${NC}"
        exit 0
    fi
    
    # Check if build script exists
    if [ ! -f "$WASM_BUILD_SCRIPT" ]; then
        echo -e "${YELLOW}[WASM] Build script not found: $WASM_BUILD_SCRIPT${NC}"
        echo -e "${YELLOW}[WASM] Please build WASM manually or copy pre-built files to static/wasm/${NC}"
        exit 1
    fi
    
    # Check if GHC WASM toolchain is available
    GHC_WASM_ENV="${GHC_WASM_ENV:-$HOME/.ghc-wasm/env}"
    if [ ! -f "$GHC_WASM_ENV" ]; then
        echo -e "${YELLOW}[WASM] GHC WASM toolchain not found at $GHC_WASM_ENV${NC}"
        echo -e "${YELLOW}[WASM] Install with: curl -L https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.10 bash${NC}"
        echo -e "${YELLOW}[WASM] Or copy pre-built WASM files to static/wasm/${NC}"
        exit 1
    fi
    
    echo -e "${BLUE}[WASM] Building WASM module...${NC}"
    "$WASM_BUILD_SCRIPT" --all
    
    echo -e "${GREEN}[WASM] Build complete${NC}"
}

main "$@"
