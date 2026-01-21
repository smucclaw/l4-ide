#!/bin/bash
# WASM Build Script
# Builds the L4 WASM module and deploys it to the web app.
#
# Prerequisites:
#   - GHC WASM toolchain installed (~/.ghc-wasm)
#   - Install with: curl -L https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.10 bash
#
# Usage:
#   ./scripts/build-wasm.sh              # Build only
#   ./scripts/build-wasm.sh --optimize   # Build and optimize (-Oz)
#   ./scripts/build-wasm.sh --test       # Build and run tests
#   ./scripts/build-wasm.sh --all        # Build, optimize, and test

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WASM_PKG_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
WEB_APP_WASM_DIR="$PROJECT_ROOT/ts-apps/jl4-web/static/wasm"

# GHC WASM environment
GHC_WASM_ENV="${GHC_WASM_ENV:-$HOME/.ghc-wasm/env}"

# Build output paths (will be determined after build)
DIST_DIR="$PROJECT_ROOT/dist-newstyle"

# Options
DO_OPTIMIZE=false
DO_TEST=false
OPT_LEVEL="-Oz"

# Print a section header
section() {
    echo ""
    echo -e "${BLUE}━━━ $1 ━━━${NC}"
}

# Print success message
success() {
    echo -e "${GREEN}✓ $1${NC}"
}

# Print warning message
warn() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

# Print error and exit
error() {
    echo -e "${RED}✗ $1${NC}"
    exit 1
}

# Check prerequisites
check_prerequisites() {
    section "Checking prerequisites"
    
    # Check for GHC WASM environment
    if [ ! -f "$GHC_WASM_ENV" ]; then
        error "GHC WASM environment not found at $GHC_WASM_ENV

Install the GHC WASM toolchain:
  curl -L https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh | FLAVOUR=9.10 bash"
    fi
    success "GHC WASM environment found"
    
    # Source the environment
    echo "  Sourcing $GHC_WASM_ENV"
    source "$GHC_WASM_ENV"
    
    # Check wasm32-wasi-cabal
    if ! command -v wasm32-wasi-cabal &> /dev/null; then
        error "wasm32-wasi-cabal not found. Is the GHC WASM environment sourced?"
    fi
    success "wasm32-wasi-cabal available"
    
    # Check wasm32-wasi-ghc
    if ! command -v wasm32-wasi-ghc &> /dev/null; then
        error "wasm32-wasi-ghc not found. Is the GHC WASM environment sourced?"
    fi
    success "wasm32-wasi-ghc available ($(wasm32-wasi-ghc --numeric-version))"
    
    # Check for wasm-opt if optimization is requested
    if $DO_OPTIMIZE; then
        if ! command -v wasm-opt &> /dev/null; then
            warn "wasm-opt not found - optimization will be skipped
  Install: brew install binaryen (macOS) or apt install binaryen (Ubuntu)"
            DO_OPTIMIZE=false
        else
            success "wasm-opt available ($(wasm-opt --version))"
        fi
    fi
}

# Build the WASM module
build_wasm() {
    section "Building WASM module"
    
    cd "$PROJECT_ROOT"
    
    echo "  Running: wasm32-wasi-cabal build jl4-wasm --project-file=cabal-wasm.project"
    echo ""
    
    wasm32-wasi-cabal build jl4-wasm --project-file=cabal-wasm.project
    
    success "WASM build complete"
}

# Find the built WASM file
find_wasm_output() {
    section "Locating build output"
    
    # Find the WASM binary
    WASM_BINARY=$(find "$DIST_DIR" -path "*/wasm32-wasi/*/jl4-wasm/jl4-wasm.wasm" -type f 2>/dev/null | head -1)
    
    if [ -z "$WASM_BINARY" ]; then
        error "Could not find jl4-wasm.wasm in $DIST_DIR"
    fi
    
    echo "  Found: $WASM_BINARY"
    
    # Get file size
    if [[ "$OSTYPE" == "darwin"* ]]; then
        WASM_SIZE=$(stat -f%z "$WASM_BINARY")
    else
        WASM_SIZE=$(stat --format=%s "$WASM_BINARY")
    fi
    WASM_SIZE_MB=$(echo "scale=1; $WASM_SIZE / 1048576" | bc)
    echo "  Size: ${WASM_SIZE_MB} MB"
    
    success "Build output located"
}

# Generate JS FFI glue code
generate_js_ffi() {
    section "Generating JS FFI glue code"
    
    local GHC_LIBDIR=$(wasm32-wasi-ghc --print-libdir)
    local POST_LINK="$GHC_LIBDIR/post-link.mjs"
    
    if [ ! -f "$POST_LINK" ]; then
        error "post-link.mjs not found at $POST_LINK"
    fi
    
    local JS_OUTPUT="$DIST_DIR/jl4-wasm.mjs"
    
    echo "  Running: post-link.mjs -i ... -o $JS_OUTPUT"
    "$POST_LINK" -i "$WASM_BINARY" -o "$JS_OUTPUT"
    
    JS_FFI_FILE="$JS_OUTPUT"
    success "JS FFI glue generated: $JS_FFI_FILE"
}

# Deploy to web app
deploy_to_webapp() {
    section "Deploying to web app"
    
    # Create target directory if it doesn't exist
    mkdir -p "$WEB_APP_WASM_DIR"
    
    # Copy WASM binary
    echo "  Copying WASM binary..."
    cp "$WASM_BINARY" "$WEB_APP_WASM_DIR/jl4-core.wasm"
    
    # Copy JS FFI glue
    echo "  Copying JS FFI glue..."
    cp "$JS_FFI_FILE" "$WEB_APP_WASM_DIR/jl4-core.mjs"
    
    success "Deployed to $WEB_APP_WASM_DIR"
    
    # List deployed files
    echo ""
    ls -lh "$WEB_APP_WASM_DIR"
}

# Optimize the WASM binary
optimize_wasm() {
    section "Optimizing WASM binary"
    
    local OPTIMIZE_SCRIPT="$SCRIPT_DIR/optimize-wasm.sh"
    
    if [ -f "$OPTIMIZE_SCRIPT" ]; then
        "$OPTIMIZE_SCRIPT" "$OPT_LEVEL"
    else
        echo "  Running: wasm-opt $OPT_LEVEL"
        local WASM_TARGET="$WEB_APP_WASM_DIR/jl4-core.wasm"
        wasm-opt "$OPT_LEVEL" "$WASM_TARGET" -o "$WASM_TARGET.opt"
        mv "$WASM_TARGET.opt" "$WASM_TARGET"
        
        # Show new size
        if [[ "$OSTYPE" == "darwin"* ]]; then
            local NEW_SIZE=$(stat -f%z "$WASM_TARGET")
        else
            local NEW_SIZE=$(stat --format=%s "$WASM_TARGET")
        fi
        local NEW_SIZE_MB=$(echo "scale=1; $NEW_SIZE / 1048576" | bc)
        success "Optimized to ${NEW_SIZE_MB} MB"
    fi
}

# Run tests
run_tests() {
    section "Running integration tests"
    
    local TEST_SCRIPT="$PROJECT_ROOT/ts-apps/jl4-web/scripts/test-wasm-integration.mjs"
    
    if [ ! -f "$TEST_SCRIPT" ]; then
        warn "Test script not found: $TEST_SCRIPT"
        return
    fi
    
    cd "$PROJECT_ROOT/ts-apps/jl4-web"
    
    # Try to find a suitable Node.js
    local NODE=""
    if [ -f ~/.ghc-wasm/nodejs/bin/node ]; then
        NODE=~/.ghc-wasm/nodejs/bin/node
    elif command -v node &> /dev/null; then
        NODE=node
    else
        warn "Node.js not found - skipping tests"
        return
    fi
    
    echo "  Running: $NODE scripts/test-wasm-integration.mjs"
    echo ""
    
    $NODE scripts/test-wasm-integration.mjs
}

# Print summary
print_summary() {
    section "Build Summary"
    
    local WASM_TARGET="$WEB_APP_WASM_DIR/jl4-core.wasm"
    local JS_TARGET="$WEB_APP_WASM_DIR/jl4-core.mjs"
    
    if [[ "$OSTYPE" == "darwin"* ]]; then
        local FINAL_SIZE=$(stat -f%z "$WASM_TARGET")
    else
        local FINAL_SIZE=$(stat --format=%s "$WASM_TARGET")
    fi
    local FINAL_SIZE_MB=$(echo "scale=1; $FINAL_SIZE / 1048576" | bc)
    
    echo ""
    echo "  Output files:"
    echo "    WASM: $WASM_TARGET (${FINAL_SIZE_MB} MB)"
    echo "    JS:   $JS_TARGET"
    echo ""
    
    if $DO_OPTIMIZE; then
        echo "  Optimization: $OPT_LEVEL applied"
    else
        echo "  Optimization: not applied (use --optimize to enable)"
    fi
    
    echo ""
    success "Build complete!"
    echo ""
    echo "  Next steps:"
    echo "    1. Update VITE_WASM_VERSION in .env.local to bust browser cache"
    echo "    2. Run: cd ts-apps/jl4-web && npm run dev"
    echo "    3. Test in browser or run: node scripts/test-wasm-integration.mjs"
}

# Show usage
usage() {
    echo "WASM Build Script"
    echo ""
    echo "Builds the L4 WASM module and deploys it to the web app."
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --optimize       Optimize the WASM binary after building (default: -Oz)"
    echo "  --opt-level LVL  Optimization level (-O1, -O2, -O3, -Os, -Oz)"
    echo "  --test           Run integration tests after building"
    echo "  --all            Build, optimize, and test"
    echo "  --help           Show this help message"
    echo ""
    echo "Environment:"
    echo "  GHC_WASM_ENV     Path to GHC WASM env file (default: ~/.ghc-wasm/env)"
    echo ""
    echo "Examples:"
    echo "  $0                         # Build only"
    echo "  $0 --optimize              # Build and optimize with -Oz"
    echo "  $0 --optimize --test       # Build, optimize, and test"
    echo "  $0 --all                   # Same as --optimize --test"
    echo "  $0 --opt-level -Os         # Build and optimize with -Os"
}

# Parse arguments
parse_args() {
    while [ $# -gt 0 ]; do
        case "$1" in
            --optimize)
                DO_OPTIMIZE=true
                ;;
            --opt-level)
                shift
                OPT_LEVEL="$1"
                DO_OPTIMIZE=true
                ;;
            --test)
                DO_TEST=true
                ;;
            --all)
                DO_OPTIMIZE=true
                DO_TEST=true
                ;;
            --help|-h)
                usage
                exit 0
                ;;
            *)
                echo -e "${RED}Unknown option: $1${NC}"
                echo ""
                usage
                exit 1
                ;;
        esac
        shift
    done
}

# Main
main() {
    echo -e "${BLUE}╔═══════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║       L4 WASM Build Script                ║${NC}"
    echo -e "${BLUE}╚═══════════════════════════════════════════╝${NC}"
    
    parse_args "$@"
    
    check_prerequisites
    build_wasm
    find_wasm_output
    generate_js_ffi
    deploy_to_webapp
    
    if $DO_OPTIMIZE; then
        optimize_wasm
    fi
    
    if $DO_TEST; then
        run_tests
    fi
    
    print_summary
}

main "$@"
