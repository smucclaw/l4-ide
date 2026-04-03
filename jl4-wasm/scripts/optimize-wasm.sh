#!/bin/bash
# WASM Binary Optimization Script
# Requires: wasm-opt (from binaryen package)
#   macOS: brew install binaryen
#   Ubuntu: apt install binaryen
#   Or: npm install -g binaryen
#
# Usage:
#   ./scripts/optimize-wasm.sh                    # Default: -Os optimization
#   ./scripts/optimize-wasm.sh -O2                # Use specific opt level
#   ./scripts/optimize-wasm.sh --all              # Compare all optimization levels

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
WASM_DIR="$PROJECT_ROOT/ts-apps/jl4-web/static/wasm"
WASM_FILE="$WASM_DIR/jl4-core.wasm"

# Check if wasm-opt is available
check_wasm_opt() {
    if ! command -v wasm-opt &> /dev/null; then
        echo -e "${RED}Error: wasm-opt not found${NC}"
        echo ""
        echo "Install binaryen to get wasm-opt:"
        echo "  macOS:  brew install binaryen"
        echo "  Ubuntu: sudo apt install binaryen"
        echo "  npm:    npm install -g binaryen"
        exit 1
    fi
    echo -e "${GREEN}✓ wasm-opt found:${NC} $(wasm-opt --version)"
}

# Format size for human readability
format_size() {
    local bytes=$1
    if [ $bytes -ge 1073741824 ]; then
        echo "$(echo "scale=2; $bytes / 1073741824" | bc) GB"
    elif [ $bytes -ge 1048576 ]; then
        echo "$(echo "scale=2; $bytes / 1048576" | bc) MB"
    elif [ $bytes -ge 1024 ]; then
        echo "$(echo "scale=2; $bytes / 1024" | bc) KB"
    else
        echo "$bytes B"
    fi
}

# Get file size in bytes
get_size() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        stat -f%z "$1"
    else
        stat --format=%s "$1"
    fi
}

# Optimize with a single level
optimize_single() {
    local opt_level="${1:--Os}"
    
    if [ ! -f "$WASM_FILE" ]; then
        echo -e "${RED}Error: WASM file not found: $WASM_FILE${NC}"
        echo "Build the WASM binary first:"
        echo "  source ~/.ghc-wasm/env"
        echo "  wasm32-wasi-cabal build jl4-wasm --project-file=cabal-wasm.project"
        exit 1
    fi
    
    local original_size=$(get_size "$WASM_FILE")
    
    echo -e "${BLUE}Optimizing WASM binary...${NC}"
    echo "  Input:  $WASM_FILE"
    echo "  Size:   $(format_size $original_size)"
    echo "  Level:  $opt_level"
    echo ""
    
    # Backup original
    cp "$WASM_FILE" "$WASM_FILE.backup"
    
    # Run optimization
    echo -e "${YELLOW}Running wasm-opt $opt_level ...${NC}"
    wasm-opt $opt_level "$WASM_FILE.backup" -o "$WASM_FILE"
    
    local new_size=$(get_size "$WASM_FILE")
    local saved=$((original_size - new_size))
    local percent=$(echo "scale=1; ($saved * 100) / $original_size" | bc)
    
    echo ""
    echo -e "${GREEN}✓ Optimization complete${NC}"
    echo "  Original: $(format_size $original_size)"
    echo "  Optimized: $(format_size $new_size)"
    echo "  Saved: $(format_size $saved) ($percent%)"
    
    # Cleanup backup
    rm "$WASM_FILE.backup"
}

# Compare all optimization levels
compare_all() {
    if [ ! -f "$WASM_FILE" ]; then
        echo -e "${RED}Error: WASM file not found: $WASM_FILE${NC}"
        exit 1
    fi
    
    local original_size=$(get_size "$WASM_FILE")
    
    echo -e "${BLUE}Comparing WASM optimization levels${NC}"
    echo "  Input: $WASM_FILE"
    echo "  Original size: $(format_size $original_size)"
    echo ""
    echo "| Level | Size | Reduction | Time |"
    echo "|-------|------|-----------|------|"
    
    # Backup original
    cp "$WASM_FILE" "$WASM_FILE.original"
    
    for level in O1 O2 O3 Os Oz; do
        # Restore original
        cp "$WASM_FILE.original" "$WASM_FILE"
        
        # Time the optimization
        local start_time=$(date +%s)
        wasm-opt -$level "$WASM_FILE" -o "$WASM_FILE.opt" 2>/dev/null
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        
        local opt_size=$(get_size "$WASM_FILE.opt")
        local saved=$((original_size - opt_size))
        local percent=$(echo "scale=1; ($saved * 100) / $original_size" | bc)
        
        printf "| -%-4s | %s | -%s (%s%%) | %ds |\n" \
            "$level" \
            "$(format_size $opt_size)" \
            "$(format_size $saved)" \
            "$percent" \
            "$duration"
        
        rm "$WASM_FILE.opt"
    done
    
    # Restore original
    mv "$WASM_FILE.original" "$WASM_FILE"
    
    echo ""
    echo -e "${YELLOW}Note: Use './optimize-wasm.sh -Os' to apply optimization (recommended)${NC}"
}

# Test the optimized binary still works
test_wasm() {
    echo ""
    echo -e "${BLUE}Testing optimized WASM binary...${NC}"
    
    # Check if Node.js is available
    local NODE=""
    if [ -f ~/.ghc-wasm/nodejs/bin/node ]; then
        NODE=~/.ghc-wasm/nodejs/bin/node
    elif command -v node &> /dev/null; then
        NODE=node
    else
        echo -e "${YELLOW}Skipping test: Node.js not found${NC}"
        return
    fi
    
    # Quick validation test
    $NODE -e "
        const fs = require('fs');
        const wasmBuffer = fs.readFileSync('$WASM_FILE');
        WebAssembly.compile(wasmBuffer).then(module => {
            const exports = WebAssembly.Module.exports(module).map(e => e.name);
            const required = ['l4_check', 'l4_hover', 'l4_completions'];
            const missing = required.filter(r => !exports.includes(r));
            if (missing.length > 0) {
                console.error('Missing exports:', missing);
                process.exit(1);
            }
            console.log('  ✓ WASM module loads successfully');
            console.log('  ✓ Required exports present');
        }).catch(e => {
            console.error('Error:', e);
            process.exit(1);
        });
    "
}

# Show usage
usage() {
    echo "WASM Binary Optimization Script"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -O1, -O2, -O3    Apply specific optimization level"
    echo "  -Os              Optimize for size (default, recommended)"
    echo "  -Oz              Optimize aggressively for size"
    echo "  --all            Compare all optimization levels (no changes)"
    echo "  --test           Test the WASM binary after optimization"
    echo "  --help           Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                    # Apply -Os optimization"
    echo "  $0 -Os --test         # Optimize and test"
    echo "  $0 --all              # Compare all levels"
}

# Main
main() {
    check_wasm_opt
    echo ""
    
    local opt_level="-Os"
    local run_test=false
    local compare=false
    
    while [ $# -gt 0 ]; do
        case "$1" in
            -O1|-O2|-O3|-Os|-Oz)
                opt_level="$1"
                ;;
            --all)
                compare=true
                ;;
            --test)
                run_test=true
                ;;
            --help|-h)
                usage
                exit 0
                ;;
            *)
                echo -e "${RED}Unknown option: $1${NC}"
                usage
                exit 1
                ;;
        esac
        shift
    done
    
    if $compare; then
        compare_all
    else
        optimize_single "$opt_level"
        if $run_test; then
            test_wasm
        fi
    fi
}

main "$@"
