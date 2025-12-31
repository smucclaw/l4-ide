#!/usr/bin/env bash
# L4 Validation Wrapper Script
# Provides convenient validation of L4 files with jl4-cli

set -euo pipefail

# Check if file argument provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <file.l4> [--fixed-now=YYYY-MM-DDTHH:MM:SSZ]"
    echo ""
    echo "Examples:"
    echo "  $0 myfile.l4"
    echo "  $0 myfile.l4 --fixed-now=2025-01-01T00:00:00Z"
    exit 1
fi

L4_FILE="$1"
shift  # Remove first argument

# Check if file exists
if [ ! -f "$L4_FILE" ]; then
    echo "Error: File '$L4_FILE' not found"
    exit 1
fi

# Check if file has .l4 extension
if [[ ! "$L4_FILE" =~ \.l4$ ]]; then
    echo "Warning: File does not have .l4 extension"
fi

echo "Validating L4 file: $L4_FILE"
echo "Running jl4-cli..."
echo ""

# Run jl4-cli with provided file and any additional arguments
if command -v jl4-cli &> /dev/null; then
    # jl4-cli is in PATH
    jl4-cli "$@" "$L4_FILE"
elif command -v cabal &> /dev/null; then
    # Use cabal to run jl4-cli
    cabal run jl4-cli -- "$@" "$L4_FILE"
else
    echo "Error: Neither jl4-cli nor cabal found in PATH"
    echo "Please install jl4-cli or Haskell's cabal build tool"
    exit 1
fi

EXIT_CODE=$?

echo ""
if [ $EXIT_CODE -eq 0 ]; then
    echo "✓ Validation successful!"
else
    echo "✗ Validation failed with exit code $EXIT_CODE"
    echo ""
    echo "Common issues:"
    echo "  - Type mismatches (passing wrong types to functions)"
    echo "  - Undefined functions or types"
    echo "  - Incomplete pattern matches (missing OTHERWISE)"
    echo "  - Indentation/layout errors"
    echo ""
    echo "Check the error messages above for specific problems."
fi

exit $EXIT_CODE
