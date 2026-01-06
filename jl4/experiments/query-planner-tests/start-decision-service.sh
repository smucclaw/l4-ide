#!/usr/bin/env bash
# Start the decision service with query planner test files
# This loads all test files from this directory

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

echo "======================================================================="
echo "Starting decision service with query planner tests..."
echo "======================================================================="
echo ""
echo "Test files directory: $SCRIPT_DIR"
echo "Project root: $PROJECT_ROOT"
echo ""

cd "$PROJECT_ROOT/jl4-decision-service"

echo "Building and starting decision service on port 8001..."
echo ""

# Build source paths for all .l4 files in the test directory
SOURCE_PATHS=""
for f in "$SCRIPT_DIR"/*.l4; do
  SOURCE_PATHS="$SOURCE_PATHS --sourcePaths $f"
done

echo "Loading test files:"
for f in "$SCRIPT_DIR"/*.l4; do
  echo "  - $(basename "$f")"
done
echo ""

echo "Starting server..."
echo "Press Ctrl+C to stop"
echo ""
echo "======================================================================="
echo ""

cabal run jl4-decision-service-exe -- --port 8001 $SOURCE_PATHS

echo ""
echo "Decision service stopped."
