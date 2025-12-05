#!/usr/bin/env bash
# Quick start script for local development
# Usage: ./dev-start.sh [mode]
# Modes: decision-only | full | websessions-test

set -euo pipefail

MODE="${1:-full}"
DB_PATH="${JL4_DB_PATH:-/tmp/sessions.db}"
DECISION_PORT="${JL4_DECISION_PORT:-8001}"
WEBSESSIONS_PORT="${JL4_WEBSESSIONS_PORT:-8002}"

case "$MODE" in
  decision-only)
    echo "Starting decision service only (port $DECISION_PORT)..."
    cd jl4-decision-service
    cabal run jl4-decision-service-exe -- \
      --port "$DECISION_PORT" \
      --sourcePaths ../jl4/examples
    ;;

  websessions-only)
    echo "Starting websessions only (port $WEBSESSIONS_PORT)..."
    cd jl4-websessions
    cabal run jl4-websessions -- "$WEBSESSIONS_PORT" "$DB_PATH"
    ;;

  websessions-with-push)
    echo "Starting websessions with decision service push..."
    cd jl4-websessions
    cabal run jl4-websessions -- "$WEBSESSIONS_PORT" "$DB_PATH" "http://localhost:$DECISION_PORT"
    ;;

  full)
    echo "Starting full stack with integration..."
    echo "Note: Run this in three separate terminals, or use a tool like tmux"
    echo ""
    echo "Terminal 1 - Decision Service:"
    echo "  cd jl4-decision-service && cabal run jl4-decision-service-exe -- \\"
    echo "    --port $DECISION_PORT --sourcePaths ../jl4/examples \\"
    echo "    --crudServerName localhost --crudServerPort $WEBSESSIONS_PORT"
    echo ""
    echo "Terminal 2 - Websessions (after decision service starts):"
    echo "  cd jl4-websessions && cabal run jl4-websessions -- \\"
    echo "    $WEBSESSIONS_PORT $DB_PATH http://localhost:$DECISION_PORT"
    echo ""
    echo "Terminal 3 - Web Frontend:"
    echo "  cd ts-apps/jl4-web && npm run dev"
    ;;

  websessions-test)
    echo "Testing websessions → decision service integration..."

    # Check decision service is running
    if ! curl -s "http://localhost:$DECISION_PORT/functions" > /dev/null 2>&1; then
      echo "Error: Decision service not running on port $DECISION_PORT"
      echo "Start it first with: ./dev-start.sh decision-only"
      exit 1
    fi

    echo "✓ Decision service is running"

    # Start websessions with push
    echo "Starting websessions with push enabled..."
    cd jl4-websessions
    cabal run jl4-websessions -- "$WEBSESSIONS_PORT" "$DB_PATH" "http://localhost:$DECISION_PORT"
    ;;

  *)
    echo "Unknown mode: $MODE"
    echo "Usage: $0 [mode]"
    echo "Modes:"
    echo "  decision-only         - Start only decision service"
    echo "  websessions-only      - Start only websessions (no push)"
    echo "  websessions-with-push - Start websessions with push to decision service"
    echo "  websessions-test      - Test websessions→decision integration"
    echo "  full                  - Show commands for full stack (default)"
    exit 1
    ;;
esac
