#!/usr/bin/env bash
# Quick start script for local development
# Usage: ./dev-start.sh [mode] [--run]
# Modes: lsp-only | decision-only | websessions-only | websessions-with-push | websessions-test | full
# Use 'full --run' to launch all services in background

set -euo pipefail

MODE="${1:-full}"
RUN_FLAG="${2:-}"
DB_PATH="${JL4_DB_PATH:-/tmp/sessions.db}"
DECISION_PORT="${JL4_DECISION_PORT:-8001}"
WEBSESSIONS_PORT="${JL4_WEBSESSIONS_PORT:-8002}"
LSP_PORT="${JL4_LSP_PORT:-8000}"
LSP_CWD="${JL4_LSP_CWD:-jl4-core/libraries}"
PIDFILE="${JL4_PIDFILE:-/tmp/jl4-dev.pid}"

case "$MODE" in
  decision-only)
    echo "Starting decision service only (port $DECISION_PORT)..."
    cd jl4-decision-service
    cabal run jl4-decision-service-exe -- \
      --port "$DECISION_PORT" \
      --sourcePaths ../jl4/experiments/britishcitizen5.l4 \
      --sourcePaths ../jl4/experiments/parking.l4
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

  lsp-only)
    echo "Starting LSP server only (port $LSP_PORT)..."
    cabal run exe:jl4-lsp -- ws --port "$LSP_PORT" --cwd "$LSP_CWD"
    ;;

  full)
    if [[ "$RUN_FLAG" == "--run" ]]; then
      echo "Starting full stack in background..."
      
      # Remove old PID file
      rm -f "$PIDFILE"
      
      # Start LSP Server
      echo "Starting LSP server on port $LSP_PORT..."
      cabal run exe:jl4-lsp -- ws --port "$LSP_PORT" --cwd "$LSP_CWD" > /tmp/jl4-lsp.log 2>&1 &
      echo "LSP_PID=$!" >> "$PIDFILE"
      
      # Start Decision Service
      echo "Starting decision service on port $DECISION_PORT..."
      (cd jl4-decision-service && cabal run jl4-decision-service-exe -- \
        --port "$DECISION_PORT" \
        --sourcePaths ../jl4/experiments/britishcitizen5.l4 \
        --sourcePaths ../jl4/experiments/parking.l4 \
        --crudServerName localhost \
        --crudServerPort "$WEBSESSIONS_PORT") > /tmp/jl4-decision.log 2>&1 &
      echo "DECISION_PID=$!" >> "$PIDFILE"
      
      # Wait a bit for decision service to start
      sleep 3
      
      # Start Websessions
      echo "Starting websessions on port $WEBSESSIONS_PORT..."
      (cd jl4-websessions && cabal run jl4-websessions -- \
        "$WEBSESSIONS_PORT" "$DB_PATH" "http://localhost:$DECISION_PORT") > /tmp/jl4-websessions.log 2>&1 &
      echo "WEBSESSIONS_PID=$!" >> "$PIDFILE"
      
      # Start Web Frontend
      echo "Starting web frontend..."
      (cd ts-apps/jl4-web && npm run dev) > /tmp/jl4-web.log 2>&1 &
      echo "WEB_PID=$!" >> "$PIDFILE"
      
      echo ""
      echo "✓ All services started in background!"
      echo ""
      echo "Services:"
      echo "  LSP Server:      http://localhost:$LSP_PORT (ws://localhost:$LSP_PORT)"
      echo "  Decision:        http://localhost:$DECISION_PORT"
      echo "  Websessions:     http://localhost:$WEBSESSIONS_PORT"
      echo "  Web Frontend:    http://localhost:5173"
      echo ""
      echo "Logs:"
      echo "  LSP:        tail -f /tmp/jl4-lsp.log"
      echo "  Decision:   tail -f /tmp/jl4-decision.log"
      echo "  Websessions: tail -f /tmp/jl4-websessions.log"
      echo "  Web:        tail -f /tmp/jl4-web.log"
      echo ""
      echo "PIDs saved to: $PIDFILE"
      echo ""
      echo "To stop all services:"
      echo "  ./dev-stop.sh"
      echo "  or: kill \$(cat $PIDFILE | cut -d= -f2)"
      echo ""
      echo "Open http://localhost:5173 in your browser"
    else
      echo "Starting full stack with integration..."
      echo "Note: Run these in four separate terminals, or use a tool like tmux"
      echo "      Or run './dev-start.sh full --run' to start all in background"
      echo ""
      echo "Terminal 1 - LSP Server:"
      echo "  cabal run exe:jl4-lsp -- ws --port $LSP_PORT --cwd $LSP_CWD"
      echo ""
      echo "Terminal 2 - Decision Service:"
      echo "  cd jl4-decision-service && cabal run jl4-decision-service-exe -- \\"
      echo "    --port $DECISION_PORT \\"
      echo "    --sourcePaths ../jl4/experiments/britishcitizen5.l4 \\"
      echo "    --sourcePaths ../jl4/experiments/parking.l4 \\"
      echo "    --crudServerName localhost --crudServerPort $WEBSESSIONS_PORT"
      echo ""
      echo "Terminal 3 - Websessions (after decision service starts):"
      echo "  cd jl4-websessions && cabal run jl4-websessions -- \\"
      echo "    $WEBSESSIONS_PORT $DB_PATH http://localhost:$DECISION_PORT"
      echo ""
      echo "Terminal 4 - Web Frontend:"
      echo "  cd ts-apps/jl4-web && npm run dev"
      echo ""
      echo "Then open http://localhost:5173 in your browser"
    fi
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
    echo "Usage: $0 [mode] [--run]"
    echo "Modes:"
    echo "  lsp-only              - Start only LSP server (websocket)"
    echo "  decision-only         - Start only decision service"
    echo "  websessions-only      - Start only websessions (no push)"
    echo "  websessions-with-push - Start websessions with push to decision service"
    echo "  websessions-test      - Test websessions→decision integration"
    echo "  full                  - Show commands for full stack (default)"
    echo "  full --run            - Launch all services in background"
    exit 1
    ;;
esac
