#!/usr/bin/env bash
# Quick start script for local development
# Usage: ./dev-start.sh [mode] [--run]
# Modes: lsp-only | websessions-only | full
# Use 'full --run' to launch all services in background
#
# After starting services, verify health with: ./dev-healthcheck.sh

set -euo pipefail

MODE="${1:-full}"
RUN_FLAG="${2:-}"
DB_PATH="${JL4_DB_PATH:-/tmp/sessions.db}"
WEBSESSIONS_PORT="${JL4_WEBSESSIONS_PORT:-8002}"
LSP_PORT="${JL4_LSP_PORT:-8000}"
LSP_CWD="${JL4_LSP_CWD:-jl4-core/libraries}"
PIDFILE="${JL4_PIDFILE:-/tmp/jl4-dev.pid}"

case "$MODE" in
  websessions-only)
    echo "Starting websessions only (port $WEBSESSIONS_PORT)..."
    cd jl4-websessions
    cabal run jl4-websessions -- "$WEBSESSIONS_PORT" "$DB_PATH"
    ;;

  lsp-only)
    echo "Starting LSP server only (port $LSP_PORT)..."
    cabal run exe:jl4-lsp -- ws --host 0.0.0.0 --port "$LSP_PORT" --cwd "$LSP_CWD"
    ;;

  full)
    if [[ "$RUN_FLAG" == "--run" ]]; then
      echo "Starting full stack in background..."

      # Remove old PID file
      rm -f "$PIDFILE"

      # Start LSP Server
      echo "Starting LSP server on port $LSP_PORT..."
      cabal run exe:jl4-lsp -- ws --host 0.0.0.0 --port "$LSP_PORT" --cwd "$LSP_CWD" > /tmp/jl4-lsp.log 2>&1 &
      echo "LSP_PID=$!" >> "$PIDFILE"

      # Start Websessions
      echo "Starting websessions on port $WEBSESSIONS_PORT..."
      (cd jl4-websessions && cabal run jl4-websessions -- \
        "$WEBSESSIONS_PORT" "$DB_PATH") > /tmp/jl4-websessions.log 2>&1 &
      echo "WEBSESSIONS_PID=$!" >> "$PIDFILE"

      # Start Web Frontend (IDE)
      echo "Starting web IDE frontend..."
      (cd ts-apps/jl4-web && npm run dev) > /tmp/jl4-web.log 2>&1 &
      echo "WEB_PID=$!" >> "$PIDFILE"

      echo ""
      echo "✓ All services started in background!"
      echo ""
      echo "Services:"
      echo "  LSP Server:      http://localhost:$LSP_PORT (ws://localhost:$LSP_PORT)"
      echo "  Websessions:     http://localhost:$WEBSESSIONS_PORT"
      echo "  Web IDE:         http://localhost:5173"
      echo ""
      echo "Logs:"
      echo "  LSP:         tail -f /tmp/jl4-lsp.log"
      echo "  Websessions: tail -f /tmp/jl4-websessions.log"
      echo "  Web IDE:     tail -f /tmp/jl4-web.log"
      echo ""
      echo "PIDs saved to: $PIDFILE"
      echo ""
      echo "To verify all services are healthy:"
      echo "  ./dev-healthcheck.sh"
      echo ""
      echo "To stop all services:"
      echo "  ./dev-stop.sh"
      echo "  or: kill \$(cat $PIDFILE | cut -d= -f2)"
      echo ""
      echo "Open in your browser:"
      echo "  IDE:    http://localhost:5173"
    else
      echo "Starting full stack with integration..."
      echo "Note: Run these in separate terminals, or use a tool like tmux"
      echo "      Or run './dev-start.sh full --run' to start all in background"
      echo ""
      echo "Terminal 1 - LSP Server:"
      echo "  cabal run exe:jl4-lsp -- ws --host 0.0.0.0 --port $LSP_PORT --cwd $LSP_CWD"
      echo ""
      echo "Terminal 2 - Websessions:"
      echo "  cd jl4-websessions && cabal run jl4-websessions -- \\"
      echo "    $WEBSESSIONS_PORT $DB_PATH"
      echo ""
      echo "Terminal 3 - Web IDE Frontend:"
      echo "  cd ts-apps/jl4-web && npm run dev"
      echo ""
      echo "Then open in your browser:"
      echo "  IDE:    http://localhost:5173"
    fi
    ;;

  *)
    echo "Unknown mode: $MODE"
    echo "Usage: $0 [mode] [--run]"
    echo "Modes:"
    echo "  lsp-only              - Start only LSP server (websocket)"
    echo "  websessions-only      - Start only websessions"
    echo "  full                  - Show commands for full stack (default)"
    echo "  full --run            - Launch all services in background"
    exit 1
    ;;
esac
