#!/usr/bin/env bash
# Stop all L4 dev services started with dev-start.sh full --run

set -euo pipefail

PIDFILE="${JL4_PIDFILE:-/tmp/jl4-dev.pid}"

if [[ ! -f "$PIDFILE" ]]; then
  echo "No PID file found at $PIDFILE"
  echo "Services may not be running or were started manually."
  exit 1
fi

echo "Stopping all L4 dev services..."

while IFS='=' read -r name pid; do
  if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
    echo "  Stopping $name (PID: $pid)..."
    kill "$pid" 2>/dev/null || true
  else
    echo "  $name (PID: $pid) - not running"
  fi
done < "$PIDFILE"

echo ""
echo "Waiting for processes to terminate..."
sleep 2

# Force kill any that didn't stop
while IFS='=' read -r name pid; do
  if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
    echo "  Force killing $name (PID: $pid)..."
    kill -9 "$pid" 2>/dev/null || true
  fi
done < "$PIDFILE"

rm -f "$PIDFILE"
echo ""
echo "✓ All services stopped"
