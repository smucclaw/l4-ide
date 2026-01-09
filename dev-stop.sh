#!/usr/bin/env bash
# Stop all L4 development services started by dev-start.sh

set -euo pipefail

PIDFILE="${JL4_PIDFILE:-/tmp/jl4-dev.pid}"

if [ ! -f "$PIDFILE" ]; then
    echo "No PID file found at $PIDFILE"
    echo "Services may not be running, or were started manually."
    exit 1
fi

echo "Stopping L4 development services..."
echo

# Read PIDs from file and kill processes
while IFS='=' read -r name pid; do
    if [ -n "$pid" ]; then
        if ps -p "$pid" > /dev/null 2>&1; then
            echo "Stopping $name (PID $pid)..."
            kill "$pid" 2>/dev/null || true
            # Wait up to 5 seconds for graceful shutdown
            for i in {1..10}; do
                if ! ps -p "$pid" > /dev/null 2>&1; then
                    break
                fi
                sleep 0.5
            done
            # Force kill if still running
            if ps -p "$pid" > /dev/null 2>&1; then
                echo "  Force killing $name..."
                kill -9 "$pid" 2>/dev/null || true
            fi
        else
            echo "Process $name (PID $pid) not running"
        fi
    fi
done < "$PIDFILE"

# Clean up PID file
rm -f "$PIDFILE"

# Clean up log files
echo
echo "Cleaning up log files..."
rm -f /tmp/jl4-*.log

echo
echo "✓ All services stopped"
