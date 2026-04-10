#!/usr/bin/env bash
# Health check script for L4 development services
# Tests actual service functionality, not just port availability

set -euo pipefail

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color
BOLD='\033[1m'

# Configuration from environment or defaults
WEBSESSIONS_PORT="${JL4_WEBSESSIONS_PORT:-8002}"
LSP_PORT="${JL4_LSP_PORT:-8000}"
SERVICE_PORT="${JL4_SERVICE_PORT:-8080}"
WEB_IDE_PORT="${JL4_WEB_IDE_PORT:-5173}"
TIMEOUT="${HEALTHCHECK_TIMEOUT:-5}"

# Track overall health
ALL_HEALTHY=true

# Helper functions
print_header() {
    echo -e "${BOLD}L4 Development Services Health Check${NC}"
    echo "======================================"
    echo
}

print_result() {
    local service="$1"
    local status="$2"
    local details="$3"

    if [ "$status" = "OK" ]; then
        echo -e "${GREEN}✓${NC} ${BOLD}${service}${NC} - ${details}"
    else
        echo -e "${RED}✗${NC} ${BOLD}${service}${NC} - ${details}"
        ALL_HEALTHY=false
    fi
}

check_websessions() {
    local url="http://localhost:${WEBSESSIONS_PORT}/session"

    # Try to GET the session list endpoint
    if response=$(curl -s --max-time "$TIMEOUT" "$url" 2>&1); then
        # Websessions should return JSON array
        if echo "$response" | jq -e 'type == "array"' >/dev/null 2>&1; then
            print_result "Websessions" "OK" "port $WEBSESSIONS_PORT (CRUD API responding)"
        else
            print_result "Websessions" "FAIL" "port $WEBSESSIONS_PORT (unexpected response format)"
        fi
    else
        print_result "Websessions" "FAIL" "port $WEBSESSIONS_PORT (not responding)"
    fi
}

check_lsp_server() {
    # LSP runs on WebSocket - check if port is listening
    if timeout "$TIMEOUT" bash -c "echo > /dev/tcp/localhost/${LSP_PORT}" 2>/dev/null; then
        print_result "LSP Server" "OK" "port $LSP_PORT (WebSocket listening)"
    else
        print_result "LSP Server" "FAIL" "port $LSP_PORT (not listening)"
    fi
}

check_jl4_service() {
    local url="http://localhost:${SERVICE_PORT}/health"

    if response=$(curl -s --max-time "$TIMEOUT" "$url" 2>&1); then
        if echo "$response" | jq -e '.status == "healthy"' >/dev/null 2>&1; then
            print_result "jl4-service" "OK" "port $SERVICE_PORT (/health responding)"
        else
            print_result "jl4-service" "FAIL" "port $SERVICE_PORT (unexpected /health response)"
        fi
    else
        print_result "jl4-service" "FAIL" "port $SERVICE_PORT (not responding)"
    fi
}

check_web_ide() {
    local url="http://localhost:${WEB_IDE_PORT}/"

    if response=$(curl -s --max-time "$TIMEOUT" "$url" 2>&1); then
        # Check for expected HTML content (should contain "jl4" or "L4")
        if echo "$response" | grep -iq "jl4\|<!DOCTYPE html>"; then
            print_result "Web IDE (jl4-web)" "OK" "port $WEB_IDE_PORT (serving pages)"
        else
            print_result "Web IDE (jl4-web)" "FAIL" "port $WEB_IDE_PORT (unexpected content)"
        fi
    else
        print_result "Web IDE (jl4-web)" "FAIL" "port $WEB_IDE_PORT (not responding)"
    fi
}

print_footer() {
    echo
    if [ "$ALL_HEALTHY" = true ]; then
        echo -e "${GREEN}${BOLD}All services healthy ✓${NC}"
        echo
        echo "Ready for development!"
        echo
        echo "Services accessible at:"
        echo "  LSP:         ws://localhost:${LSP_PORT}"
        echo "  jl4-service: http://localhost:${SERVICE_PORT}"
        echo "  Websessions: http://localhost:${WEBSESSIONS_PORT}"
        echo "  Web IDE:     http://localhost:${WEB_IDE_PORT}"
        return 0
    else
        echo -e "${RED}${BOLD}Some services are unhealthy ✗${NC}"
        echo
        echo "Troubleshooting:"
        echo "  1. Start services: ./dev-start.sh full --run"
        echo "  2. Check logs in /tmp/jl4-*.log"
        echo "  3. Verify ports not in use: lsof -i :${WEBSESSIONS_PORT},${LSP_PORT},${SERVICE_PORT},${WEB_IDE_PORT}"
        return 1
    fi
}

# Main execution
main() {
    print_header

    # Run checks in order
    check_lsp_server
    check_jl4_service
    check_websessions
    check_web_ide

    # Print summary
    print_footer
}

# Check for required tools
for tool in curl jq timeout; do
    if ! command -v "$tool" &> /dev/null; then
        echo -e "${RED}Error: Required tool '$tool' not found${NC}"
        echo "Install with: brew install $tool (macOS) or apt install $tool (Linux)"
        exit 1
    fi
done

# Run main function
main
