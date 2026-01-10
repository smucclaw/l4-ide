#!/usr/bin/env bash
# Health check script for L4 development services
# Tests actual service functionality, not just port availability

set -euo pipefail

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color
BOLD='\033[1m'

# Configuration from environment or defaults
DECISION_PORT="${JL4_DECISION_PORT:-8001}"
WEBSESSIONS_PORT="${JL4_WEBSESSIONS_PORT:-8002}"
LSP_PORT="${JL4_LSP_PORT:-8000}"
WEB_IDE_PORT="${JL4_WEB_IDE_PORT:-5173}"
WIZARD_PORT="${JL4_WIZARD_PORT:-5174}"
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

check_decision_service() {
    local url="http://localhost:${DECISION_PORT}/functions"
    
    if response=$(curl -s --max-time "$TIMEOUT" "$url" 2>&1); then
        # Check if response is valid JSON and contains expected structure
        if echo "$response" | jq -e 'type == "array"' >/dev/null 2>&1; then
            local count=$(echo "$response" | jq 'length')
            print_result "Decision Service" "OK" "port $DECISION_PORT ($count functions loaded)"
        else
            print_result "Decision Service" "FAIL" "port $DECISION_PORT (invalid response format)"
        fi
    else
        print_result "Decision Service" "FAIL" "port $DECISION_PORT (not responding)"
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
    # We can't easily test WebSocket from bash without wscat/websocat
    # So we'll do a basic TCP connection test
    if timeout "$TIMEOUT" bash -c "echo > /dev/tcp/localhost/${LSP_PORT}" 2>/dev/null; then
        print_result "LSP Server" "OK" "port $LSP_PORT (WebSocket listening)"
    else
        print_result "LSP Server" "FAIL" "port $LSP_PORT (not listening)"
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

check_wizard() {
    local url="http://localhost:${WIZARD_PORT}/"
    
    if response=$(curl -s --max-time "$TIMEOUT" "$url" 2>&1); then
        # Check for expected HTML content
        if echo "$response" | grep -iq "<!DOCTYPE html>\|wizard"; then
            print_result "L4 Wizard" "OK" "port $WIZARD_PORT (serving pages)"
        else
            print_result "L4 Wizard" "FAIL" "port $WIZARD_PORT (unexpected content)"
        fi
    else
        print_result "L4 Wizard" "FAIL" "port $WIZARD_PORT (not responding)"
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
        echo "  LSP:        ws://localhost:${LSP_PORT}"
        echo "  Decision:   http://localhost:${DECISION_PORT}"
        echo "  Websessions: http://localhost:${WEBSESSIONS_PORT}"
        echo "  Web IDE:    http://localhost:${WEB_IDE_PORT}"
        echo "  Wizard:     http://localhost:${WIZARD_PORT}"
        return 0
    else
        echo -e "${RED}${BOLD}Some services are unhealthy ✗${NC}"
        echo
        echo "Troubleshooting:"
        echo "  1. Start services: ./dev-start.sh full --run"
        echo "  2. Check logs in /tmp/jl4-*.log"
        echo "  3. Verify ports not in use: lsof -i :${DECISION_PORT},${WEBSESSIONS_PORT},${LSP_PORT},${WEB_IDE_PORT},${WIZARD_PORT}"
        return 1
    fi
}

# Main execution
main() {
    print_header
    
    # Run checks in order
    check_lsp_server
    check_decision_service
    check_websessions
    check_web_ide
    check_wizard
    
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
