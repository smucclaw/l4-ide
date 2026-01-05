#!/usr/bin/env bash
# Run all query planner tests systematically
# This script tests each L4 file with various bindings to verify query planner behavior

set -euo pipefail

BASE_URL="${BASE_URL:-http://localhost:8001}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}======================================================================${NC}"
echo -e "${BLUE}Query Planner Test Suite${NC}"
echo -e "${BLUE}======================================================================${NC}"
echo ""

# Check if decision service is running
if ! curl -s "$BASE_URL/functions" > /dev/null 2>&1; then
  echo -e "${RED}ERROR: Decision service not responding at $BASE_URL${NC}"
  echo ""
  echo "Please start the decision service with test files:"
  echo "  cd jl4-decision-service"
  echo "  cabal run jl4-decision-service-exe -- --port 8001 \\"
  echo "    --sourcePaths ../jl4/experiments/query-planner-tests"
  echo ""
  exit 1
fi

echo -e "${GREEN}✓ Decision service is running${NC}"
echo ""

# Helper function to run a test
run_test() {
  local test_name="$1"
  local bindings="$2"
  local expected="$3"

  echo -e "${YELLOW}TEST: $test_name${NC}"
  echo "  Bindings: $bindings"
  echo "  Expected: $expected"

  # Find function ID for this test
  local functions_json=$(curl -s "$BASE_URL/functions")
  local function_id=$(echo "$functions_json" | jq -r ".[0].id" | head -1)

  if [ -z "$function_id" ] || [ "$function_id" = "null" ]; then
    echo -e "${RED}  ✗ FAILED: Could not find function${NC}"
    return 1
  fi

  # Call query-plan
  local response=$(curl -s -X POST "$BASE_URL/functions/$function_id/query-plan" \
    -H "Content-Type: application/json" \
    -d "$bindings")

  local outcome=$(echo "$response" | jq -r '.outcome')
  local still_needed=$(echo "$response" | jq -r '.stillNeeded | join(", ")')

  echo "  Outcome: $outcome"
  echo "  Still needed: $still_needed"

  # Simple validation (you can make this more sophisticated)
  if echo "$expected" | grep -q "$outcome"; then
    echo -e "${GREEN}  ✓ PASSED${NC}"
  else
    echo -e "${RED}  ✗ FAILED: Expected to match '$expected', got '$outcome'${NC}"
  fi

  echo ""
}

echo -e "${BLUE}======================================================================${NC}"
echo -e "${BLUE}Level 1: Simple Boolean Logic${NC}"
echo -e "${BLUE}======================================================================${NC}"
echo ""

echo -e "${YELLOW}01-simple-and.l4 - Basic AND operation${NC}"
echo "Test: No bindings (should ask for both a and b)"
run_test "01-simple-and no bindings" '{}' 'Unknown'

echo "Test: a=false (should determine False, short-circuit)"
run_test "01-simple-and a=false" '{"label": {"a": false}}' 'False'

echo "Test: a=true (should still need b)"
run_test "01-simple-and a=true" '{"label": {"a": true}}' 'Unknown'

echo "Test: a=true, b=true (should determine True)"
run_test "01-simple-and both true" '{"label": {"a": true, "b": true}}' 'True'

echo ""
echo -e "${YELLOW}01-simple-or.l4 - Basic OR operation${NC}"
echo "Test: No bindings (should ask for both a and b)"
run_test "01-simple-or no bindings" '{}' 'Unknown'

echo "Test: a=true (should determine True, short-circuit)"
run_test "01-simple-or a=true" '{"label": {"a": true}}' 'True'

echo "Test: a=false (should still need b)"
run_test "01-simple-or a=false" '{"label": {"a": false}}' 'Unknown'

echo "Test: a=false, b=false (should determine False)"
run_test "01-simple-or both false" '{"label": {"a": false, "b": false}}' 'False'

echo ""
echo -e "${BLUE}======================================================================${NC}"
echo -e "${BLUE}Level 3: Don't-Care Detection${NC}"
echo -e "${BLUE}======================================================================${NC}"
echo ""

echo -e "${YELLOW}03-dont-care-simple.l4 - (a AND b) OR (c AND d)${NC}"
echo "Test: a=true, b=true (should determine True, c and d don't care)"
run_test "03-dont-care left branch true" '{"label": {"a": true, "b": true}}' 'True'

echo "Test: a=false (should need c and d, b is don't care)"
run_test "03-dont-care a false" '{"label": {"a": false}}' 'Unknown'

echo ""
echo -e "${BLUE}======================================================================${NC}"
echo -e "${BLUE}Level 4: Complex Nested Logic${NC}"
echo -e "${BLUE}======================================================================${NC}"
echo ""

echo -e "${YELLOW}04-alcohol-purchase.l4 - Realistic example${NC}"
echo "Test: age_21_plus=true (parental approval and emancipation don't care)"
run_test "04-alcohol age 21+" '{"label": {"age_21_plus": true}}' 'Unknown'

echo "Test: age_21_plus=false (married, spousal_approval, beer_only don't care)"
run_test "04-alcohol under 21" '{"label": {"age_21_plus": false}}' 'Unknown'

echo "Test: age_21_plus=true, married=false (spousal_approval don't care)"
run_test "04-alcohol 21+ unmarried" '{"label": {"age_21_plus": true, "married": false}}' 'True'

echo ""
echo -e "${BLUE}======================================================================${NC}"
echo -e "${GREEN}Test suite completed!${NC}"
echo -e "${BLUE}======================================================================${NC}"
echo ""
echo "For interactive testing, use:"
echo "  ./test-api.sh <test-file.l4> '{\"label\": {\"var\": true}}'"
