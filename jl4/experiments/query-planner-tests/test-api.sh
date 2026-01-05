#!/usr/bin/env bash
# Test runner for query planner API tests
# Usage: ./test-api.sh [function-name-pattern] [binding-json]
#
# Examples:
#   ./test-api.sh "simple and"
#   ./test-api.sh "simple and" '{"label": {"a": true}}'
#   ./test-api.sh "may purchase alcohol" '{"label": {"age_21_plus": true}}'

set -euo pipefail

BASE_URL="${BASE_URL:-http://localhost:8001}"
FUNCTION_PATTERN="${1:-}"
BINDINGS="${2:-{}}"

if [ -z "$FUNCTION_PATTERN" ]; then
  echo "Usage: $0 <function-name-pattern> [bindings-json]"
  echo ""
  echo "Examples:"
  echo "  $0 'simple and'"
  echo "  $0 'simple and' '{\"label\": {\"a\": true}}'"
  echo "  $0 'may purchase alcohol' '{\"label\": {\"age_21_plus\": true}}'"
  echo ""
  echo "Available functions:"
  curl -s "$BASE_URL/functions" | jq -r '.[] | "  \(.function.name) - \(.function.description // "no description")"' | head -20
  exit 1
fi

echo "==================================================================="
echo "Testing: $FUNCTION_PATTERN"
echo "Bindings: $BINDINGS"
echo "==================================================================="
echo ""

# Get list of functions
echo "Step 1: Finding function..."
FUNCTIONS_RESPONSE=$(curl -s "$BASE_URL/functions")

# Try to find function by description or name containing the pattern (case-insensitive)
# Convert pattern to lowercase for comparison
PATTERN_LOWER=$(echo "$FUNCTION_PATTERN" | tr '[:upper:]' '[:lower:]')
FUNCTION_NAME=$(echo "$FUNCTIONS_RESPONSE" | jq -r --arg pattern "$PATTERN_LOWER" '.[] | select(.function.description != null and (.function.description | tostring | ascii_downcase | contains($pattern)) or (.function.name | tostring | ascii_downcase | contains($pattern))) | .function.name' | head -1)

if [ -z "$FUNCTION_NAME" ] || [ "$FUNCTION_NAME" = "null" ]; then
  echo "ERROR: Could not find function matching '$FUNCTION_PATTERN'"
  echo ""
  echo "Available functions:"
  echo "$FUNCTIONS_RESPONSE" | jq -r '.[] | "  \(.function.name) - \(.function.description // "no description")"'
  echo ""
  echo "Is the decision service running? Try:"
  echo "  cd jl4/experiments/query-planner-tests"
  echo "  ./start-decision-service.sh"
  exit 1
fi

echo "Found function: $FUNCTION_NAME"
echo ""

# URL-encode function name for API calls
FUNCTION_NAME_ENCODED=$(echo -n "$FUNCTION_NAME" | jq -sRr @uri)

# Get function metadata
echo "Step 2: Function metadata..."
FUNC_META=$(curl -s "$BASE_URL/functions/$FUNCTION_NAME_ENCODED")
echo "$FUNC_META" | jq '{name: .function.name, description: .function.description, parameters: .function.parameters.properties | to_entries | map({name: .key, type: .value.type, description: .value.description})}'
echo ""

# Call query-plan endpoint
echo "Step 3: Query plan with bindings: $BINDINGS"
QUERY_PLAN_RESPONSE=$(curl -s -X POST "$BASE_URL/functions/$FUNCTION_NAME_ENCODED/query-plan" \
  -H "Content-Type: application/json" \
  -d "$BINDINGS")

echo "$QUERY_PLAN_RESPONSE" | jq '.'
echo ""

# Parse and display key results
echo "==================================================================="
echo "SUMMARY"
echo "==================================================================="

OUTCOME=$(echo "$QUERY_PLAN_RESPONSE" | jq -r '.outcome')
STILL_NEEDED=$(echo "$QUERY_PLAN_RESPONSE" | jq -r '.stillNeeded | length')
DONT_CARE_LIST=$(echo "$QUERY_PLAN_RESPONSE" | jq -r '.impact | to_entries | map(select(.value == 0)) | map(.key) | join(", ")')
NEXT_ASKS=$(echo "$QUERY_PLAN_RESPONSE" | jq -r '.asks[0].label // "none"')

echo "Outcome: $OUTCOME"
echo "Still needed ($STILL_NEEDED vars): $(echo "$QUERY_PLAN_RESPONSE" | jq -r '.stillNeeded | join(", ")')"
echo "Don't care: ${DONT_CARE_LIST:-none}"
echo "Next to ask: $NEXT_ASKS"
echo ""

# Display impact scores
echo "Impact scores:"
echo "$QUERY_PLAN_RESPONSE" | jq -r '.impact | to_entries | sort_by(-.value) | .[] | "  \(.key): \(.value)"'
echo ""

# Display asks with schema info
ASKS_COUNT=$(echo "$QUERY_PLAN_RESPONSE" | jq -r '.asks | length')
if [ "$ASKS_COUNT" -gt 0 ]; then
  echo "Recommended questions (in priority order):"
  echo "$QUERY_PLAN_RESPONSE" | jq -r '.asks[] | "  [\(.label)] atoms: \(.atoms | join(", "))"'
  echo ""
fi

echo "==================================================================="
echo "Function: $FUNCTION_NAME"
echo "To test other bindings, run:"
echo "  $0 '$FUNCTION_PATTERN' '{\"label\": {\"var_name\": true}}'"
echo "==================================================================="
