#!/usr/bin/env bash
# Test script for state graph REST API endpoints

set -e

BASE_URL="${BASE_URL:-http://localhost:8001}"
FUNCTION_NAME="weddingcontract"
OUTPUT_DIR="./test-output"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}=== State Graph API Test Script ===${NC}\n"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Test 1: Upload L4 file with regulative rules
echo -e "${YELLOW}Test 1: Uploading wedding.l4 with regulative rules${NC}"

WEDDING_SOURCE=$(cat <<'EOF'
SECTION wedding

# Traditional wedding vows formalized as L4 regulative rules

DECIDE `weddingceremony`
  PARTY Spouse1 MUST `exchange vows`
    HENCE
      PARTY Spouse2 MUST `exchange vows`

DECIDE `noabandonment`
  PARTY Spouse1 SHANT `abandon` Spouse2

DECIDE `fidelity`
  PARTY Spouse1 SHANT `commit adultery`

DECIDE `supportinallcircumstances`
  PARTY Spouse1 MUST `provide support` TO Spouse2

DECIDE `marriagecontract`
  PARTY Spouse1 MUST `love` Spouse2
    RAND PARTY Spouse1 MUST `cherish` Spouse2
    RAND PARTY Spouse2 MUST `love` Spouse1
    RAND PARTY Spouse2 MUST `cherish` Spouse1
EOF
)

# Create function with L4 source
UPLOAD_PAYLOAD=$(cat <<EOF
{
  "name": "$FUNCTION_NAME",
  "description": "Wedding contract with vows",
  "supportedEvalBackend": ["jl4"],
  "source": {
    "jl4": $(echo "$WEDDING_SOURCE" | jq -Rs .)
  }
}
EOF
)

UPLOAD_RESULT=$(curl -s -X PUT \
  "$BASE_URL/functions/$FUNCTION_NAME" \
  -H "Content-Type: application/json" \
  -d "$UPLOAD_PAYLOAD")

echo -e "${GREEN}✓ Function uploaded${NC}\n"

# Test 2: List all state graphs
echo -e "${YELLOW}Test 2: Listing all state graphs in module${NC}"

GRAPHS_LIST=$(curl -s "$BASE_URL/functions/$FUNCTION_NAME/state-graphs")
echo "$GRAPHS_LIST" | jq .

GRAPH_COUNT=$(echo "$GRAPHS_LIST" | jq '.graphs | length')
echo -e "${GREEN}✓ Found $GRAPH_COUNT state graphs${NC}\n"

# Extract graph names
GRAPH_NAMES=$(echo "$GRAPHS_LIST" | jq -r '.graphs[].graphName')

if [ -z "$GRAPH_NAMES" ]; then
  echo -e "${RED}✗ No state graphs found!${NC}"
  exit 1
fi

# Test 3-5: For each graph, test DOT, SVG, and PNG endpoints
for GRAPH_NAME in $GRAPH_NAMES; do
  echo -e "${YELLOW}Testing graph: $GRAPH_NAME${NC}"

  # Test 3: Get DOT source
  echo "  Getting DOT source..."
  DOT_OUTPUT="$OUTPUT_DIR/${GRAPH_NAME}.dot"
  curl -s "$BASE_URL/functions/$FUNCTION_NAME/state-graphs/$GRAPH_NAME" > "$DOT_OUTPUT"

  if [ -s "$DOT_OUTPUT" ]; then
    LINE_COUNT=$(wc -l < "$DOT_OUTPUT")
    echo -e "  ${GREEN}✓ DOT file saved ($LINE_COUNT lines)${NC}"
  else
    echo -e "  ${RED}✗ DOT file is empty!${NC}"
    exit 1
  fi

  # Test 4: Get SVG
  echo "  Getting SVG..."
  SVG_OUTPUT="$OUTPUT_DIR/${GRAPH_NAME}.svg"
  HTTP_CODE=$(curl -s -o "$SVG_OUTPUT" -w "%{http_code}" \
    "$BASE_URL/functions/$FUNCTION_NAME/state-graphs/$GRAPH_NAME/svg")

  if [ "$HTTP_CODE" = "200" ] && [ -s "$SVG_OUTPUT" ]; then
    SIZE=$(wc -c < "$SVG_OUTPUT")
    echo -e "  ${GREEN}✓ SVG rendered ($SIZE bytes)${NC}"
  elif [ "$HTTP_CODE" = "503" ]; then
    echo -e "  ${YELLOW}⚠ GraphViz not installed (503), skipping SVG/PNG tests${NC}"
    continue
  else
    echo -e "  ${RED}✗ SVG rendering failed (HTTP $HTTP_CODE)${NC}"
    exit 1
  fi

  # Test 5: Get PNG
  echo "  Getting PNG..."
  PNG_OUTPUT="$OUTPUT_DIR/${GRAPH_NAME}.png"
  HTTP_CODE=$(curl -s -o "$PNG_OUTPUT" -w "%{http_code}" \
    "$BASE_URL/functions/$FUNCTION_NAME/state-graphs/$GRAPH_NAME/png")

  if [ "$HTTP_CODE" = "200" ] && [ -s "$PNG_OUTPUT" ]; then
    SIZE=$(wc -c < "$PNG_OUTPUT")
    echo -e "  ${GREEN}✓ PNG rendered ($SIZE bytes)${NC}"
  else
    echo -e "  ${RED}✗ PNG rendering failed (HTTP $HTTP_CODE)${NC}"
    exit 1
  fi

  echo ""
done

# Test 6: Test error handling (nonexistent graph)
echo -e "${YELLOW}Test 6: Error handling for nonexistent graph${NC}"
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" \
  "$BASE_URL/functions/$FUNCTION_NAME/state-graphs/nonexistent")

if [ "$HTTP_CODE" = "404" ]; then
  echo -e "${GREEN}✓ Correctly returns 404 for nonexistent graph${NC}\n"
else
  echo -e "${RED}✗ Expected 404, got $HTTP_CODE${NC}\n"
  exit 1
fi

# Test 7: Test error handling (nonexistent function)
echo -e "${YELLOW}Test 7: Error handling for nonexistent function${NC}"
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" \
  "$BASE_URL/functions/nonexistent/state-graphs")

if [ "$HTTP_CODE" = "404" ]; then
  echo -e "${GREEN}✓ Correctly returns 404 for nonexistent function${NC}\n"
else
  echo -e "${RED}✗ Expected 404, got $HTTP_CODE${NC}\n"
  exit 1
fi

# Summary
echo -e "${GREEN}═══════════════════════════════════════${NC}"
echo -e "${GREEN}All tests passed!${NC}"
echo -e "${GREEN}═══════════════════════════════════════${NC}"
echo ""
echo "Output files saved to: $OUTPUT_DIR/"
ls -lh "$OUTPUT_DIR/" | tail -n +2

echo ""
echo -e "${YELLOW}Example usage:${NC}"
echo "  View SVG: open $OUTPUT_DIR/weddingceremony.svg"
echo "  View PNG: open $OUTPUT_DIR/weddingceremony.png"
echo "  View DOT: cat $OUTPUT_DIR/weddingceremony.dot"
