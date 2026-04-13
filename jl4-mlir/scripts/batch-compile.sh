#!/bin/bash
# Portable — works with both macOS bash 3.2 and Linux bash 4+.
#
# batch-compile.sh — scan the l4-ide repo for every .l4 file, check it
# with the l4 CLI, and compile passing files to WASM via jl4-mlir.
#
# Usage:
#   scripts/batch-compile.sh [--output-dir DIR] [--keep-going] [--verbose]
#                            [--filter GLOB] [--skip-check]
#
# Defaults:
#   --output-dir ./dist-wasm
#   --keep-going (do not abort on errors)
#
# Output layout:
#   <output-dir>/
#     <rel/path/to/foo>.wasm
#     <rel/path/to/foo>.schema.json
#     <rel/path/to/foo>.log         (per-file compile log, only on failure)
#   <output-dir>/summary.json       (machine-readable summary)
#   <output-dir>/summary.txt        (human-readable summary)

set -o pipefail

# ---------------------------------------------------------------------------
# Arg parsing
# ---------------------------------------------------------------------------

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
OUTPUT_DIR="$REPO_ROOT/dist-wasm"
KEEP_GOING=1
VERBOSE=0
SKIP_CHECK=0
FILTER=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --output-dir) OUTPUT_DIR="$2"; shift 2 ;;
    --keep-going) KEEP_GOING=1; shift ;;
    --stop-on-error) KEEP_GOING=0; shift ;;
    --verbose|-v) VERBOSE=1; shift ;;
    --filter) FILTER="$2"; shift 2 ;;
    --skip-check) SKIP_CHECK=1; shift ;;
    -h|--help)
      sed -n '1,/^# ------/p' "$0" | sed 's/^# \{0,1\}//'
      exit 0 ;;
    *) echo "Unknown flag: $1" >&2; exit 2 ;;
  esac
done

mkdir -p "$OUTPUT_DIR"

# ---------------------------------------------------------------------------
# Toolchain discovery
# ---------------------------------------------------------------------------

# Ensure MLIR/LLVM tools are on PATH so `jl4-mlir wasm` can find them.
for prefix in /opt/homebrew/opt/llvm/bin /opt/homebrew/opt/lld/bin /usr/local/opt/llvm/bin /usr/local/opt/lld/bin; do
  if [[ -d "$prefix" ]]; then
    PATH="$prefix:$PATH"
  fi
done
export PATH

for tool in cabal mlir-opt mlir-translate llc wasm-ld; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "Missing required tool: $tool" >&2
    [[ "$tool" == "cabal" ]] && exit 1
    echo "  (WASM compilation stages that need $tool will fail — continuing with --mlir-only-capable files)" >&2
  fi
done

# ---------------------------------------------------------------------------
# Build the CLIs once, rather than paying cabal's overhead per invocation.
# ---------------------------------------------------------------------------

echo "Building l4 and jl4-mlir CLIs..."
cd "$REPO_ROOT"
cabal build l4 jl4-mlir >/dev/null 2>&1 || {
  echo "Failed to build the CLIs. Run 'cabal build l4 jl4-mlir' for details." >&2
  exit 1
}

L4_BIN="$(cabal list-bin l4 2>/dev/null)"
MLIR_BIN="$(cabal list-bin jl4-mlir 2>/dev/null)"

if [[ ! -x "$L4_BIN" ]] || [[ ! -x "$MLIR_BIN" ]]; then
  echo "Could not locate cabal-built binaries." >&2
  exit 1
fi

# ---------------------------------------------------------------------------
# Discover .l4 files
# ---------------------------------------------------------------------------

# Exclude build artifacts, node_modules, dist folders, and the output dir
# itself so we don't loop over previously-generated artifacts.
L4_LIST_FILE="$(mktemp)"
trap 'rm -f "$L4_LIST_FILE"' EXIT

find "$REPO_ROOT" -type f -name '*.l4' \
  -not -path '*/node_modules/*' \
  -not -path '*/dist-newstyle/*' \
  -not -path '*/dist-wasm/*' \
  -not -path "$OUTPUT_DIR/*" \
  | sort > "$L4_LIST_FILE"

if [[ -n "$FILTER" ]]; then
  grep -F -- "$FILTER" "$L4_LIST_FILE" > "$L4_LIST_FILE.filtered" || true
  mv "$L4_LIST_FILE.filtered" "$L4_LIST_FILE"
fi

TOTAL="$(wc -l < "$L4_LIST_FILE" | tr -d ' ')"
if [[ "$TOTAL" -eq 0 ]]; then
  echo "No .l4 files found."
  exit 0
fi

echo "Found $TOTAL .l4 file(s)."

# ---------------------------------------------------------------------------
# Counters & tracking
# ---------------------------------------------------------------------------

CHECKED_OK=0
CHECK_FAILED=0
COMPILED_OK=0
COMPILE_FAILED=0
SKIPPED_NO_EXPORTS=0

RESULTS_FILE="$(mktemp)"

# ---------------------------------------------------------------------------
# Per-file processing
# ---------------------------------------------------------------------------

while IFS= read -r src; do
  [[ -z "$src" ]] && continue
  rel="${src#$REPO_ROOT/}"
  base="${rel%.l4}"
  out_dir="$OUTPUT_DIR/$(dirname "$rel")"
  mkdir -p "$out_dir"
  log_file="$OUTPUT_DIR/$base.log"

  [[ $VERBOSE -eq 1 ]] && echo "[check ] $rel"

  # Step 1: l4 check (typecheck without evaluating)
  check_status="skipped"
  if [[ $SKIP_CHECK -eq 0 ]]; then
    if "$L4_BIN" check "$src" >"$log_file" 2>&1; then
      CHECKED_OK=$((CHECKED_OK + 1))
      check_status="ok"
    else
      CHECK_FAILED=$((CHECK_FAILED + 1))
      check_status="failed"
      echo "{\"file\":\"$rel\",\"check\":\"failed\",\"compile\":\"skipped\"}" >> "$RESULTS_FILE"
      echo "  [check-fail ] $rel"
      [[ $KEEP_GOING -eq 0 ]] && exit 1
      continue
    fi
  fi

  # Step 2: jl4-mlir wasm
  [[ $VERBOSE -eq 1 ]] && echo "[compile] $rel → $OUTPUT_DIR/$base.wasm"
  wasm_dir="$out_dir"
  base_name="$(basename "$base")"

  if "$MLIR_BIN" wasm "$src" -o "$wasm_dir/$base_name.wasm" >"$log_file" 2>&1; then
    # Check whether any exports were produced. A file with no @export
    # declarations compiles successfully but yields an almost-empty WASM;
    # that's useful to know but not a failure.
    schema_file="$wasm_dir/$base_name.schema.json"
    export_count=0
    if [[ -f "$schema_file" ]]; then
      # Use wc -l which reliably prints a single integer on macOS.
      export_count="$(grep -o '"wasmSymbol"' "$schema_file" 2>/dev/null | wc -l | tr -d ' ')"
      [[ -z "$export_count" ]] && export_count=0
    fi

    if [[ "$export_count" -eq 0 ]]; then
      SKIPPED_NO_EXPORTS=$((SKIPPED_NO_EXPORTS + 1))
      echo "{\"file\":\"$rel\",\"check\":\"$check_status\",\"compile\":\"ok-no-exports\",\"exports\":0}" >> "$RESULTS_FILE"
      [[ $VERBOSE -eq 1 ]] && echo "  [no-exports] $rel"
    else
      COMPILED_OK=$((COMPILED_OK + 1))
      echo "{\"file\":\"$rel\",\"check\":\"$check_status\",\"compile\":\"ok\",\"exports\":$export_count}" >> "$RESULTS_FILE"
      rm -f "$log_file"  # success: drop the log to keep the tree tidy
      echo "  [compile-ok ] $rel ($export_count exports)"
    fi
  else
    COMPILE_FAILED=$((COMPILE_FAILED + 1))
    echo "{\"file\":\"$rel\",\"check\":\"$check_status\",\"compile\":\"failed\"}" >> "$RESULTS_FILE"
    echo "  [compile-fail] $rel (see $log_file)"
    [[ $KEEP_GOING -eq 0 ]] && exit 1
  fi
done < "$L4_LIST_FILE"

# ---------------------------------------------------------------------------
# Summaries
# ---------------------------------------------------------------------------

SUMMARY_TXT="$OUTPUT_DIR/summary.txt"
SUMMARY_JSON="$OUTPUT_DIR/summary.json"

{
  echo "L4 → WASM batch compile summary"
  echo "Generated: $(date)"
  echo ""
  echo "Total files scanned:     $TOTAL"
  [[ $SKIP_CHECK -eq 0 ]] && {
    echo "Typecheck ok:            $CHECKED_OK"
    echo "Typecheck failed:        $CHECK_FAILED"
  }
  echo "Compiled to WASM:        $COMPILED_OK"
  echo "Compiled (no exports):   $SKIPPED_NO_EXPORTS"
  echo "Compile failed:          $COMPILE_FAILED"
} > "$SUMMARY_TXT"

{
  echo "{"
  echo "  \"totalFiles\": $TOTAL,"
  echo "  \"typecheckOk\": $CHECKED_OK,"
  echo "  \"typecheckFailed\": $CHECK_FAILED,"
  echo "  \"compileOk\": $COMPILED_OK,"
  echo "  \"compileOkNoExports\": $SKIPPED_NO_EXPORTS,"
  echo "  \"compileFailed\": $COMPILE_FAILED,"
  echo "  \"results\": ["

  if [[ -s "$RESULTS_FILE" ]]; then
    total_results="$(wc -l < "$RESULTS_FILE" | tr -d ' ')"
    idx=0
    while IFS= read -r line; do
      idx=$((idx + 1))
      if [[ $idx -lt $total_results ]]; then
        echo "    $line,"
      else
        echo "    $line"
      fi
    done < "$RESULTS_FILE"
  fi

  echo "  ]"
  echo "}"
} > "$SUMMARY_JSON"

rm -f "$RESULTS_FILE"

echo ""
cat "$SUMMARY_TXT"
echo ""
echo "Summary written to:"
echo "  $SUMMARY_TXT"
echo "  $SUMMARY_JSON"

# Clean up the per-file build products (.wasm, .mlir, .ll, .o, .opt.mlir,
# .schema.json, .log). Keep only the summary files. This keeps a run from
# leaving hundreds of MB of generated artifacts in dist-wasm/.
find "$OUTPUT_DIR" -type f \
  \( -name "*.wasm" -o -name "*.mlir" -o -name "*.ll" \
     -o -name "*.o" -o -name "*.schema.json" -o -name "*.log" \) \
  -delete 2>/dev/null
# Remove any directories that are now empty.
find "$OUTPUT_DIR" -type d -empty -delete 2>/dev/null

if [[ $COMPILE_FAILED -gt 0 || $CHECK_FAILED -gt 0 ]]; then
  exit 1
fi
