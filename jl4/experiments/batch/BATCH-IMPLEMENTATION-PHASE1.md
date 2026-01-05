# Batch Processing Implementation - Phase 1 Complete

## What Was Implemented

### 1. CLI Infrastructure

- Added `--batch` flag to `jl4-cli` (short option: `-b`)
- Supports reading from file or STDIN (use `-` as filename)
- Help text: `--batch BATCH_FILE` - "Batch input file (JSON/YAML/CSV); use '-' for stdin"

### 2. JSON Parsing

- Added `aeson` and `bytestring` dependencies to `jl4.cabal`
- Parses JSON array from batch input file or STDIN
- Validates JSON structure and reports parse errors

### 3. Output Format

Current output structure (placeholder for Phase 1):

```json
[
  {
    "input": { "x": 5, "y": 10 },
    "output": null,
    "status": "success"
  }
]
```

### 4. Build Status

✅ Compiles successfully  
✅ Executable created: `dist-newstyle/build/.../jl4-cli`
✅ Basic stdin test passes

## Testing

### Test STDIN mode:

```bash
echo '[{"name":"test"}]' | cabal run jl4-cli -- --batch - jl4/examples/ok/basic.l4
```

Output:

```json
[{ "input": { "name": "test" }, "output": null, "status": "success" }]
```

### Test file mode:

```bash
cabal run jl4-cli -- --batch test-batch-input.json jl4/examples/ok/basic.l4
```

## Files Modified

1. **jl4/jl4.cabal** (line 30-33)
   - Added `aeson` and `bytestring` dependencies

2. **jl4/app/Main.hs** (complete rewrite)
   - Added imports for JSON processing
   - Added `BatchProcessing` log constructor
   - Branched main function on `batchFile` option
   - Implemented batch processing skeleton
   - STDIN support when batchFile == "-"
   - Normal oneshot mode preserved

3. **test-batch-input.json** (created)
   - Sample test file with JSON array input

## What Remains (Phase 2-4)

### Phase 2: Wire Up L4 Evaluation

- [ ] Use `L4.Export.getExportedFunctions` to find entrypoint
- [ ] Load and typecheck L4 file using existing Shake rules
- [ ] Call L4 function with JSON parameters
- [ ] Convert L4 output to JSON
- [ ] Handle evaluation errors (status: "error")

### Phase 3: Type Validation

- [ ] Use `ExportedParam` from PR #682
- [ ] Validate input parameters against function signature
- [ ] Report type mismatches with helpful messages

### Phase 4: Advanced Features

- [ ] YAML support (`--format yaml`)
- [ ] CSV support (`--format csv`)
- [ ] Entrypoint selection (`--entrypoint NAME`)
- [ ] Continue on error (`--continue-on-error`)
- [ ] Trace output (`--trace`)

## Architecture Notes

### Reused Existing Patterns

- `oneshotL4Action` pattern for loading L4 files
- `Shake.use Rules.*` for typecheck/eval
- Logger infrastructure for error reporting
- Option parsing with `optparse-applicative`

### Design Decisions

1. **Placeholder output**: Phase 1 echoes inputs back with null outputs and success status
   - Allows testing of infrastructure without L4 evaluation complexity
   - Output structure matches final spec

2. **Batch vs. Normal mode**: Separate code paths for clarity
   - Batch mode: parse JSON, iterate, output JSON
   - Normal mode: unchanged (preserves existing behavior)

3. **STDIN support**: Special case when batchFile == "-"
   - Uses `BSL.hGetContents stdin`
   - No temp files needed

## Next Steps

To continue implementation (Phase 2):

1. Import `L4.Export` functions
2. Modify batch processing to:
   - Load L4 file once (reuse oneshot pattern)
   - Find default export or use `--entrypoint`
   - For each JSON input:
     - Convert JSON to L4 value
     - Call L4 function
     - Convert result to JSON
   - Output array of results

3. Add error handling:
   - Parse errors → {"status": "error", "message": "..."}
   - Evaluation errors → capture and report
   - Type errors → use param descriptions from PR #682

## References

- Spec: `doc/dev/specs/todo/BATCH-PROCESSING-SPEC.md`
- Export functions: `jl4-core/src/L4/Export.hs`
- PR #682: `@desc` annotation support (merged)
