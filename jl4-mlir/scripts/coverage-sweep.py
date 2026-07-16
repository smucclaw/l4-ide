#!/usr/bin/env python3
"""coverage-sweep.py — M2 compile-coverage triage for the L4→MLIR backend.

Runs every .l4 file in the repo through `jl4-mlir wasm --mlir-only` (typecheck
+ lower + schema emission — no MLIR/LLVM toolchain required) and buckets the
outcome:

  - typecheck-fail   : jl4-core rejected the file (a *language coverage* gap,
                       not a backend gap). The error class is recorded.
  - lower-fail       : typecheck passed but lowering/emit crashed.
  - no-exports       : compiled, but nothing is @export-ed.
  - clean            : every exported function is `supported: true`.
  - has-unsupported  : compiled, but >=1 export is `supported: false`
                       (flagged by M1a). The reasons are aggregated.

Writes a human report + JSON to the output dir. This is the M2 deliverable:
a map from unsupported feature -> affected functions, and a categorised list
of typecheck failures.

  python3 scripts/coverage-sweep.py [--out DIR] [--limit N] [--filter SUBSTR]
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
from collections import Counter, defaultdict

REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))


def find_l4_files(filter_substr):
    out = []
    skip = ("/node_modules/", "/dist-newstyle/", "/dist-wasm/", "/.git/")
    for dirpath, _dirs, files in os.walk(REPO_ROOT):
        if any(s.strip("/") in dirpath.split(os.sep) for s in skip):
            continue
        for f in files:
            if f.endswith(".l4"):
                p = os.path.join(dirpath, f)
                if filter_substr and filter_substr not in p:
                    continue
                out.append(p)
    return sorted(out)


def classify_typecheck_error(stderr):
    """Bucket a typecheck failure into a coarse category for triage."""
    s = stderr.lower()
    # Order matters: most specific first.
    patterns = [
        ("import-resolution", r"could not (find|resolve) import|import .* not found"),
        ("parse-error", r"parse error|unexpected|expecting"),
        ("type-mismatch", r"inferred to be of type|couldn't match|expected.*type"),
        ("unbound-name", r"not in scope|unbound|undefined name|unknown identifier"),
        ("ambiguous", r"ambiguous"),
        ("kind/forall", r"forall|kind"),
    ]
    for label, rx in patterns:
        if re.search(rx, s):
            return label
    return "other"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=os.path.join(REPO_ROOT, "jl4-mlir", "coverage-report"))
    ap.add_argument("--limit", type=int, default=0)
    ap.add_argument("--filter", default="")
    ap.add_argument("--bin", default="")
    args = ap.parse_args()

    mlir_bin = args.bin
    if not mlir_bin:
        mlir_bin = subprocess.run(
            ["cabal", "list-bin", "jl4-mlir"], cwd=REPO_ROOT,
            capture_output=True, text=True,
        ).stdout.strip()
    if not mlir_bin or not os.path.exists(mlir_bin):
        print("Could not locate jl4-mlir binary (build with `cabal build jl4-mlir`).", file=sys.stderr)
        return 1

    files = find_l4_files(args.filter)
    if args.limit:
        files = files[: args.limit]
    print(f"Sweeping {len(files)} .l4 files through {os.path.basename(mlir_bin)} --mlir-only\n")

    buckets = Counter()
    tc_error_classes = Counter()
    unsupported_reasons = Counter()
    unsupported_fns = defaultdict(list)   # reason -> [file::fn]
    per_file = []

    tmp = tempfile.mkdtemp(prefix="l4cov-")
    for i, src in enumerate(files, 1):
        rel = os.path.relpath(src, REPO_ROOT)
        out_mlir = os.path.join(tmp, f"f{i}.mlir")
        out_schema = os.path.join(tmp, f"f{i}.schema.json")
        proc = subprocess.run(
            [mlir_bin, "wasm", src, "--mlir-only", "-o", out_mlir],
            capture_output=True, text=True,
        )
        rec = {"file": rel}
        if proc.returncode != 0:
            cls = classify_typecheck_error(proc.stderr + proc.stdout)
            # Distinguish typecheck vs later-stage failures heuristically.
            bucket = "typecheck-fail" if "typecheck" in (proc.stderr + proc.stdout).lower() or cls != "other" else "lower-fail"
            buckets[bucket] += 1
            tc_error_classes[cls] += 1
            rec.update(outcome=bucket, errorClass=cls,
                       error=(proc.stderr or proc.stdout).strip().splitlines()[-1:] )
            per_file.append(rec)
            print(f"[{i}/{len(files)}] {bucket:15} ({cls}) {rel}")
            continue

        if not os.path.exists(out_schema):
            buckets["no-schema"] += 1
            rec.update(outcome="no-schema")
            per_file.append(rec)
            continue

        with open(out_schema) as fh:
            schema = json.load(fh)
        fns = schema.get("functions", {})
        if not fns:
            buckets["no-exports"] += 1
            rec.update(outcome="no-exports", functions=0)
            per_file.append(rec)
            print(f"[{i}/{len(files)}] {'no-exports':15} {rel}")
            continue

        unsupported = {n: fe for n, fe in fns.items() if fe.get("supported") is False}
        if unsupported:
            buckets["has-unsupported"] += 1
            for n, fe in unsupported.items():
                reason = fe.get("unsupportedReason") or "unspecified"
                for r in reason.split("; "):
                    unsupported_reasons[r] += 1
                    unsupported_fns[r].append(f"{rel}::{n}")
            rec.update(outcome="has-unsupported", functions=len(fns),
                       unsupported={n: fe.get("unsupportedReason") for n, fe in unsupported.items()})
            print(f"[{i}/{len(files)}] {'has-unsupported':15} {rel} ({len(unsupported)}/{len(fns)})")
        else:
            buckets["clean"] += 1
            rec.update(outcome="clean", functions=len(fns))
            print(f"[{i}/{len(files)}] {'clean':15} {rel} ({len(fns)} fns)")
        per_file.append(rec)

    os.makedirs(args.out, exist_ok=True)
    report = {
        "totalFiles": len(files),
        "buckets": dict(buckets),
        "typecheckErrorClasses": dict(tc_error_classes),
        "unsupportedReasonHistogram": dict(unsupported_reasons),
        "unsupportedFunctionsByReason": {r: fns for r, fns in unsupported_fns.items()},
        "perFile": per_file,
    }
    with open(os.path.join(args.out, "coverage.json"), "w") as fh:
        json.dump(report, fh, indent=2)

    lines = []
    lines.append("L4 -> MLIR/WASM compile-coverage sweep (M2)\n")
    lines.append(f"Total files: {len(files)}\n")
    lines.append("Outcomes:")
    for k in ["clean", "has-unsupported", "no-exports", "typecheck-fail", "lower-fail", "no-schema"]:
        if k in buckets:
            lines.append(f"  {k:18} {buckets[k]}")
    lines.append("\nTypecheck failure classes:")
    for cls, n in tc_error_classes.most_common():
        lines.append(f"  {cls:18} {n}")
    lines.append("\nUnsupported-construct reasons (function count):")
    for r, n in unsupported_reasons.most_common():
        lines.append(f"  {n:4}  {r}")
    txt = "\n".join(lines) + "\n"
    with open(os.path.join(args.out, "coverage.txt"), "w") as fh:
        fh.write(txt)

    print("\n" + txt)
    print(f"Report: {os.path.join(args.out, 'coverage.txt')} / coverage.json")
    return 0


if __name__ == "__main__":
    sys.exit(main())
