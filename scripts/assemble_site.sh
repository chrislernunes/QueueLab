#!/bin/sh
# Assemble a GitHub Pages tree. Report files are copied, not rewritten.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
OUT=${1:-"$ROOT/_site"}
rm -rf "$OUT"
mkdir -p "$OUT/data"
cp -R "$ROOT/site/." "$OUT/"
cp "$ROOT/reports/sensitivity.csv" "$OUT/data/sensitivity.csv"
cp "$ROOT/reports/figures/markout_horizons.csv" "$OUT/data/markout_horizons.csv"
cp "$ROOT/reports/figures/fill_rate_by_offset.csv" "$OUT/data/fill_rate_by_offset.csv"
cp "$ROOT/reports/divergence/executions.csv" "$OUT/data/divergence_executions.csv"
cp "$ROOT/reports/book_series.csv" "$OUT/data/book_series.csv"
cp "$ROOT/reports/bench.json" "$OUT/data/bench.json"
cp "$ROOT/reports/data_quality.json" "$OUT/data/data_quality.json"
cp "$ROOT/reports/queue_sensitivity.json" "$OUT/data/queue_sensitivity.json"
cp "$ROOT/reports/baseline/sensitivity.csv" "$OUT/data/baseline_sensitivity.csv"
cp "$ROOT/reports/capture_attempt.json" "$OUT/data/capture_attempt.json"
cp "$ROOT/reports/divergence/data_quality.json" "$OUT/data/divergence_quality.json"
cp "$ROOT/reports/execution_research.md" "$OUT/data/execution_research.md"
cp "$ROOT/configs/queue_sensitivity.json" "$OUT/data/queue_sensitivity_config.json"
cp "$ROOT/configs/divergence.json" "$OUT/data/divergence_config.json"
cp "$ROOT/configs/baseline.json" "$OUT/data/baseline_config.json"
: > "$OUT/.nojekyll"
echo "assembled $OUT"
