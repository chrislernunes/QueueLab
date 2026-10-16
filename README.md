# QueueLab

Deterministic market-data replay, queue-position inference and passive-execution simulation in OCaml.

Research question: how sensitive are passive fill probability, execution cost and adverse selection to queue-position assumptions and order-entry latency?

The committed numerical answer is in [reports/execution_research.md](reports/execution_research.md). It is a labeled synthetic run. Read it before quoting a fill rate. A static rendering of the same files is the GitHub Pages site, once Pages is enabled from the `pages` workflow.

## Layout

- `lib/` replay, depth sync, queue models, fill engine, ledger, experiment
- `bin/main.ml` command line
- `configs/` experiment files
- `data/processed/synthetic/` fixtures this repository is allowed to ship
- `scripts/` Binance capture client, figure builder, research note
- `site/` static GitHub Pages rendering of the committed reports
- `docs/` semantics and limits

Details: [docs/architecture.md](docs/architecture.md), [docs/queue_models.md](docs/queue_models.md), [docs/assumptions_and_limitations.md](docs/assumptions_and_limitations.md).

## Build

OCaml 5.3 and Dune 3.16 or newer. Dependencies are Yojson and, for tests, qcheck-core. The engine uses the OCaml standard library, not Core or Async.

```
opam install . --deps-only --with-test
dune build
dune runtest
```

Python, only for figures and the research note:

```
python3 -m pip install -r requirements.txt
```

## Commands

```
dune exec queuelab -- validate --dataset data/processed/synthetic/session
dune exec queuelab -- replay --dataset data/processed/synthetic/session
dune exec queuelab -- generate-fixture --out data/processed/synthetic/session --steps 4000 --seed 7
dune exec queuelab -- generate-fixture --out data/processed/synthetic/divergence --scenario divergence
dune exec queuelab -- experiment --config configs/queue_sensitivity.json
dune exec queuelab -- experiment --config configs/divergence.json
dune exec queuelab -- simulate --config configs/baseline.json
QUEUELAB_PROFILE=release dune exec --profile release queuelab -- benchmark \
  --dataset data/processed/synthetic/session --synthetic-events 200000 --out reports/bench.json
dune exec queuelab -- normalize --raw test/fixtures/protocol_example --out /tmp/queuelab-protocol --symbol BTCUSDT
dune exec queuelab -- capture --symbol BTCUSDT --duration-seconds 30 --out data/raw/btcusdt-30
```

Exit codes: 0 success, 1 failed validation or a gap, 2 bad usage.

`simulate` and `experiment` are the same runner. `capture` starts `scripts/capture_binance.py` (stdlib TLS, public streams, no API key) and then normalizes. The protocol state machine is `lib/depth_sync.ml`. If the bridge from snapshot to diff fails, the command exits 1 and leaves the raw file in place.

`make test`, `make experiment`, `make bench` and `make figures` wrap the same steps.

## Figures

From the repository root, after the experiment and benchmark files exist:

```
python3 scripts/build_figures.py
python3 scripts/write_report.py
```

`scripts/analysis.py` only reads engine outputs. Figures land in `reports/figures/` as PNG and PDF. A missing input is an error. The numerical write-up is `reports/execution_research.md`.

## Reproduce the committed run

[docs/reproducibility.md](docs/reproducibility.md). The session fixture is seed 7, 4000 steps. The benchmark file records the CPU it was measured on. Do not reuse that events/second number on another machine.

## Capture and historical data

Public historical dumps do not include the incremental depth this book needs. Capture prospectively. Do not commit the raw messages. A hand-built protocol example lives in `test/fixtures/protocol_example` so normalize can be checked offline.

## Tests

`dune runtest` covers checksums, depth sync, duplicate and gap handling, replay determinism, checkpoint resume, queue-model disagreement, double-count, cancel/fill races, ledger identities, fee arithmetic, Binance JSON decode and a randomized book update. The property test prints a QCheck seed.

Frame encode/decode for the capture socket:

```
python3 scripts/capture_binance.py --self-test
```

## Benchmark method

Release binary, one warmup call discarded, then repeated wall-clock samples. Median is what the report quotes. The synthetic stream is generated in process at the requested step count. Checkpoint timing is the final book only. See `reports/bench.json` and `reports/execution_research.md`.

## Limits

Public depth is market-by-price. Queue position is a model. Latency grids and the 100 ppm fee are scenario assumptions. Integer ledger multiplication wraps on overflow. The synthetic session does not separate fill rates across queue models; the five-event divergence fixture does. Both results are in the research note. Fuller list: [docs/assumptions_and_limitations.md](docs/assumptions_and_limitations.md).

## License

MIT. Copyright Chrisler Nunes, 2026.
