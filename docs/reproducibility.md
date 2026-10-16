# Reproducibility

## OCaml

```
opam switch 5.3.0
opam install . --deps-only --with-test
dune build
dune runtest
```

The committed runs used OCaml 5.3.0, Dune 3.24, Yojson 3, qcheck-core 0.91. `queuelab.opam` allows older compatible versions. CI installs the opam constraints on `ubuntu-latest` with compiler 5.3.0.

## Fixture and experiment

From the repository root:

```
dune exec queuelab -- generate-fixture --out data/processed/synthetic/session --steps 4000 --seed 7
dune exec queuelab -- validate --dataset data/processed/synthetic/session
dune exec queuelab -- replay --dataset data/processed/synthetic/session
dune exec queuelab -- experiment --config configs/queue_sensitivity.json
dune exec queuelab -- generate-fixture --out data/processed/synthetic/divergence --scenario divergence
dune exec queuelab -- experiment --config configs/divergence.json
dune exec queuelab -- simulate --config configs/baseline.json
```

The generator is a pure function of the seed and the step count. The session manifest records the SHA-256. If the hash changes, the generator changed and the report must be regenerated. Do not keep the old prose.

## Benchmark

```
QUEUELAB_PROFILE=release dune exec --profile release queuelab -- benchmark \
  --dataset data/processed/synthetic/session --synthetic-events 200000 --out reports/bench.json
```

Record `uname`, `/proc/cpuinfo`, OCaml version and the profile field with the number. The committed `reports/bench.json` is one machine.

## Python

```
python3 -m pip install -r requirements.txt
python3 scripts/build_figures.py
python3 scripts/write_report.py
```

`write_report.py` fails if an input file is missing. It does not synthesize a replacement table.

## Offline

After the opam switch and the Python packages are installed, tests, replay and the synthetic experiment do not need the network. `queuelab capture` does. A failed capture is a failed capture, not a license to invent depth.

## Git

The engine writes `git rev-parse --short HEAD` into the experiment JSON when that command works. A dirty tree can still produce a hash that does not contain your edits. Trust the dataset SHA-256 and re-run.
