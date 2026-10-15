.PHONY: build test release experiment bench validate replay fixture docs-figures

OPAMROOT ?= /opt/opam
export OPAMROOT

DUNE = dune

build:
	$(DUNE) build

test:
	$(DUNE) build
	$(DUNE) runtest

release:
	$(DUNE) build --profile release

fixture: build
	$(DUNE) exec queuelab -- generate-fixture --out data/processed/synthetic/session --steps 4000 --seed 7

validate: fixture
	$(DUNE) exec queuelab -- validate --dataset data/processed/synthetic/session

replay: fixture
	$(DUNE) exec queuelab -- replay --dataset data/processed/synthetic/session

experiment: fixture
	mkdir -p reports/figures
	$(DUNE) exec queuelab -- experiment --config configs/queue_sensitivity.json

bench: release fixture
	QUEUELAB_PROFILE=release $(DUNE) exec --profile release queuelab -- benchmark --dataset data/processed/synthetic/session --synthetic-events 200000 --out reports/bench.json

figures:
	python3 scripts/build_figures.py

all: test experiment bench figures
