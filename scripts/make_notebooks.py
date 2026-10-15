#!/usr/bin/env python3
"""Create the six research notebooks. Execution is a separate step."""

from __future__ import annotations

from pathlib import Path

import nbformat
from nbformat.v4 import new_code_cell, new_markdown_cell, new_notebook

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "notebooks"


def nb(cells):
    book = new_notebook(cells=cells)
    book.metadata.kernelspec = {
        "display_name": "Python 3",
        "language": "python",
        "name": "queuelab",
    }
    book.metadata.language_info = {"name": "python", "pygments_lexer": "ipython3"}
    return book


def save(name, cells):
    OUT.mkdir(exist_ok=True)
    path = OUT / name
    nbformat.write(nb(cells), path)
    print("wrote", path)


BOOT = """import sys
from pathlib import Path
ROOT = Path.cwd()
if not (ROOT / "scripts" / "analysis.py").is_file():
    raise SystemExit("run this notebook from the repository root")
sys.path.insert(0, str(ROOT / "scripts"))
import analysis
from IPython.display import Image, display

def show(name):
    display(Image(filename=str(analysis.FIG / f"{name}.png")))
"""


def main():
    save(
        "01_data_quality.ipynb",
        [
            new_markdown_cell(
                """# 01 — Data quality

Reads the canonical log and the OCaml validation report. Python does not rebuild the book.

The committed session is synthetic. A capture, if you point this notebook at one, is still market-by-price: queue position is not in the file."""
            ),
            new_code_cell(
                BOOT
                + """
q = analysis.quality()
meta = analysis.summary()
print("synthetic:", q["synthetic"])
print("symbol:", q["symbol"])
print("notes:", q.get("notes"))
print("limitations:", q.get("limitations"))
if q["synthetic"] is not True:
    print("This file is not marked synthetic. It is still not an order-by-order feed.")
table = analysis.quality_table()
table
"""
            ),
            new_code_cell(
                """events = analysis.events(analysis.ROOT / "data" / "processed" / "synthetic" / "session")
if not bool(events["synthetic"].all()):
    raise RuntimeError("session events must stay marked synthetic")
print(events["kind"].value_counts())
print("schema", sorted(events["schema"].unique().tolist()))
analysis.plot_interarrival(events, analysis.FIG)
show("interarrival")
print("gaps", q["gaps"], "duplicates", q["duplicates"], "crossed", q["crossed_observations"])
print("rx-ex is not latency. Summary:", q["rx_minus_ex_us"])
"""
            ),
            new_markdown_cell(
                """Inter-arrival uses exchange time when it is non-zero, else receive time. On the synthetic session those clocks are the same by construction. On a capture they are not, and their difference is not a one-way delay."""
            ),
        ],
    )
    save(
        "02_order_book_diagnostics.ipynb",
        [
            new_markdown_cell(
                """# 02 — Order book diagnostics

`book_series.csv` is written by the OCaml replay. Displayed size at a price is not a list of orders.

Mid is the integer average of best bid and best ask, truncated toward zero. Microprice is computed in the engine; this series stores mid, touch prices, touch quantities and spread. Spread is ask minus bid in price units."""
            ),
            new_code_cell(
                BOOT
                + """
book = analysis.book_series()
q = analysis.quality()
print("synthetic:", q["synthetic"], "rows:", len(book))
book["imbalance"] = (book["bid_qty"] - book["ask_qty"]) / (book["bid_qty"] + book["ask_qty"])
print(book[["mid", "spread", "bid_qty", "ask_qty", "imbalance"]].describe())
analysis.plot_book(book, analysis.FIG)
show("mid_path")
show("spread_hist")
show("touch_depth")
"""
            ),
            new_markdown_cell(
                """Touch imbalance is `(bid_qty - ask_qty) / (bid_qty + ask_qty)` using displayed size at the best prices only. It is not an order-flow toxicity measure and it does not see hidden size."""
            ),
        ],
    )
    save(
        "03_latency_and_slippage.ipynb",
        [
            new_markdown_cell(
                """# 03 — Latency and slippage

Submit delay is a scenario grid, not a measured Binance RTT. Implementation shortfall for a buy is fill price minus the decision mid. Positive means the fill was worse than the mid at decision. Spread capture for a buy is mid-at-fill minus price. Fees are not inside either column."""
            ),
            new_code_cell(
                BOOT
                + """
sens = analysis.sensitivity()
execs = analysis.executions()
print("orders", int(sens["n_orders"].iloc[0]), "cells", len(sens))
print(sens[["model", "submit_delay_us", "n_accepted", "fill_rate", "fill_rate_ci_lo", "fill_rate_ci_hi", "mean_implementation_shortfall", "mean_spread_capture"]].to_string(index=False))
analysis.plot_fill_rate(sens, analysis.FIG)
show("fill_rate_by_latency")
zero = execs[execs["submit_delay_us"] == 0]
filled = zero[zero["filled_qty"] > 0]
print("filled orders at delay 0:", len(filled), "of", len(zero))
print("IS percentile 50", analysis.descriptive_percentile(filled["implementation_shortfall"], 50))
print("spread-capture percentile 50", analysis.descriptive_percentile(filled["spread_capture"], 50))
"""
            ),
            new_markdown_cell(
                """The percentile lines are descriptive. The inferential interval in the table is the OCaml block bootstrap of the fill indicator, not of shortfall. Fills overlap in time, so the bootstrap block only partially addresses dependence."""
            ),
        ],
    )
    save(
        "04_queue_model_sensitivity.ipynb",
        [
            new_markdown_cell(
                """# 04 — Queue-model sensitivity

Differences across models are estimates under explicit rules, not observed queue positions. The session figure can be flat. The divergence figure is a constructed counterexample, not a market sample."""
            ),
            new_code_cell(
                BOOT
                + """
sens = analysis.sensitivity()
span = float(sens.groupby("submit_delay_us")["fill_rate"].agg(lambda s: s.max() - s.min()).max())
print("max fill-rate gap across models at fixed delay:", round(span, 6))
analysis.plot_markout(sens, analysis.FIG)
analysis.plot_markout_heatmap(sens, analysis.FIG)
analysis.plot_offset(analysis.executions(), analysis.FIG)
show("markout_by_latency")
show("markout_heatmap")
show("fill_rate_by_offset")
div = analysis.executions(analysis.DIVERGENCE)
print(div[["model", "filled_qty", "status", "n_fills"]].to_string(index=False))
analysis.plot_divergence(div, analysis.FIG)
show("divergence_filled_qty")
if div.loc[div["model"] == "optimistic", "filled_qty"].iloc[0] <= div.loc[div["model"] == "conservative", "filled_qty"].iloc[0]:
    raise RuntimeError("divergence fixture no longer separates optimistic from conservative")
"""
            ),
            new_markdown_cell(
                """Optimistic filled quantity should exceed conservative on this fixture. Probabilistic depends on the seed. A zero probabilistic fill is a draw, not a third deterministic bound."""
            ),
        ],
    )
    save(
        "05_adverse_selection.ipynb",
        [
            new_markdown_cell(
                """# 05 — Adverse selection

Signed markout at horizon h is `mid(t+h) - mid(fill)` for a buy and the opposite for a sell. Positive is favorable. Future mids are not inputs to the decision.

The session mean pools horizons. This notebook splits them. Orders overlap, so sample size is not an independent-draw count."""
            ),
            new_code_cell(
                BOOT
                + """
marks = analysis.markouts()
execs = analysis.executions()
print("markout rows", len(marks), "nulls", int(marks["signed_markout"].isna().sum()))
print(marks.groupby(["side", "horizon_us"])["signed_markout"].agg(["count", "mean"]).to_string())
print(marks.groupby(["vol_bucket", "horizon_us"])["signed_markout"].mean().to_string())
analysis.plot_markout_horizons(marks, analysis.FIG)
show("markout_horizons")
joined = analysis.plot_capture_vs_markout(execs, marks, analysis.FIG)
show("capture_vs_markout")
print("joined points", len(joined))
"""
            ),
            new_markdown_cell(
                """A fill can show positive spread capture and negative markout at the same time. Net cash outcome also subtracts the modeled fee inside the ledger. The scatter does not add that fee a second time."""
            ),
        ],
    )
    save(
        "06_performance_and_validation.ipynb",
        [
            new_markdown_cell(
                """# 06 — Performance and validation

Numbers come from `reports/bench.json` on the machine that ran the release binary. They are not a hardware-independent throughput.

Checkpoint time is the final shallow book, not the event log. The correctness check for checkpoints is the OCaml test that a restored suffix matches a full replay hash, not this timer."""
            ),
            new_code_cell(
                BOOT
                + """
bench = analysis.benchmark()
print(bench["note"])
print(bench["uname"])
print(bench["cpu"])
print(bench["meminfo"])
print("ocaml", bench["ocaml"], "profile", bench["profile"])
print("dataset events", bench["dataset_events"], "median replay s", bench["dataset_replay"]["median_s"])
print("synthetic events", bench["synthetic_events"], "median replay s", bench["synthetic_replay"]["median_s"])
print("events/s", bench["events_per_second_synthetic_median"])
print("checkpoint median s", bench["checkpoint_roundtrip_synthetic"]["median_s"])
analysis.plot_benchmark(bench, analysis.FIG)
show("benchmark_rates")
q = analysis.quality()
print("replay hash", q["book_hash"], "quality ok", q["ok"])
"""
            ),
            new_markdown_cell(
                """Tests that must already be green before trusting this notebook: `dune runtest`. That run covers sync, duplicate, gap, deterministic replay, checkpoint resume, the conservative/optimistic divergence, no double-count, cancel/fill race, ledger identity and a QCheck book update."""
            ),
        ],
    )


if __name__ == "__main__":
    main()
