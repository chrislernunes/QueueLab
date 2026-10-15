"""Load QueueLab outputs. Does not simulate a book or a fill."""

from __future__ import annotations

import json
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from scipy import stats

ROOT = Path(__file__).resolve().parents[1]
SESSION = ROOT / "reports"
DIVERGENCE = ROOT / "reports" / "divergence"
FIG = ROOT / "reports" / "figures"

sns.set_theme(style="ticks", context="paper")
plt.rcParams.update(
    {
        "axes.spines.top": False,
        "axes.spines.right": False,
        "figure.dpi": 120,
        "savefig.dpi": 160,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    }
)


def require(path: Path) -> Path:
    if not path.is_file():
        raise FileNotFoundError(
            f"missing {path}. Run the OCaml experiment or benchmark that writes it. "
            "This module will not invent rows."
        )
    return path


def read_json(path: Path) -> dict:
    return json.loads(require(path).read_text())


def sensitivity(directory: Path = SESSION) -> pd.DataFrame:
    frame = pd.read_csv(require(directory / "sensitivity.csv"))
    if frame.empty:
        raise RuntimeError(f"empty sensitivity.csv in {directory}")
    return frame


def executions(directory: Path = SESSION) -> pd.DataFrame:
    return pd.read_csv(require(directory / "executions.csv"))


def markouts(directory: Path = SESSION) -> pd.DataFrame:
    return pd.read_csv(require(directory / "markouts.csv"))


def book_series(directory: Path = SESSION) -> pd.DataFrame:
    frame = pd.read_csv(require(directory / "book_series.csv"))
    frame["time_s"] = frame["time_us"] / 1_000_000.0
    return frame


def quality(directory: Path = SESSION) -> dict:
    return read_json(directory / "data_quality.json")


def summary(directory: Path = SESSION) -> dict:
    return read_json(directory / "queue_sensitivity.json")


def benchmark() -> dict:
    return read_json(SESSION / "bench.json")


def events(dataset: Path) -> pd.DataFrame:
    path = require(dataset / "events.jsonl")
    rows = [json.loads(line) for line in path.read_text().splitlines() if line.strip()]
    if not rows:
        raise RuntimeError(f"no events in {path}")
    frame = pd.DataFrame(rows)
    if "synthetic" not in frame.columns or not bool(frame["synthetic"].all()):
        # Real captures are allowed. Callers must print the flag. Do not relabel.
        pass
    return frame


def _save(fig: plt.Figure, name: str, out: Path) -> None:
    out.mkdir(parents=True, exist_ok=True)
    fig.savefig(out / f"{name}.png")
    fig.savefig(out / f"{name}.pdf")
    plt.close(fig)


def plot_fill_rate(sens: pd.DataFrame, out: Path) -> None:
    fig, ax = plt.subplots(figsize=(7.2, 4.2))
    models = list(sens["model"].unique())
    delays = sorted(sens["submit_delay_us"].unique())
    width = 0.8 / max(1, len(models))
    x = np.arange(len(delays))
    for i, model in enumerate(models):
        part = sens[sens["model"] == model].set_index("submit_delay_us").loc[delays]
        xpos = x + (i - (len(models) - 1) / 2) * width
        y = part["fill_rate"].to_numpy()
        lo = part["fill_rate_ci_lo"].to_numpy()
        hi = part["fill_rate_ci_hi"].to_numpy()
        ax.bar(xpos, y, width=width, label=model)
        ax.errorbar(xpos, y, yerr=[y - lo, hi - y], fmt="none", ecolor="black", elinewidth=0.8, capsize=2)
    ax.set_xticks(x, [str(int(d)) for d in delays])
    ax.set_xlabel("Submit delay (µs), scenario assumption")
    ax.set_ylabel("Fill rate (any fill / accepted)")
    ax.set_ylim(0, 1)
    ax.set_title("Synthetic session: fill rate by queue model and submit delay")
    ax.legend(frameon=False, title="queue model")
    fig.tight_layout()
    _save(fig, "fill_rate_by_latency", out)


def plot_markout(sens: pd.DataFrame, out: Path) -> None:
    fig, ax = plt.subplots(figsize=(7.2, 4.2))
    for model, part in sens.groupby("model"):
        part = part.sort_values("submit_delay_us")
        ax.plot(part["submit_delay_us"], part["mean_markout"], marker="o", label=model)
    ax.axhline(0, color="black", linewidth=0.6)
    ax.set_xlabel("Submit delay (µs), scenario assumption")
    ax.set_ylabel("Mean signed markout (price units)")
    ax.set_title("Synthetic session: pooled mean markout across horizons")
    ax.legend(frameon=False, title="queue model")
    fig.tight_layout()
    _save(fig, "markout_by_latency", out)


def plot_markout_heatmap(sens: pd.DataFrame, out: Path) -> None:
    pivot = sens.pivot(index="model", columns="submit_delay_us", values="mean_markout")
    fig, ax = plt.subplots(figsize=(7.2, 3.6))
    sns.heatmap(pivot, annot=True, fmt=".3f", cmap="RdBu_r", center=0, ax=ax, cbar_kws={"label": "price units"})
    ax.set_title("Mean signed markout (synthetic session)")
    ax.set_xlabel("Submit delay (µs)")
    ax.set_ylabel("Queue model")
    fig.tight_layout()
    _save(fig, "markout_heatmap", out)


def plot_offset(execs: pd.DataFrame, out: Path) -> None:
    part = execs[execs["submit_delay_us"] == 0].copy()
    if part.empty:
        raise RuntimeError("no executions at submit_delay_us = 0")
    grouped = (
        part.groupby(["model", "offset_ticks"], as_index=False)
        .agg(n=("order_id", "size"), mean_filled=("filled_qty", "mean"), fill_rate=("filled_qty", lambda s: float((s > 0).mean())))
    )
    fig, ax = plt.subplots(figsize=(7.2, 4.2))
    sns.barplot(data=grouped, x="offset_ticks", y="fill_rate", hue="model", ax=ax)
    ax.set_ylim(0, 1)
    ax.set_xlabel("Offset from touch (ticks)")
    ax.set_ylabel("Fill rate")
    ax.set_title("Synthetic session, submit delay 0: fill rate by distance")
    ax.legend(frameon=False, title="queue model")
    fig.tight_layout()
    _save(fig, "fill_rate_by_offset", out)
    grouped.to_csv(out / "fill_rate_by_offset.csv", index=False)


def plot_divergence(execs: pd.DataFrame, out: Path) -> None:
    fig, ax = plt.subplots(figsize=(6.4, 4.0))
    sns.barplot(data=execs, x="model", y="filled_qty", color="#4c72b0", ax=ax)
    ax.set_ylabel("Filled quantity (order 1)")
    ax.set_xlabel("Queue model")
    ax.set_title("Constructed divergence: one passive buy, not a market sample")
    fig.tight_layout()
    _save(fig, "divergence_filled_qty", out)


def plot_book(book: pd.DataFrame, out: Path) -> None:
    fig, ax = plt.subplots(figsize=(7.2, 4.0))
    ax.plot(book["time_s"], book["mid"], color="#4c72b0", linewidth=0.8)
    ax.set_xlabel("Event time (s)")
    ax.set_ylabel("Mid (price units)")
    ax.set_title("Synthetic session: mid from the replayed book")
    fig.tight_layout()
    _save(fig, "mid_path", out)

    fig, ax = plt.subplots(figsize=(6.4, 4.0))
    sns.histplot(book["spread"].dropna(), bins=20, ax=ax, color="#4c72b0")
    ax.set_xlabel("Spread (price units)")
    ax.set_ylabel("Book observations")
    ax.set_title("Synthetic session: displayed spread")
    fig.tight_layout()
    _save(fig, "spread_hist", out)

    fig, ax = plt.subplots(figsize=(7.2, 4.0))
    ax.plot(book["time_s"], book["bid_qty"], label="bid touch qty", linewidth=0.7)
    ax.plot(book["time_s"], book["ask_qty"], label="ask touch qty", linewidth=0.7)
    ax.set_xlabel("Event time (s)")
    ax.set_ylabel("Displayed quantity")
    ax.set_title("Synthetic session: displayed size at the touch")
    ax.legend(frameon=False)
    fig.tight_layout()
    _save(fig, "touch_depth", out)


def plot_interarrival(frame: pd.DataFrame, out: Path) -> None:
    times = frame["exchange_time_us"].fillna(frame["receive_time_us"]).sort_values()
    delta = times.diff().dropna()
    delta = delta[delta >= 0]
    fig, ax = plt.subplots(figsize=(6.4, 4.0))
    sns.histplot(delta / 1000.0, bins=30, ax=ax, color="#4c72b0")
    ax.set_xlabel("Inter-arrival (ms of primary event time)")
    ax.set_ylabel("Events")
    ax.set_title("Canonical event inter-arrival")
    fig.tight_layout()
    _save(fig, "interarrival", out)


def plot_markout_horizons(marks: pd.DataFrame, out: Path) -> None:
    part = marks.dropna(subset=["signed_markout"]).copy()
    if part.empty:
        raise RuntimeError("no signed markouts")
    fig, ax = plt.subplots(figsize=(7.2, 4.2))
    summary_rows = (
        part.groupby(["model", "horizon_us"], as_index=False)["signed_markout"].mean()
    )
    sns.lineplot(data=summary_rows, x="horizon_us", y="signed_markout", hue="model", marker="o", ax=ax)
    ax.axhline(0, color="black", linewidth=0.6)
    ax.set_xlabel("Horizon (µs)")
    ax.set_ylabel("Mean signed markout (price units)")
    ax.set_title("Post-fill mid change. Positive is favorable.")
    ax.legend(frameon=False, title="queue model")
    fig.tight_layout()
    _save(fig, "markout_horizons", out)
    summary_rows.to_csv(out / "markout_horizons.csv", index=False)


def plot_capture_vs_markout(execs: pd.DataFrame, marks: pd.DataFrame, out: Path) -> None:
    # One point per order at the shortest horizon, submit delay 0, so the
    # scatter is not three copies of the same order stacked across the grid.
    horizon = int(marks["horizon_us"].min())
    m = marks[(marks["horizon_us"] == horizon) & (marks["submit_delay_us"] == 0)]
    e = execs[execs["submit_delay_us"] == 0][
        ["model", "order_id", "spread_capture", "implementation_shortfall", "filled_qty"]
    ]
    joined = m.merge(e, on=["model", "order_id"], how="inner").dropna(subset=["spread_capture", "signed_markout"])
    if joined.empty:
        raise RuntimeError("no joined fill markouts")
    fig, ax = plt.subplots(figsize=(6.6, 4.4))
    sns.scatterplot(data=joined, x="spread_capture", y="signed_markout", hue="model", ax=ax, s=18, alpha=0.75)
    ax.axhline(0, color="black", linewidth=0.6)
    ax.axvline(0, color="black", linewidth=0.6)
    ax.set_xlabel("Spread capture at fill (price units)")
    ax.set_ylabel(f"Signed markout at {horizon} µs (price units)")
    ax.set_title("Spread capture is not the post-fill markout")
    ax.legend(frameon=False, title="queue model")
    fig.tight_layout()
    _save(fig, "capture_vs_markout", out)
    return joined


def plot_benchmark(bench: dict, out: Path) -> None:
    rows = [
        ("dataset replay", bench["dataset_replay"]["median_s"], bench["dataset_events"]),
        ("synthetic replay", bench["synthetic_replay"]["median_s"], bench["synthetic_events"]),
        ("one-order sim", bench["queue_sim_one_order_synthetic"]["median_s"], bench["synthetic_events"]),
        ("20k JSON roundtrips", bench["json_roundtrip_20k"]["median_s"], 20000),
    ]
    labels = [r[0] for r in rows]
    rates = [r[2] / r[1] if r[1] > 0 else np.nan for r in rows]
    fig, ax = plt.subplots(figsize=(7.2, 4.0))
    ax.barh(labels, rates, color="#4c72b0")
    ax.set_xlabel("Events per second (median wall time)")
    ax.set_title(f"{bench['cpu']} — {bench.get('profile', 'profile')} — not portable")
    fig.tight_layout()
    _save(fig, "benchmark_rates", out)


def descriptive_percentile(values: pd.Series, q: float) -> float:
    clean = values.dropna().to_numpy()
    if clean.size == 0:
        raise RuntimeError("empty sample")
    return float(stats.scoreatpercentile(clean, q))


def save_session_figures(out: Path = FIG) -> None:
    sens = sensitivity()
    execs = executions()
    marks = markouts()
    book = book_series()
    ev = events(ROOT / "data" / "processed" / "synthetic" / "session")
    plot_fill_rate(sens, out)
    plot_markout(sens, out)
    plot_markout_heatmap(sens, out)
    plot_offset(execs, out)
    plot_book(book, out)
    plot_interarrival(ev, out)
    plot_markout_horizons(marks, out)
    plot_capture_vs_markout(execs, marks, out)
    plot_benchmark(benchmark(), out)
    plot_divergence(executions(DIVERGENCE), out)
    quality_table().to_csv(out / "data_quality_table.csv", index=False)
    sens.to_csv(out / "sensitivity.csv", index=False)


def quality_table() -> pd.DataFrame:
    q = quality()
    counts = q.get("counts") or {}
    rows = [{"item": "symbol", "value": q.get("symbol")}, {"item": "synthetic", "value": q.get("synthetic")}]
    rows.append({"item": "sync_mode", "value": q.get("sync_mode")})
    rows.append({"item": "gaps", "value": q.get("gaps")})
    rows.append({"item": "duplicates", "value": q.get("duplicates")})
    rows.append({"item": "crossed_observations", "value": q.get("crossed_observations")})
    rows.append({"item": "events_consumed", "value": q.get("events_consumed")})
    rows.append({"item": "book_hash", "value": q.get("book_hash")})
    for key, value in counts.items():
        rows.append({"item": f"count_{key}", "value": value})
    inter = q.get("interarrival_us") or {}
    for key, value in inter.items():
        rows.append({"item": f"interarrival_us_{key}", "value": value})
    return pd.DataFrame(rows)
