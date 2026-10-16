# Architecture

QueueLab is a sequential, deterministic research engine. Network capture is outside the simulator. Figure scripts read files the simulator already wrote.

## Libraries

| Module | Role |
|---|---|
| `decimal` | Parse a decimal string into an integer at a fixed scale. Extra fractional digits are an error. No rounding. |
| `instrument` | Symbol, price scale, quantity scale, tick, lot. |
| `event` | Canonical events: snapshot, depth, trade, aggregate trade, gap. |
| `order_book` | `Map` of price to displayed quantity. Hash is MD5 of a canonical dump. |
| `depth_sync` | Binance spot diff-depth rules. Checkpoint state is this record, not the book alone. |
| `replay` | Total order, apply, prefix, resume. |
| `queue_model` | Conservative, probabilistic, optimistic. |
| `lifecycle` | Explicit order states. Illegal transitions return an error. |
| `latency` | Fixed delay, or a lognormal draw rounded to integer microseconds. |
| `fill_engine` | Virtual clock. Simulated orders never enter the public book. |
| `fees`, `ledger` | Integer notional and an average-cost inventory. |
| `metrics` | Circular block bootstrap. |
| `binance_decoder` | Public JSON to canonical events. |
| `manifest`, `event_log`, `validation` | Dataset files, checksums, integrity report. |
| `experiment` | Config grid, CSV and JSON outputs. |
| `synthetic` | Labeled fixtures. |
| `benchmark` | Wall-clock timings of this binary. |

The public command is `queuelab` (`bin/main.ml`).

## What is observed, inferred, simulated

- Observed: canonical events and the displayed book built from them.
- Inferred: quantity ahead of a hypothetical order. The feed does not contain it.
- Simulated: order states, fills, fees, ledger. A fill is not written back as a trade.

## Clocks

Replay orders by exchange time when it is non-zero, otherwise by receive time. It does not convert one clock into the other.

On a tie the order is snapshot, trade, aggregate trade, depth, gap, then `seq_index`. Trades sit before a depth update with the same timestamp so the queue model can treat the print as the explanation of the size change. That is a policy, not a claim about exchange causality.

Simulation time for an event is exchange time plus `md_delay`. Actions scheduled at time T run before market events at T. The price is chosen from the book visible at decision time and is not revised with later data.

## Standard library

The OCaml side uses the compiler standard library plus Yojson. It does not use Core, Base or Async. Capture I/O is `scripts/capture_binance.py` (stdlib TLS). The depth-sync state machine it feeds is OCaml and is tested without a socket.

## Dependencies

One direction: decoder and synthetic produce events, replay consumes them, the fill engine consumes a replayed stream plus intents, the experiment writer consumes the fill engine. Python figure scripts are not imported by OCaml.
