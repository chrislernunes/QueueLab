# Data contract

## Dataset directory

```
manifest.json
events.jsonl
```

`events.jsonl` is one canonical event per line, schema version 1. Integer fields are JSON integers, not floats. Prices and quantities are integers in the instrument's scale.

`manifest.json` records symbol, scales, tick, lot, market, synthetic flag, capture version, trade source, notes, event count, per-kind counts, time range, gap list, and the SHA-256 of the event file bytes.

Validation fails the process when the checksum does not match, a quantity is negative, the symbol changes mid-file, or depth sync does not reach a live book. An experiment refuses a dataset that fails validation.

## Raw capture

```
instrument.json
raw.jsonl
capture_notes.json
```

Each raw line is `{"rx_us", "kind", "payload"}`.

| kind | payload |
|---|---|
| `ws` | Combined-stream message (`stream` + `data`) or a bare event |
| `snapshot` | REST `/api/v3/depth` body (`lastUpdateId`, `bids`, `asks`) |
| `interrupt` | `{"reason": "..."}`. Becomes a canonical gap. Not applied as a book update. |

`instrument.json` is the OCaml instrument record. Scales come from `exchangeInfo` tick size and step size. The capture does not invent them.

## Canonical kinds

| kind | Identity |
|---|---|
| `snapshot` | `last_update_id`, full displayed bids and asks |
| `depth` | `U`, `u`, absolute level sizes, zero deletes |
| `trade` | `trade_id`, price, qty, `buyer_is_maker` |
| `agg_trade` | `agg_id`, first and last trade id, price, qty |
| `gap` | expected id, actual id, reason |

Exchange timestamps in Binance payloads are milliseconds. The decoder stores `ms * 1000`. That changes the unit. It does not add precision.

## Provenance

`synthetic: true` means the generator or a unit fixture produced the row. `synthetic: false` means a decoder pass over a local capture. Neither flag means the queue position was observed.

Aggregate trades are kept so a dataset can be audited. The fill engine ignores them. If a capture contains only aggregate trades, passive fills will not be generated from those prints. Do not silently substitute one stream for the other.

## Redistribution

Do not commit `data/raw/` or a live capture. Binance public market data is not relicensed by this repository. The committed fixtures under `data/processed/synthetic/` and `test/fixtures/protocol_example/` are generated or written here and are marked as such.
