# Data

| Path | What it is | Commit? |
|---|---|---|
| `data/processed/synthetic/session` | 4000-step seeded book, `synthetic: true` | yes |
| `data/processed/synthetic/divergence` | Five-event cancellation scenario, `synthetic: true` | yes |
| `test/fixtures/protocol_example` | Hand-written Binance-shaped messages | yes |
| `data/raw/` | Live capture | no |
| `data/processed/<capture>/` | Normalized capture | no |

Nothing under `synthetic/` or `protocol_example/` is a Binance history. The symbol `BTCUSDT` on the session fixture is a label on a generated book, with assumed scales documented in `docs/assumptions_and_limitations.md`.

Capture with:

```
dune exec queue_positioning -- capture --symbol BTCUSDT --duration-seconds 30 --out data/raw/btcusdt-30
```

That process uses public streams only. It does not read an API key and it does not place orders. Normalization exits 1 if the depth sequence does not stay live. Keep the raw file if you want to audit the gap. Do not commit it.
