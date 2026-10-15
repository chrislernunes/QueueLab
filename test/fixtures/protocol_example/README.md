# Protocol example

Hand-written Binance-shaped JSON. Not a market capture and not for research claims.

`raw.jsonl` is enough to exercise `queuelab normalize` offline:

- one depth update with `u <= lastUpdateId`, which the sync drops
- one REST-shaped snapshot
- one bridging depth update
- one raw trade
- one duplicate of the bridging update

Prices and quantities match `instrument.json`.
