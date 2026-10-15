# Validation protocol

## Integrity

`queuelab validate` recomputes the event-file SHA-256, checks the manifest, replays, and returns exit 1 when sync is not live or a check fails. Gaps, duplicate counts and crossed-book observations are in the JSON either way.

`queuelab experiment` calls that check first and does not write a result for an invalid dataset.

## Replay

Two full replays of the same file produce the same book hash. A checkpoint taken at an offset, restored, and fed the suffix (not re-sorted) matches the full-replay hash. The checkpoint is the depth-sync state. The pre-snapshot buffer is not part of the saved checkpoint; restore is defined after a snapshot has been applied.

## Book

Off-tick, off-lot and negative sizes are rejected. Zero deletes. Best bid, best ask, spread, mid and microprice have direct tests. A QCheck run applies random level updates and checks the book invariants after each accepted update.

## Sync

- Duplicate final id does not move the book.
- A gap stops application.
- The hand-built file `test/fixtures/protocol_example` normalizes offline: drop `u <= lastUpdateId`, bridge on the next id, count the repeated id as a duplicate.

## Execution

- Conservative fill 0 and optimistic fill 1 on the shared divergence events.
- Same probabilistic seed, same fills.
- A trade and the depth update that reflects it do not fill twice.
- Trade-through fills the residual.
- Cancel effective at or before arrival never rests.
- A partial fill then a later cancel keeps the filled quantity and ends `Cancelled`.
- No fill after a terminal state. Overfill is rejected.
- Simulated orders do not change the book hash.
- Ledger identity holds for a round trip and for a flip. A 100 ppm fee on notional 10000 is 1.

## Acceptance

A change that breaks any of the above is a defect, including a change that makes the research tables look more interesting. Session fill rates are allowed to be equal across models. They are not allowed to be silently forced apart.

`dune runtest` is necessary and not sufficient. Read a trace in `reports/divergence` (the config sets `trace` true) before claiming a fill.
