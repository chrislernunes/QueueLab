# Exchange semantics

Source used for the spot depth procedure: Binance spot web-socket streams, "How to manage a local order book". This file describes what the code does. It is not a second copy of the exchange manual, and it is not a matching-engine specification.

## Depth sync

Implemented in `lib/depth_sync.ml`.

1. Before a snapshot, diffs are buffered.
2. The snapshot replaces the book.
3. Buffered diffs with `u <= lastUpdateId` are dropped.
4. The first kept diff must satisfy `U <= lastUpdateId + 1` and `u >= lastUpdateId + 1`.
5. Each later diff must satisfy `U = previous u + 1`.
6. `u <=` the cursor is a duplicate and is not applied.
7. Any other step is a gap. The book is marked broken. Later diffs are not applied across the gap.
8. Level quantity is absolute. Zero removes the level.

Spot diffs in the documentation this code was written against do not carry `pu`. The decoder does not invent one.

A REST snapshot has no event time. The decoder stores the local receive time in the snapshot's exchange-time field so the replay has a number to sort. That mixes a local clock into a field that is otherwise an exchange clock. Clock skew can place the snapshot on the wrong side of a diff. The capture writes the snapshot when it is taken and the sync rules above still have to hold. A failed bridge is a gap, not a guessed book.

## Trades

A raw trade is one print (`t`, `p`, `q`, `m`, `T`). An aggregate trade covers a range of trade ids. They are different canonical kinds.

`buyer_is_maker` is stored. The queue model does not currently split the print by aggressor side beyond the buy/sell price interaction: a trade at the order's price consumes the queue from the front, and a trade strictly through the limit fills the residual. A sell print at the bid is the usual passive-buy case. A print through the bid is treated as price priority over the worse price. That is biased if the print was off-book or the visible book was already inconsistent.

## What the book is not

The visible book is market-by-price. It does not identify resting orders, queue slots, or which of several orders cancelled. Hidden size is not in the message. Simulated orders are not inserted into the book, so a replay hash does not depend on the queue model.

## Historical files

Binance's public historical data dumps provide trades, aggregate trades and klines. They do not provide the incremental depth stream this sync procedure needs. Rebuilding a book for a past day from those dumps is not implemented, because it would require fabricating diffs. Capture forward, or replay a dataset you already stored.

## Limits, not a venue simulator

Tick and lot checks reject an order that is off-grid. A limit that would lock or cross the visible opposite touch is rejected (`Would_cross`). There is no aggressive taking path. There is no STP, no iceberg, no self-match of simulated orders against each other except through `ahead_sim` (earlier simulated quantity at the same price is known and sits ahead). Replacement is not a separate message: cancel the old id and submit a new one.

None of these transitions are Binance acknowledgements.
