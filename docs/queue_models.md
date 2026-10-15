# Queue models

The public feed does not contain queue position. All three models start from the same fiction: a new simulated order joins at the back of the displayed size at its price, after subtracting prints at that price that the book update has not yet absorbed (`displayed_effective`). Earlier simulated orders at the same price are not fictional. Their remaining quantity is `ahead_sim` and sits ahead of the new order. It is reduced when those orders fill or cancel.

## State

- `ahead_public` — estimated displayed quantity in front of us
- `ahead_sim` — known simulated quantity in front of us
- `pending_trades` — print quantity at our price since the last depth update
- `last_displayed` — displayed size at our price after the last depth update

## Unexplained decrease

On a depth update at our price:

```
visible_after = max(0, last_displayed - pending_trades)
ahead_public  = min(ahead_public, visible_after)
behind        = max(0, visible_after - ahead_public)
C             = max(0, visible_after - new_displayed)
add           = max(0, new_displayed - visible_after)
```

`C` is size that disappeared and was not explained by a print. Adds join behind and do not change `ahead_public`. `C` is not a fill.

## Where C is taken from

- Conservative: from behind first. Only the residual that cannot fit behind is taken from ahead. This is the pessimistic bound. It is not "never reduce ahead". If `C` exceeds `behind`, volume identity forces the rest out of ahead.
- Optimistic: from ahead first, capped by `ahead_public`. Favorable bound, not an estimate.
- Probabilistic: each unit of `C` is independently ahead with probability `ahead_public / (ahead_public + behind)`. The draw is `Rng.binomial` with `exact_limit = 4096`. At or below that count the draw is an exact Bernoulli sum. Above it, a seeded normal approximation is used. The cutoff is part of the definition, not an implementation detail that callers may ignore. The draw is then clamped to `[0, min(C, ahead_public)]`.

The probabilistic probability uses public ahead and behind only. `ahead_sim` is not random.

## Fills

Trades are the only fills.

- At our price: the print consumes `ahead_public`, then `ahead_sim`, then our remaining quantity.
- Strictly through our limit: ahead is cleared and the residual order fills. Labeled `trade_through` in the trace.

A later depth update must not count that print again. `pending_trades` is subtracted before `C` is computed, then reset.

## Races

A cancel of a simulated order ahead of us reduces `ahead_sim` by that order's remaining quantity at cancel-effective time. That reduction is known.

If the cancel-effective time of our own order is at or before arrival, the order is rejected with `Cancel_before_arrival` and never rests.

A fill during the in-flight window of a cancel stands. The cancel then applies only to the residual. If nothing remains, the trace records `cancel_lost_race` and the status stays filled.

A snapshot that arrives while orders are working does not know where we were in the new book. The engine rejoins at the back and warns `snapshot_resync_rejoins_at_back`. That is a pessimistic choice, not a recovery of the true slot.

## What the tests lock

- On the small divergence fixture, conservative fills 0 and optimistic fills 1 for the same order and the same events.
- A print followed by a depth update that shrinks to the post-trade size does not fill again.
- Two runs with the same seed and the same events produce the same probabilistic fills.
- The synthetic session in `reports/` does not have to separate the models. See the research note. The unit fixture is the disagreement test. The session is the sensitivity test.
