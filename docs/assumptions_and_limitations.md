# Assumptions and limitations

## The data cannot show

- Which resting order was cancelled.
- The queue slot of an order that was never sent.
- Hidden or iceberg quantity.
- The matching engine's private acknowledgements.
- One-way network latency. `receive_time - exchange_time` mixes two clocks.
- A historical L2 book from the public trade dumps. Those dumps do not include diff depth.

## Inferred

Quantity ahead of a simulated order, under one of the three rules in `docs/queue_models.md`. Report the three models. Do not average them into a "true" queue.

## Scenario knobs, not measurements

Unless a config note says otherwise:

- `submit_delay_us`, `md_delay_us`, `decision_delay_us`, `cancel_delay_us`
- `fee_ppm` (100 ppm = 1 bp of notional, truncating toward zero)
- The BTCUSDT scales on the synthetic session (`price_scale` 2, `qty_scale` 4, tick 1, lot 1)
- Lognormal latency parameters, if a caller uses that sampler

The session experiment uses fixed delays. The sampler exists and rounds to integer microseconds. The float exists only inside the draw.

## Accounting

Cash is quote units of `price * qty` in scaled integers, net of fees. Position is signed, positive long.

```
equity = cash + mark * position
equity = realized + unrealized(mark)
```

Fees reduce cash and realized immediately, including fees on opens. A fill that flips through zero charges the whole fee to realized and sets average cost to the flip price. The fee is not split.

Spread capture and implementation shortfall in the CSV are price-unit, quantity-weighted averages of the fills. They do not subtract the fee a second time. Positive spread capture is not positive net P&L.

Signed markout uses a later mid. It is ex-post. It is not an input to the decision.

Unfilled orders are not given a fake fill price. Their opportunity is simply that they did not trade. Do not fold a counterfactual mid into the fill shortfall column.

## Bias

- Trade-through fills the residual even when the print might have been an off-book or inconsistency print.
- Join-at-back is pessimistic relative to an order that somehow joined in the middle. Nothing in this feed supports a middle join.
- Conservative vs optimistic is a bound on unexplained cancels, not a confidence interval.
- The synthetic session's fill rate is insensitive to the model because the generator often consumes a whole level. Quoting that insensitivity as a market result is a misuse.
- Same-timestamp trade-before-depth can attribute a print that the exchange actually emitted after the book change, or the reverse. The policy is documented because the alternative double-counts.
- `Int64` multiplication in the ledger wraps on overflow. The tests use small notionals. A production notional near 2^63 will not raise.
- The probabilistic normal tail for `n > 4096` is not an exact binomial.
- Block bootstrap does not remove all dependence, and the percentile interval is not a proven 95% coverage statement.
- Aggressor side is stored and not otherwise used to refuse a print.

## Look-ahead

The decision reads the book at decision time. Horizons read later mids only when writing markouts. The order grid is a function of the clock in the config, not of future prices.
