(** Maker fee in integer parts-per-million of quote notional.

    [fee = notional * fee_ppm / 1_000_000], truncated toward zero.
    100 ppm = 1 basis point. This is a scenario assumption, not a Binance
    fee-tier lookup. *)

let quote_notional ~price ~qty = Int64.mul price qty

let fee ~notional ~fee_ppm =
  if fee_ppm < 0L then failwith "negative fee"
  else Int64.div (Int64.mul notional fee_ppm) 1_000_000L
