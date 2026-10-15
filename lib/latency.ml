(** Latency components, in virtual microseconds. Nothing here sleeps.

    Fixed delays are exact. Distributional delays are scenario assumptions
    unless a dataset of measurements is supplied by the caller; this module
    does not contain measured Binance latencies. *)

type dist =
  | Fixed of int64
  | Lognormal of { mu : float; sigma : float }

type component = {
  md_us : dist;
  decision_us : dist;
  submit_us : dist;
  cancel_us : dist;
}

let fixed ~md ~decision ~submit ~cancel =
  {
    md_us = Fixed md;
    decision_us = Fixed decision;
    submit_us = Fixed submit;
    cancel_us = Fixed cancel;
  }

let sample rng = function
  | Fixed n -> if n < 0L then 0L else n
  | Lognormal { mu; sigma } ->
      let z = Rng.normal rng in
      let v = Stdlib.exp (mu +. (sigma *. z)) in
      if v < 0.0 || v > 9.0e15 then 0L else Int64.of_float v

let sample_component rng c =
  (sample rng c.md_us, sample rng c.decision_us, sample rng c.submit_us, sample rng c.cancel_us)
