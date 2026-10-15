(** SplitMix64. Used everywhere a seed is required. Floating point appears only
    inside [float_unit], [normal] and the binomial sampler. *)

type t = int64 ref

let of_hex s =
  let r = ref 0L in
  String.iter
    (fun c ->
      let d =
        if c >= '0' && c <= '9' then Char.code c - 48
        else if c >= 'a' && c <= 'f' then Char.code c - 87
        else if c >= 'A' && c <= 'F' then Char.code c - 55
        else failwith "rng hex"
      in
      r := Int64.logor (Int64.shift_left !r 4) (Int64.of_int d))
    s;
  !r

let c1 = of_hex "9E3779B97F4A7C15"
let c2 = of_hex "BF58476D1CE4E5B9"
let c3 = of_hex "94D049BB133111EB"

let create seed = ref seed

let next t =
  let z = Int64.add !t c1 in
  t := z;
  let z = Int64.mul (Int64.logxor z (Int64.shift_right_logical z 30)) c2 in
  let z = Int64.mul (Int64.logxor z (Int64.shift_right_logical z 27)) c3 in
  Int64.logxor z (Int64.shift_right_logical z 31)

let int t n =
  if n <= 0 then failwith "Rng.int";
  Int64.to_int (Int64.rem (Int64.shift_right_logical (next t) 1) (Int64.of_int n))
  |> fun x -> if x < 0 then -x mod n else x

let float_unit t =
  let x = next t in
  let n = Int64.to_float (Int64.shift_right_logical x 11) in
  n /. 9007199254740992.0

let normal t =
  let u1 = max 1e-12 (float_unit t) in
  let u2 = float_unit t in
  Stdlib.sqrt (-2.0 *. Stdlib.log u1) *. Stdlib.cos (2.0 *. Float.pi *. u2)

let bernoulli t p = float_unit t < p

(** Seeded binomial. Exact Bernoulli sum up to [exact_limit]; above that a
    normal approximation with continuity correction, clamped to [[0, n]].
    The approximation is part of the model definition, not an implementation
    accident: tests pin the exact regime. *)
let binomial t ~n ~p ~exact_limit =
  if n <= 0L || p <= 0.0 then 0L
  else if p >= 1.0 then n
  else if n <= exact_limit then (
    let n_i = Int64.to_int n in
    let rec loop i acc =
      if i = n_i then Int64.of_int acc
      else loop (i + 1) (acc + if bernoulli t p then 1 else 0)
    in
    loop 0 0)
  else
    let nf = Int64.to_float n in
    let mean = nf *. p in
    let sd = Stdlib.sqrt (nf *. p *. (1.0 -. p)) in
    let z = normal t in
    let x = Stdlib.floor ((mean +. (z *. sd)) +. 0.5) in
    let xi =
      if x <= 0.0 then 0L
      else if x >= nf then n
      else Int64.of_float x
    in
    if xi < 0L then 0L else if xi > n then n else xi
