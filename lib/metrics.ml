(** Summary statistics. Bootstrap intervals are circular block bootstrap so
    adjacent orders are not treated as independent. The block length defaults
    to [max(1, floor(sqrt n))]. Intervals are percentile intervals of the
    resampled statistic, not a claim of correct coverage under every
    dependence structure. *)

let mean_float xs =
  let n = Array.length xs in
  if n = 0 then None
  else
    let s = Array.fold_left ( +. ) 0.0 xs in
    Some (s /. float n)

let sorted_copy xs =
  let a = Array.copy xs in
  Array.sort Float.compare a;
  a

let quantile_sorted a p =
  let n = Array.length a in
  if n = 0 then None
  else
    let idx = int_of_float (p *. float (n - 1)) in
    let idx = if idx < 0 then 0 else if idx >= n then n - 1 else idx in
    Some a.(idx)

let block_bootstrap rng ~block ~samples ~stat xs =
  let n = Array.length xs in
  if n = 0 || samples <= 1 then None
  else
    let b = max 1 (min block n) in
    let reps = Array.make samples 0.0 in
    for i = 0 to samples - 1 do
      let buf = Array.make n xs.(0) in
      let j = ref 0 in
      while !j < n do
        let start = Rng.int rng n in
        let k = ref 0 in
        while !k < b && !j < n do
          buf.(!j) <- xs.((start + !k) mod n);
          incr j;
          incr k
        done
      done;
      reps.(i) <- stat buf
    done;
    Array.sort Float.compare reps;
    let at p =
      let idx = int_of_float (p *. float (samples - 1)) in
      reps.(idx)
    in
    Some (at 0.025, at 0.975)
