(** Fixed-point decimal parsing. No floating point.

    [parse_fixed "10.5" ~scale:2 = Ok 1050]. Extra fractional digits are an
    error (no silent rounding). The result is an integer in units of
    [10^(-scale)]. *)

let overflow = Error "overflow"

let mul a b =
  if a = 0L || b = 0L then Ok 0L
  else
    let r = Int64.mul a b in
    if Int64.div r a = b then Ok r else overflow

let add a b =
  let r = Int64.add a b in
  if a >= 0L && b >= 0L && r < 0L then overflow
  else if a < 0L && b < 0L && r >= 0L then overflow
  else Ok r

let pow10 n =
  let rec go i acc =
    if i = 0 then Ok acc
    else match mul acc 10L with Ok a -> go (i - 1) a | Error _ as e -> e
  in
  go n 1L

let parse_fixed s ~scale =
  if scale < 0 || scale > 18 then Error "scale out of range"
  else
    let s = String.trim s in
    if s = "" then Error "empty decimal"
    else
      let neg, i0 =
        if s.[0] = '-' then true, 1 else if s.[0] = '+' then false, 1 else false, 0
      in
      if i0 >= String.length s then Error "empty decimal"
      else
        let len = String.length s in
        let rec scan i int_acc frac_acc frac_digits seen_dot =
          if i = len then Ok (int_acc, frac_acc, frac_digits)
          else
            let c = s.[i] in
            if c = '.' then
              if seen_dot then Error "multiple decimal points"
              else scan (i + 1) int_acc frac_acc frac_digits true
            else if c < '0' || c > '9' then Error ("bad decimal char " ^ String.make 1 c)
            else
              let d = Int64.of_int (Char.code c - 48) in
              if not seen_dot then
                match mul int_acc 10L with
                | Error _ -> overflow
                | Ok n -> (
                    match add n d with
                    | Error _ -> overflow
                    | Ok n -> scan (i + 1) n frac_acc frac_digits false)
              else if frac_digits >= scale then
                if d = 0L then scan (i + 1) int_acc frac_acc frac_digits true
                else Error "extra fractional digits"
              else
                match mul frac_acc 10L with
                | Error _ -> overflow
                | Ok n -> scan (i + 1) int_acc (Int64.add n d) (frac_digits + 1) true
        in
        match scan i0 0L 0L 0 false with
        | Error _ as e -> e
        | Ok (int_acc, frac_acc, frac_digits) -> (
            match pow10 scale with
            | Error _ -> overflow
            | Ok scale_factor -> (
                match mul int_acc scale_factor with
                | Error _ -> overflow
                | Ok scaled ->
                    let pad = scale - frac_digits in
                    let rec pad_frac acc k =
                      if k = 0 then Ok acc
                      else match mul acc 10L with Ok a -> pad_frac a (k - 1) | Error _ as e -> e
                    in
                    match pad_frac frac_acc pad with
                    | Error _ -> overflow
                    | Ok frac -> (
                        match add scaled frac with
                        | Error _ -> overflow
                        | Ok mag -> Ok (if neg then Int64.neg mag else mag))))

let format n ~scale =
  if scale = 0 then Int64.to_string n
  else
    let neg = n < 0L in
    let mag = if neg then Int64.neg n else n in
    let factor =
      match pow10 scale with Ok f -> f | Error _ -> failwith "format scale"
    in
    let ip = Int64.div mag factor in
    let fp = Int64.rem mag factor in
    let frac = Printf.sprintf "%0*Ld" scale fp in
    let body = Int64.to_string ip ^ "." ^ frac in
    if neg then "-" ^ body else body
