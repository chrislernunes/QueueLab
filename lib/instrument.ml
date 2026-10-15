(** Instrument tick/lot rules. Prices and quantities in the engine are integers
    in units of [10^(-price_scale)] and [10^(-qty_scale)]. [tick] and [lot]
    are multiples of those units (usually 1). *)

type t = {
  symbol : string;
  price_scale : int;
  qty_scale : int;
  tick : int64;
  lot : int64;
}

let make ~symbol ~price_scale ~qty_scale ~tick ~lot =
  if price_scale < 0 || qty_scale < 0 then Error "negative scale"
  else if tick <= 0L || lot <= 0L then Error "tick and lot must be positive"
  else Ok { symbol; price_scale; qty_scale; tick; lot }

let syn =
  { symbol = "SYN"; price_scale = 0; qty_scale = 0; tick = 1L; lot = 1L }

let check_price t p =
  if Int64.rem p t.tick <> 0L then Error (Printf.sprintf "off tick %Ld" p) else Ok ()

let check_qty t q =
  if q < 0L then Error "negative quantity"
  else if Int64.rem q t.lot <> 0L then Error (Printf.sprintf "off lot %Ld" q)
  else Ok ()

let parse_price t s =
  match Decimal.parse_fixed s ~scale:t.price_scale with
  | Error e -> Error e
  | Ok p -> ( match check_price t p with Ok () -> Ok p | Error e -> Error e)

let parse_qty t s =
  match Decimal.parse_fixed s ~scale:t.qty_scale with
  | Error e -> Error e
  | Ok q -> ( match check_qty t q with Ok () -> Ok q | Error e -> Error e)

let to_json t =
  Jsonx.obj
    [
      ("symbol", Jsonx.str t.symbol);
      ("price_scale", Jsonx.int t.price_scale);
      ("qty_scale", Jsonx.int t.qty_scale);
      ("tick", Jsonx.i64 t.tick);
      ("lot", Jsonx.i64 t.lot);
    ]

let of_json j =
  let symbol = Jsonx.string_of (Jsonx.require "symbol" j) in
  let price_scale = Int64.to_int (Jsonx.i64_of (Jsonx.require "price_scale" j)) in
  let qty_scale = Int64.to_int (Jsonx.i64_of (Jsonx.require "qty_scale" j)) in
  let tick = Jsonx.i64_of (Jsonx.require "tick" j) in
  let lot = Jsonx.i64_of (Jsonx.require "lot" j) in
  match make ~symbol ~price_scale ~qty_scale ~tick ~lot with
  | Ok t -> t
  | Error e -> failwith e
