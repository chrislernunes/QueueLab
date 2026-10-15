(** Price-level book. Keys are integer prices. [Map] gives O(log n) updates.
    The book stores observed displayed depth only. Simulated orders are not
    inserted here. *)

module M = Map.Make (Int64)

type side = Bid | Ask

type t = {
  inst : Instrument.t;
  bids : int64 M.t;
  asks : int64 M.t;
  last_update_id : int64;
  exchange_time_us : int64;
  update_count : int64;
}

type diagnostic = {
  negative_rejected : int;
  off_tick_rejected : int;
  crossed : bool;
  empty_bid : bool;
  empty_ask : bool;
}

let empty inst =
  {
    inst;
    bids = M.empty;
    asks = M.empty;
    last_update_id = 0L;
    exchange_time_us = 0L;
    update_count = 0L;
  }

let side_map t = function Bid -> t.bids | Ask -> t.asks

let qty_at t side price =
  match M.find_opt price (side_map t side) with Some q -> q | None -> 0L

let best_bid t = M.max_binding_opt t.bids
let best_ask t = M.min_binding_opt t.asks

let spread t =
  match (best_bid t, best_ask t) with
  | Some (b, _), Some (a, _) -> Some (Int64.sub a b)
  | _ -> None

let mid t =
  match (best_bid t, best_ask t) with
  | Some (b, _), Some (a, _) -> Some (Int64.div (Int64.add a b) 2L)
  | _ -> None

(** Microprice = (ask * bid_qty + bid * ask_qty) / (bid_qty + ask_qty),
    integer division toward zero. Undefined when either touch qty is zero
    or a touch is missing. *)
let microprice t =
  match (best_bid t, best_ask t) with
  | Some (b, bq), Some (a, aq) ->
      let denom = Int64.add bq aq in
      if denom = 0L then None
      else
        let num = Int64.add (Int64.mul a bq) (Int64.mul b aq) in
        Some (Int64.div num denom)
  | _ -> None

let is_crossed t =
  match (best_bid t, best_ask t) with
  | Some (b, _), Some (a, _) -> b >= a
  | _ -> false

let top t side n =
  let m = side_map t side in
  let seq =
    match side with
    | Bid -> List.rev (M.bindings m)
    | Ask -> M.bindings m
  in
  let rec take k acc = function
    | _ when k = 0 -> List.rev acc
    | [] -> List.rev acc
    | (p, q) :: rest -> take (k - 1) ((p, q) :: acc) rest
  in
  take n [] seq

let set_raw side_map price qty =
  if qty = 0L then M.remove price side_map else M.add price qty side_map

type reject = [ `Negative | `Off_tick | `Off_lot ]

let apply_level t side price qty =
  match Instrument.check_price t.inst price with
  | Error _ -> Error `Off_tick
  | Ok () -> (
      match Instrument.check_qty t.inst qty with
      | Error _ when qty < 0L -> Error `Negative
      | Error _ -> Error `Off_lot
      | Ok () ->
          let t =
            match side with
            | Bid -> { t with bids = set_raw t.bids price qty }
            | Ask -> { t with asks = set_raw t.asks price qty }
          in
          Ok t)

let apply_levels t side levels =
  let rec go t rejected = function
    | [] -> (t, rejected)
    | (price, qty) :: rest -> (
        match apply_level t side price qty with
        | Ok t -> go t rejected rest
        | Error r -> go t (r :: rejected) rest)
  in
  go t [] levels

let finalize t ~final_id ~exchange_time_us =
  {
    t with
    last_update_id = final_id;
    exchange_time_us;
    update_count = Int64.succ t.update_count;
  }

let levels_of_map m =
  M.bindings m |> List.map (fun (price, qty) -> { Event.px = price; sz = qty })

let to_json t =
  Jsonx.obj
    [
      ("last_update_id", Jsonx.i64 t.last_update_id);
      ("exchange_time_us", Jsonx.i64 t.exchange_time_us);
      ("update_count", Jsonx.i64 t.update_count);
      ("instrument", Instrument.to_json t.inst);
      ( "bids",
        Jsonx.list
          (List.map
             (fun (p, q) -> Jsonx.list [ Jsonx.i64 p; Jsonx.i64 q ])
             (M.bindings t.bids)) );
      ( "asks",
        Jsonx.list
          (List.map
             (fun (p, q) -> Jsonx.list [ Jsonx.i64 p; Jsonx.i64 q ])
             (M.bindings t.asks)) );
    ]

let of_json j =
  let inst = Instrument.of_json (Jsonx.require "instrument" j) in
  let pair el =
    match Jsonx.list_of el with
    | [ p; q ] -> (Jsonx.i64_of p, Jsonx.i64_of q)
    | _ -> failwith "book level"
  in
  let load m el =
    let p, q = pair el in
    if q < 0L then failwith "negative qty in checkpoint" else M.add p q m
  in
  let bids = List.fold_left load M.empty (Jsonx.list_of (Jsonx.require "bids" j)) in
  let asks = List.fold_left load M.empty (Jsonx.list_of (Jsonx.require "asks" j)) in
  {
    inst;
    bids;
    asks;
    last_update_id = Jsonx.i64_of (Jsonx.require "last_update_id" j);
    exchange_time_us = Jsonx.i64_of (Jsonx.require "exchange_time_us" j);
    update_count = Jsonx.i64_of (Jsonx.require "update_count" j);
  }

let canonical_string t =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Int64.to_string t.last_update_id);
  Buffer.add_char buf '|';
  M.iter
    (fun p q ->
      Buffer.add_string buf (Int64.to_string p);
      Buffer.add_char buf ':';
      Buffer.add_string buf (Int64.to_string q);
      Buffer.add_char buf ',')
    t.bids;
  Buffer.add_char buf '|';
  M.iter
    (fun p q ->
      Buffer.add_string buf (Int64.to_string p);
      Buffer.add_char buf ':';
      Buffer.add_string buf (Int64.to_string q);
      Buffer.add_char buf ',')
    t.asks;
  Buffer.contents buf

let hash t = Digest.to_hex (Digest.string (canonical_string t))

let invariant_violations t =
  let v = ref [] in
  let check side m =
    M.iter
      (fun p q ->
        if q <= 0L then v := "non-positive qty" :: !v;
        if Int64.rem p t.inst.tick <> 0L then v := "off tick" :: !v)
      m;
    ignore side
  in
  check Bid t.bids;
  check Ask t.asks;
  !v
