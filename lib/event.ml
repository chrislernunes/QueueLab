(** Canonical events. Observed exchange events and explicit gap markers.
    Simulated fills are never represented with these constructors. *)

type level = { px : int64; sz : int64 }

type trade = {
  trade_id : int64;
  price : int64;
  qty : int64;
  buyer_is_maker : bool;
  exchange_time_us : int64;
}

type agg_trade = {
  agg_id : int64;
  first_trade_id : int64;
  last_trade_id : int64;
  price : int64;
  qty : int64;
  buyer_is_maker : bool;
  exchange_time_us : int64;
}

type depth = {
  first_id : int64;
  final_id : int64;
  bids : level list;
  asks : level list;
  exchange_time_us : int64;
}

type snapshot = {
  last_update_id : int64;
  bids : level list;
  asks : level list;
  exchange_time_us : int64;
}

type gap = { expected_id : int64; actual_id : int64; reason : string }

type kind =
  | Snapshot of snapshot
  | Depth of depth
  | Trade of trade
  | Agg_trade of agg_trade
  | Gap of gap

type t = {
  schema : int;
  symbol : string;
  kind : kind;
  receive_time_us : int64;
  seq_index : int64;
  synthetic : bool;
  provenance : string;
}

let schema_version = 1

let exchange_time e =
  match e.kind with
  | Snapshot s -> s.exchange_time_us
  | Depth d -> d.exchange_time_us
  | Trade t -> t.exchange_time_us
  | Agg_trade a -> a.exchange_time_us
  | Gap _ -> 0L

(** Total order used by replay.

    1. Exchange time when present, otherwise receive time. These clocks are not
       converted into each other.
    2. On a timestamp tie, trades are ordered before depth updates so a trade
       and the depth change that shares its timestamp are not double-counted
       by the queue model. This is a documented tie policy, not a claim that
       the exchange emitted them in that order.
    3. [seq_index] (source order) breaks any remaining tie. *)
let kind_rank = function
  | Snapshot _ -> 0
  | Trade _ -> 1
  | Agg_trade _ -> 2
  | Depth _ -> 3
  | Gap _ -> 4

let primary_time e =
  let ex = exchange_time e in
  if ex <> 0L then ex else e.receive_time_us

let compare a b =
  let c = Int64.compare (primary_time a) (primary_time b) in
  if c <> 0 then c
  else
    let c = Int.compare (kind_rank a.kind) (kind_rank b.kind) in
    if c <> 0 then c else Int64.compare a.seq_index b.seq_index

let kind_name = function
  | Snapshot _ -> "snapshot"
  | Depth _ -> "depth"
  | Trade _ -> "trade"
  | Agg_trade _ -> "agg_trade"
  | Gap _ -> "gap"

let levels_to_json (xs : level list) =
  Jsonx.list
    (List.map
       (fun (l : level) -> Jsonx.list [ Jsonx.i64 l.px; Jsonx.i64 l.sz ])
       xs)

let levels_of_json j =
  List.map
    (fun el ->
      match Jsonx.list_of el with
      | [ p; q ] -> ({ px = Jsonx.i64_of p; sz = Jsonx.i64_of q } : level)
      | _ -> failwith "level")
    (Jsonx.list_of j)

let to_json e =
  let body =
    match e.kind with
    | Snapshot s ->
        [
          ("kind", Jsonx.str "snapshot");
          ("last_update_id", Jsonx.i64 s.last_update_id);
          ("bids", levels_to_json s.bids);
          ("asks", levels_to_json s.asks);
          ("exchange_time_us", Jsonx.i64 s.exchange_time_us);
        ]
    | Depth d ->
        [
          ("kind", Jsonx.str "depth");
          ("U", Jsonx.i64 d.first_id);
          ("u", Jsonx.i64 d.final_id);
          ("bids", levels_to_json d.bids);
          ("asks", levels_to_json d.asks);
          ("exchange_time_us", Jsonx.i64 d.exchange_time_us);
        ]
    | Trade t ->
        [
          ("kind", Jsonx.str "trade");
          ("trade_id", Jsonx.i64 t.trade_id);
          ("price", Jsonx.i64 t.price);
          ("qty", Jsonx.i64 t.qty);
          ("buyer_is_maker", Jsonx.bool t.buyer_is_maker);
          ("exchange_time_us", Jsonx.i64 t.exchange_time_us);
        ]
    | Agg_trade a ->
        [
          ("kind", Jsonx.str "agg_trade");
          ("agg_id", Jsonx.i64 a.agg_id);
          ("first_trade_id", Jsonx.i64 a.first_trade_id);
          ("last_trade_id", Jsonx.i64 a.last_trade_id);
          ("price", Jsonx.i64 a.price);
          ("qty", Jsonx.i64 a.qty);
          ("buyer_is_maker", Jsonx.bool a.buyer_is_maker);
          ("exchange_time_us", Jsonx.i64 a.exchange_time_us);
        ]
    | Gap g ->
        [
          ("kind", Jsonx.str "gap");
          ("expected_id", Jsonx.i64 g.expected_id);
          ("actual_id", Jsonx.i64 g.actual_id);
          ("reason", Jsonx.str g.reason);
        ]
  in
  Jsonx.obj
    ([
       ("schema", Jsonx.int e.schema);
       ("symbol", Jsonx.str e.symbol);
       ("receive_time_us", Jsonx.i64 e.receive_time_us);
       ("seq_index", Jsonx.i64 e.seq_index);
       ("synthetic", Jsonx.bool e.synthetic);
       ("provenance", Jsonx.str e.provenance);
     ]
    @ body)

let of_json j =
  let schema = Int64.to_int (Jsonx.i64_of (Jsonx.require "schema" j)) in
  if schema <> schema_version then failwith "unsupported event schema";
  let symbol = Jsonx.string_of (Jsonx.require "symbol" j) in
  let receive_time_us = Jsonx.i64_of (Jsonx.require "receive_time_us" j) in
  let seq_index = Jsonx.i64_of (Jsonx.require "seq_index" j) in
  let synthetic = Jsonx.bool_of (Jsonx.require "synthetic" j) in
  let provenance = Jsonx.string_of (Jsonx.require "provenance" j) in
  let kind =
    match Jsonx.string_of (Jsonx.require "kind" j) with
    | "snapshot" ->
        Snapshot
          {
            last_update_id = Jsonx.i64_of (Jsonx.require "last_update_id" j);
            bids = levels_of_json (Jsonx.require "bids" j);
            asks = levels_of_json (Jsonx.require "asks" j);
            exchange_time_us = Jsonx.i64_of (Jsonx.require "exchange_time_us" j);
          }
    | "depth" ->
        Depth
          {
            first_id = Jsonx.i64_of (Jsonx.require "U" j);
            final_id = Jsonx.i64_of (Jsonx.require "u" j);
            bids = levels_of_json (Jsonx.require "bids" j);
            asks = levels_of_json (Jsonx.require "asks" j);
            exchange_time_us = Jsonx.i64_of (Jsonx.require "exchange_time_us" j);
          }
    | "trade" ->
        Trade
          {
            trade_id = Jsonx.i64_of (Jsonx.require "trade_id" j);
            price = Jsonx.i64_of (Jsonx.require "price" j);
            qty = Jsonx.i64_of (Jsonx.require "qty" j);
            buyer_is_maker = Jsonx.bool_of (Jsonx.require "buyer_is_maker" j);
            exchange_time_us = Jsonx.i64_of (Jsonx.require "exchange_time_us" j);
          }
    | "agg_trade" ->
        Agg_trade
          {
            agg_id = Jsonx.i64_of (Jsonx.require "agg_id" j);
            first_trade_id = Jsonx.i64_of (Jsonx.require "first_trade_id" j);
            last_trade_id = Jsonx.i64_of (Jsonx.require "last_trade_id" j);
            price = Jsonx.i64_of (Jsonx.require "price" j);
            qty = Jsonx.i64_of (Jsonx.require "qty" j);
            buyer_is_maker = Jsonx.bool_of (Jsonx.require "buyer_is_maker" j);
            exchange_time_us = Jsonx.i64_of (Jsonx.require "exchange_time_us" j);
          }
    | "gap" ->
        Gap
          {
            expected_id = Jsonx.i64_of (Jsonx.require "expected_id" j);
            actual_id = Jsonx.i64_of (Jsonx.require "actual_id" j);
            reason = Jsonx.string_of (Jsonx.require "reason" j);
          }
    | k -> failwith ("unknown kind " ^ k)
  in
  { schema; symbol; kind; receive_time_us; seq_index; synthetic; provenance }
