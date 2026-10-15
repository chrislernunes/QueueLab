(** Binance spot diff-depth synchronization.

    Official rule (spot web-socket streams, "How to manage a local order book"):

    - Drop any event with [u <= lastUpdateId] of the snapshot.
    - The first processed event must satisfy
      [U <= lastUpdateId + 1] and [u >= lastUpdateId + 1].
    - Each later event must satisfy [U = previous_u + 1].
    - Quantities are absolute. Zero deletes the level.
    - [u <= local id] is a duplicate and is ignored.
    - [U <> previous_u + 1] with [u] still past the cursor is a gap. The book is
      marked broken. Events are not applied across a gap.

    Spot depth diffs in the current public docs do not carry a [pu] field.
    This module does not invent one. *)

type diff = {
  first_id : int64;
  final_id : int64;
  bids : (int64 * int64) list;
  asks : (int64 * int64) list;
  exchange_time_us : int64;
}

type mode =
  | Buffering
  | Awaiting_bridge of { snapshot_id : int64 }
  | Live of { prev_u : int64 }
  | Broken of string

type t = {
  book : Order_book.t;
  mode : mode;
  buffer : diff list;
  duplicates : int;
  applied : int;
  gaps : (int64 * int64 * string) list;
  rejected_levels : int;
}

let create inst =
  {
    book = Order_book.empty inst;
    mode = Buffering;
    buffer = [];
    duplicates = 0;
    applied = 0;
    gaps = [];
    rejected_levels = 0;
  }

let fail t expected actual reason =
  { t with mode = Broken reason; gaps = (expected, actual, reason) :: t.gaps }

let apply_diff t diff =
  let book, rb = Order_book.apply_levels t.book Order_book.Bid diff.bids in
  let book, ra = Order_book.apply_levels book Order_book.Ask diff.asks in
  let book =
    Order_book.finalize book ~final_id:diff.final_id ~exchange_time_us:diff.exchange_time_us
  in
  {
    t with
    book;
    applied = t.applied + 1;
    rejected_levels = t.rejected_levels + List.length rb + List.length ra;
  }

let on_diff t diff =
  match t.mode with
  | Broken _ -> t
  | Buffering -> { t with buffer = diff :: t.buffer }
  | Awaiting_bridge { snapshot_id } ->
      let need = Int64.succ snapshot_id in
      if diff.final_id <= snapshot_id then { t with duplicates = t.duplicates + 1 }
      else if diff.first_id <= need && diff.final_id >= need then
        let t = apply_diff t diff in
        { t with mode = Live { prev_u = diff.final_id } }
      else
        fail t need diff.first_id
          (Printf.sprintf "bridge failed: snapshot=%Ld event U=%Ld u=%Ld" snapshot_id
             diff.first_id diff.final_id)
  | Live { prev_u } ->
      let need = Int64.succ prev_u in
      if diff.final_id <= prev_u then { t with duplicates = t.duplicates + 1 }
      else if diff.first_id = need then
        let t = apply_diff t diff in
        { t with mode = Live { prev_u = diff.final_id } }
      else
        fail t need diff.first_id
          (Printf.sprintf "gap: expected U=%Ld got U=%Ld u=%Ld" need diff.first_id
             diff.final_id)

let load_snapshot t ~last_update_id ~bids ~asks ~exchange_time_us =
  let book = Order_book.empty t.book.inst in
  let book, rb = Order_book.apply_levels book Order_book.Bid bids in
  let book, ra = Order_book.apply_levels book Order_book.Ask asks in
  let book = Order_book.finalize book ~final_id:last_update_id ~exchange_time_us in
  let buffered =
    t.buffer |> List.rev
    |> List.filter (fun d -> d.final_id > last_update_id)
    |> List.sort (fun a b -> Int64.compare a.first_id b.first_id)
  in
  let base =
    {
      t with
      book;
      buffer = [];
      rejected_levels = t.rejected_levels + List.length rb + List.length ra;
      mode = Awaiting_bridge { snapshot_id = last_update_id };
    }
  in
  List.fold_left on_diff base buffered

let is_live = function { mode = Live _; _ } -> true | _ -> false

let mode_name = function
  | Buffering -> "buffering"
  | Awaiting_bridge _ -> "awaiting_bridge"
  | Live _ -> "live"
  | Broken s -> "broken:" ^ s

let mode_to_json = function
  | Buffering -> Jsonx.obj [ ("state", Jsonx.str "buffering") ]
  | Awaiting_bridge { snapshot_id } ->
      Jsonx.obj
        [ ("state", Jsonx.str "awaiting_bridge"); ("snapshot_id", Jsonx.i64 snapshot_id) ]
  | Live { prev_u } ->
      Jsonx.obj [ ("state", Jsonx.str "live"); ("prev_u", Jsonx.i64 prev_u) ]
  | Broken reason ->
      Jsonx.obj [ ("state", Jsonx.str "broken"); ("reason", Jsonx.str reason) ]

let mode_of_json j =
  match Jsonx.string_of (Jsonx.require "state" j) with
  | "buffering" -> Buffering
  | "awaiting_bridge" ->
      Awaiting_bridge { snapshot_id = Jsonx.i64_of (Jsonx.require "snapshot_id" j) }
  | "live" -> Live { prev_u = Jsonx.i64_of (Jsonx.require "prev_u" j) }
  | "broken" -> Broken (Jsonx.string_of (Jsonx.require "reason" j))
  | s -> failwith ("bad sync mode " ^ s)

let to_json t =
  Jsonx.obj
    [
      ("book", Order_book.to_json t.book);
      ("mode", mode_to_json t.mode);
      ("duplicates", Jsonx.int t.duplicates);
      ("applied", Jsonx.int t.applied);
      ("rejected_levels", Jsonx.int t.rejected_levels);
      ( "gaps",
        Jsonx.list
          (List.map
             (fun (a, b, r) ->
               Jsonx.obj
                 [
                   ("expected", Jsonx.i64 a);
                   ("actual", Jsonx.i64 b);
                   ("reason", Jsonx.str r);
                 ])
             (List.rev t.gaps)) );
    ]

let of_json j =
  let book = Order_book.of_json (Jsonx.require "book" j) in
  let mode = mode_of_json (Jsonx.require "mode" j) in
  let gaps =
    List.map
      (fun el ->
        ( Jsonx.i64_of (Jsonx.require "expected" el),
          Jsonx.i64_of (Jsonx.require "actual" el),
          Jsonx.string_of (Jsonx.require "reason" el) ))
      (Jsonx.list_of (Jsonx.require "gaps" j))
  in
  {
    book;
    mode;
    buffer = [];
    duplicates = Int64.to_int (Jsonx.i64_of (Jsonx.require "duplicates" j));
    applied = Int64.to_int (Jsonx.i64_of (Jsonx.require "applied" j));
    rejected_levels = Int64.to_int (Jsonx.i64_of (Jsonx.require "rejected_levels" j));
    gaps = List.rev gaps;
  }
