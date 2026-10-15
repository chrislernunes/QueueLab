(** Deterministic replay of canonical events.

    Checkpoint state is the depth-sync state (book + sequence cursor), not the
    book alone. Restoring that state and replaying the suffix matches a
    full replay, including the final hash. *)

type diagnostics = {
  snapshots : int;
  depths : int;
  trades : int;
  agg_trades : int;
  gaps : int;
  duplicates : int;
  crossed_observations : int;
  rejected_levels : int;
  warnings : string list;
}

let empty_diag =
  {
    snapshots = 0;
    depths = 0;
    trades = 0;
    agg_trades = 0;
    gaps = 0;
    duplicates = 0;
    crossed_observations = 0;
    rejected_levels = 0;
    warnings = [];
  }

type result = {
  sync : Depth_sync.t;
  hash : string;
  diagnostics : diagnostics;
  mids : (int64 * int64 option) list;
  sync_mode : string;
  events_consumed : int;
}

let bump_crossed diag book =
  if Order_book.is_crossed book then
    { diag with crossed_observations = diag.crossed_observations + 1 }
  else diag

let apply diag sync ev =
  let time = Event.primary_time ev in
  match ev.kind with
  | Event.Snapshot s ->
      let sync =
        Depth_sync.load_snapshot sync ~last_update_id:s.last_update_id
          ~bids:(List.map (fun (l : Event.level) -> (l.px, l.sz)) s.bids)
          ~asks:(List.map (fun (l : Event.level) -> (l.px, l.sz)) s.asks)
          ~exchange_time_us:s.exchange_time_us
      in
      let diag = bump_crossed { diag with snapshots = diag.snapshots + 1 } sync.book in
      let diag =
        match sync.mode with
        | Depth_sync.Broken reason ->
            { diag with gaps = diag.gaps + 1; warnings = reason :: diag.warnings }
        | _ -> diag
      in
      (diag, sync, Some (time, Order_book.mid sync.book))
  | Event.Depth d ->
      let before_dup = sync.duplicates in
      let sync =
        Depth_sync.on_diff sync
          {
            first_id = d.first_id;
            final_id = d.final_id;
            bids = List.map (fun (l : Event.level) -> (l.px, l.sz)) d.bids;
            asks = List.map (fun (l : Event.level) -> (l.px, l.sz)) d.asks;
            exchange_time_us = d.exchange_time_us;
          }
      in
      let dup = sync.duplicates > before_dup in
      let diag =
        bump_crossed
          {
            diag with
            depths = diag.depths + if dup then 0 else 1;
            duplicates = sync.duplicates;
            rejected_levels = sync.rejected_levels;
          }
          sync.book
      in
      let diag =
        match sync.mode with
        | Depth_sync.Broken reason ->
            { diag with gaps = diag.gaps + 1; warnings = reason :: diag.warnings }
        | _ -> diag
      in
      (diag, sync, Some (time, Order_book.mid sync.book))
  | Event.Trade _ -> ({ diag with trades = diag.trades + 1 }, sync, None)
  | Event.Agg_trade _ -> ({ diag with agg_trades = diag.agg_trades + 1 }, sync, None)
  | Event.Gap g ->
      ( {
          diag with
          gaps = diag.gaps + 1;
          warnings = g.reason :: diag.warnings;
        },
        sync,
        None )

let run_list ?(stop_on_gap = true) sync events =
  let events = List.sort Event.compare events in
  let rec loop diag sync mids consumed = function
    | [] -> (diag, sync, List.rev mids, consumed, [])
    | ev :: rest ->
        let diag, sync, mid = apply diag sync ev in
        let mids = match mid with None -> mids | Some m -> m :: mids in
        let broken =
          match sync.mode with Depth_sync.Broken _ -> true | _ -> false
        in
        let gap_event = match ev.kind with Event.Gap _ -> true | _ -> false in
        if stop_on_gap && (broken || gap_event) then
          (diag, sync, List.rev mids, consumed + 1, rest)
        else loop diag sync mids (consumed + 1) rest
  in
  let diag, sync, mids, consumed, rest =
    loop empty_diag sync [] 0 events
  in
  ( {
      sync;
      hash = Order_book.hash sync.book;
      diagnostics = { diag with warnings = List.rev diag.warnings };
      mids;
      sync_mode = Depth_sync.mode_name sync.mode;
      events_consumed = consumed;
    },
    rest )

let replay ?stop_on_gap inst events =
  let result, _rest = run_list ?stop_on_gap (Depth_sync.create inst) events in
  result

let replay_prefix inst events n =
  let events = List.sort Event.compare events in
  let rec split k acc xs =
    if k <= 0 then (List.rev acc, xs)
    else match xs with [] -> (List.rev acc, []) | x :: tl -> split (k - 1) (x :: acc) tl
  in
  let head, tail = split n [] events in
  let result, _ = run_list ~stop_on_gap:false (Depth_sync.create inst) head in
  (result, tail)

let resume sync events =
  (* [events] must already be the sorted suffix. Do not re-sort against a
     cursor: the caller passes the exact remaining list. *)
  let rec loop diag sync mids consumed = function
    | [] -> (diag, sync, List.rev mids, consumed)
    | ev :: rest ->
        let diag, sync, mid = apply diag sync ev in
        let mids = match mid with None -> mids | Some m -> m :: mids in
        loop diag sync mids (consumed + 1) rest
  in
  let diag, sync, mids, consumed = loop empty_diag sync [] 0 events in
  {
    sync;
    hash = Order_book.hash sync.book;
    diagnostics = { diag with warnings = List.rev diag.warnings };
    mids;
    sync_mode = Depth_sync.mode_name sync.mode;
    events_consumed = consumed;
  }
