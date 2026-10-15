type report = {
  ok : bool;
  errors : string list;
  manifest : Manifest.t;
  replay : Replay.result;
  interarrival_us : (string * int64) list;
  rx_minus_ex_us : (string * int64) list;
}

let percentile_sorted xs p =
  let n = List.length xs in
  if n = 0 then 0L
  else
    let i = int_of_float (p *. float (n - 1)) in
    List.nth xs i

let summarize_deltas xs =
  let xs = List.sort Int64.compare xs in
  [
    ("n", Int64.of_int (List.length xs));
    ("min", percentile_sorted xs 0.0);
    ("p50", percentile_sorted xs 0.5);
    ("p90", percentile_sorted xs 0.9);
    ("max", percentile_sorted xs 1.0);
  ]

let interarrival events =
  let times = events |> List.map Event.primary_time |> List.sort Int64.compare in
  let rec diffs prev acc = function
    | [] -> List.rev acc
    | t :: rest ->
        let d = Int64.sub t prev in
        diffs t (d :: acc) rest
  in
  match times with [] | [ _ ] -> [] | t :: rest -> diffs t [] rest

let clock_deltas events =
  List.filter_map
    (fun e ->
      let ex = Event.exchange_time e in
      if ex = 0L || e.receive_time_us = 0L then None
      else Some (Int64.sub e.receive_time_us ex))
    events

let count_of man k =
  match List.assoc_opt k man.Manifest.counts with Some n -> n | None -> 0

let validate dir =
  let man, events = Manifest.load dir in
  let errors = ref [] in
  let err s = errors := s :: !errors in
  if man.schema_version <> Manifest.schema_version then err "manifest schema mismatch";
  (match man.files with
  | [ f ] when f.name = "events.jsonl" ->
      let path = Filename.concat dir "events.jsonl" in
      let sha = Sha256.hash_file path in
      let bytes = (Unix.stat path).st_size in
      if sha <> f.sha256 then err (Printf.sprintf "checksum mismatch %s vs %s" sha f.sha256);
      if bytes <> f.bytes then err "byte length mismatch"
  | _ -> err "manifest must list events.jsonl");
  let counts = Manifest.count_events events in
  List.iter
    (fun (k, n) ->
      let got = match List.assoc_opt k counts with Some n -> n | None -> 0 in
      if got <> n then err (Printf.sprintf "count %s manifest=%d file=%d" k n got))
    man.counts;
  List.iter
    (fun (k, n) ->
      if count_of man k <> n && not (List.mem_assoc k man.counts) then
        err (Printf.sprintf "undeclared count %s=%d" k n))
    counts;
  let symbols = events |> List.map (fun (e : Event.t) -> e.symbol) |> List.sort_uniq String.compare in
  (match symbols with
  | [] -> err "empty dataset"
  | [ s ] when s = man.symbol -> ()
  | [ s ] -> err ("symbol mismatch " ^ s)
  | _ -> err "mixed symbols");
  List.iter
    (fun (e : Event.t) ->
      let check_levels ls =
        List.iter
          (fun (l : Event.level) ->
            if l.sz < 0L then err "negative level";
            (match Instrument.check_price man.instrument l.px with
            | Error msg -> err msg
            | Ok () -> ());
            match Instrument.check_qty man.instrument l.sz with
            | Error msg -> err msg
            | Ok () -> ())
          ls
      in
      match e.kind with
      | Event.Snapshot s ->
          check_levels s.bids;
          check_levels s.asks
      | Event.Depth d ->
          check_levels d.bids;
          check_levels d.asks;
          if d.first_id > d.final_id then err "depth U > u"
      | Event.Trade t ->
          if t.qty < 0L then err "negative trade"
      | Event.Agg_trade a -> if a.qty < 0L then err "negative agg trade"
      | Event.Gap _ -> ())
    events;
  let replay = Replay.replay ~stop_on_gap:false man.instrument events in
  if replay.diagnostics.gaps > 0 || man.gaps <> [] then
    err
      (Printf.sprintf "integrity gaps: replay=%d manifest=%d mode=%s" replay.diagnostics.gaps
         (List.length man.gaps) replay.sync_mode);
  (match replay.sync.mode with
  | Depth_sync.Live _ -> ()
  | Depth_sync.Awaiting_bridge _ ->
      if count_of man "depth" > 0 then err "depth never bridged to snapshot"
  | Depth_sync.Buffering -> if count_of man "snapshot" > 0 then err "unexpected buffering"
  | Depth_sync.Broken s -> err s);
  if Order_book.invariant_violations replay.sync.book <> [] then err "book invariant violated";
  {
    ok = !errors = [];
    errors = List.rev !errors;
    manifest = man;
    replay;
    interarrival_us = summarize_deltas (interarrival events);
    rx_minus_ex_us = summarize_deltas (clock_deltas events);
  }

let report_json (r : report) =
  Jsonx.obj
    [
      ("ok", Jsonx.bool r.ok);
      ("errors", Jsonx.list (List.map Jsonx.str r.errors));
      ("symbol", Jsonx.str r.manifest.symbol);
      ("synthetic", Jsonx.bool r.manifest.synthetic);
      ("notes", Jsonx.str r.manifest.notes);
      ("sync_mode", Jsonx.str r.replay.sync_mode);
      ("book_hash", Jsonx.str r.replay.hash);
      ("counts", Jsonx.obj (List.map (fun (k, n) -> (k, Jsonx.int n)) r.manifest.counts));
      ("interarrival_us", Jsonx.obj (List.map (fun (k, v) -> (k, Jsonx.i64 v)) r.interarrival_us));
      ("rx_minus_ex_us", Jsonx.obj (List.map (fun (k, v) -> (k, Jsonx.i64 v)) r.rx_minus_ex_us));
      ("crossed_observations", Jsonx.int r.replay.diagnostics.crossed_observations);
      ("duplicates", Jsonx.int r.replay.diagnostics.duplicates);
      ("gaps", Jsonx.int r.replay.diagnostics.gaps);
      ("events_consumed", Jsonx.int r.replay.events_consumed);
      ( "limitations",
        Jsonx.list
          [
            Jsonx.str
              "Public Binance depth is market-by-price. Queue position is not observed.";
            Jsonx.str "rx_minus_ex mixes two clocks and is not a latency measurement.";
          ] );
    ]
