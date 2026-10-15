type file = { name : string; sha256 : string; bytes : int }

type t = {
  schema_version : int;
  symbol : string;
  market : string;
  synthetic : bool;
  instrument : Instrument.t;
  capture_version : string;
  time_start_us : int64;
  time_end_us : int64;
  counts : (string * int) list;
  gaps : (int64 * int64 * string) list;
  files : file list;
  notes : string;
  trade_source : string;
}

let schema_version = 1

let count_events events =
  let bump k acc =
    let n = match List.assoc_opt k acc with Some n -> n | None -> 0 in
    (k, n + 1) :: List.remove_assoc k acc
  in
  List.fold_left (fun acc (e : Event.t) -> bump (Event.kind_name e.kind) acc) [] events
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let time_range events =
  match events with
  | [] -> (0L, 0L)
  | xs ->
      let times = List.map Event.primary_time xs in
      let mn = List.fold_left Int64.min Int64.max_int times in
      let mx = List.fold_left Int64.max Int64.min_int times in
      (mn, mx)

let to_json t =
  Jsonx.obj
    [
      ("schema_version", Jsonx.int t.schema_version);
      ("symbol", Jsonx.str t.symbol);
      ("market", Jsonx.str t.market);
      ("synthetic", Jsonx.bool t.synthetic);
      ("instrument", Instrument.to_json t.instrument);
      ("capture_version", Jsonx.str t.capture_version);
      ("time_start_us", Jsonx.i64 t.time_start_us);
      ("time_end_us", Jsonx.i64 t.time_end_us);
      ("counts", Jsonx.obj (List.map (fun (k, n) -> (k, Jsonx.int n)) t.counts));
      ( "gaps",
        Jsonx.list
          (List.map
             (fun (a, b, r) ->
               Jsonx.obj
                 [ ("expected", Jsonx.i64 a); ("actual", Jsonx.i64 b); ("reason", Jsonx.str r) ])
             t.gaps) );
      ( "files",
        Jsonx.list
          (List.map
             (fun f ->
               Jsonx.obj
                 [
                   ("name", Jsonx.str f.name);
                   ("sha256", Jsonx.str f.sha256);
                   ("bytes", Jsonx.int f.bytes);
                 ])
             t.files) );
      ("notes", Jsonx.str t.notes);
      ("trade_source", Jsonx.str t.trade_source);
    ]

let of_json j =
  let files =
    List.map
      (fun el ->
        {
          name = Jsonx.string_of (Jsonx.require "name" el);
          sha256 = Jsonx.string_of (Jsonx.require "sha256" el);
          bytes = Int64.to_int (Jsonx.i64_of (Jsonx.require "bytes" el));
        })
      (Jsonx.list_of (Jsonx.require "files" j))
  in
  let counts =
    match Jsonx.require "counts" j with
    | `Assoc xs -> List.map (fun (k, v) -> (k, Int64.to_int (Jsonx.i64_of v))) xs
    | _ -> failwith "counts"
  in
  let gaps =
    List.map
      (fun el ->
        ( Jsonx.i64_of (Jsonx.require "expected" el),
          Jsonx.i64_of (Jsonx.require "actual" el),
          Jsonx.string_of (Jsonx.require "reason" el) ))
      (Jsonx.list_of (Jsonx.require "gaps" j))
  in
  {
    schema_version = Int64.to_int (Jsonx.i64_of (Jsonx.require "schema_version" j));
    symbol = Jsonx.string_of (Jsonx.require "symbol" j);
    market = Jsonx.string_of (Jsonx.require "market" j);
    synthetic = Jsonx.bool_of (Jsonx.require "synthetic" j);
    instrument = Instrument.of_json (Jsonx.require "instrument" j);
    capture_version = Jsonx.string_of (Jsonx.require "capture_version" j);
    time_start_us = Jsonx.i64_of (Jsonx.require "time_start_us" j);
    time_end_us = Jsonx.i64_of (Jsonx.require "time_end_us" j);
    counts;
    gaps;
    files;
    notes = Jsonx.string_of (Jsonx.require "notes" j);
    trade_source =
      (match Jsonx.member "trade_source" j with
      | Some v -> Jsonx.string_of v
      | None -> "raw");
  }

let write_dataset ~dir ~(instrument : Instrument.t) ~market ~synthetic ~capture_version ~notes ~trade_source
    ~gaps events =
  Event_log.mkdir_p dir;
  let path = Filename.concat dir "events.jsonl" in
  Event_log.write_events path events;
  let bytes = In_channel.with_open_bin path In_channel.input_all |> String.length in
  let sha = Sha256.hash_file path in
  let t0, t1 = time_range events in
  let symbol =
    match events with
    | e :: _ -> e.symbol
    | [] -> (instrument : Instrument.t).symbol
  in
  let man =
    {
      schema_version;
      symbol;
      market;
      synthetic;
      instrument;
      capture_version;
      time_start_us = t0;
      time_end_us = t1;
      counts = count_events events;
      gaps;
      files = [ { name = "events.jsonl"; sha256 = sha; bytes } ];
      notes;
      trade_source;
    }
  in
  let mp = Filename.concat dir "manifest.json" in
  Out_channel.with_open_text mp (fun oc ->
      Out_channel.output_string oc (Jsonx.to_string_pretty (to_json man)));
  man

let load dir =
  let man = of_json (Jsonx.of_file (Filename.concat dir "manifest.json")) in
  let events = Event_log.read_events (Filename.concat dir "events.jsonl") in
  (man, events)
