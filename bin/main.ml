let usage =
  {|QueueLab — market replay, queue inference, execution simulation

  dune exec queuelab -- validate --dataset DIR
  dune exec queuelab -- replay --dataset DIR
  dune exec queuelab -- generate-fixture --out DIR [--steps N] [--seed N] [--scenario session|divergence]
  dune exec queuelab -- simulate --config FILE
  dune exec queuelab -- experiment --config FILE
  dune exec queuelab -- benchmark --dataset DIR --synthetic-events N --out FILE
  dune exec queuelab -- normalize --raw DIR --out DIR
  dune exec queuelab -- capture --symbol SYM --duration-seconds N [--out DIR]

Exit codes: 0 ok, 1 data or simulation failure, 2 usage.
Run from the repository root. Capture uses scripts/capture_binance.py
(stdlib TLS) and does not send orders or require an API key.
|}

let die_usage msg =
  prerr_endline msg;
  prerr_endline usage;
  exit 2

let rec flag name = function
  | x :: v :: rest when x = name -> Some v
  | _ :: rest -> flag name rest
  | [] -> None

let flag_int name default argv =
  match flag name argv with None -> default | Some s -> int_of_string s

let flag_i64 name default argv =
  match flag name argv with None -> default | Some s -> Int64.of_string s

let require name argv =
  match flag name argv with Some v -> v | None -> die_usage ("missing " ^ name)

let cmd_validate argv =
  let dir = require "--dataset" argv in
  let r = Queuelab.Validation.validate dir in
  print_endline (Queuelab.Jsonx.to_string_pretty (Queuelab.Validation.report_json r));
  if r.ok then exit 0 else exit 1

let cmd_replay argv =
  let dir = require "--dataset" argv in
  let man, events = Queuelab.Manifest.load dir in
  let r = Queuelab.Replay.replay man.instrument events in
  let bid =
    match Queuelab.Order_book.best_bid r.sync.book with
    | None -> "none"
    | Some (p, q) -> Printf.sprintf "%Ld x %Ld" p q
  in
  let ask =
    match Queuelab.Order_book.best_ask r.sync.book with
    | None -> "none"
    | Some (p, q) -> Printf.sprintf "%Ld x %Ld" p q
  in
  Printf.printf "symbol %s synthetic %b\n" man.symbol man.synthetic;
  Printf.printf "events %d hash %s mode %s\n" r.events_consumed r.hash r.sync_mode;
  Printf.printf "bid %s ask %s spread %s\n" bid ask
    (match Queuelab.Order_book.spread r.sync.book with
    | None -> "na"
    | Some s -> Int64.to_string s);
  Printf.printf "gaps %d duplicates %d crossed %d\n" r.diagnostics.gaps r.diagnostics.duplicates
    r.diagnostics.crossed_observations;
  if r.diagnostics.gaps > 0 then exit 1 else exit 0

let cmd_fixture argv =
  let dir = require "--out" argv in
  match flag "--scenario" argv with
  | Some "divergence" ->
      let man = Queuelab.Synthetic.write_divergence ~dir in
      Printf.printf "wrote %s scenario=divergence sha256=%s synthetic=true\n" dir
        (List.hd man.files).sha256;
      exit 0
  | Some other -> die_usage ("unknown scenario " ^ other)
  | None ->
      let steps = flag_int "--steps" 4000 argv in
      let seed = flag_i64 "--seed" 7L argv in
      let man, events = Queuelab.Synthetic.write_session ~dir ~seed ~steps in
      Printf.printf "wrote %s events=%d sha256=%s synthetic=true\n" dir (List.length events)
        (List.hd man.files).sha256;
      exit 0

let cmd_experiment argv =
  let path = require "--config" argv in
  let spec = Queuelab.Experiment.load_spec path in
  let out, n, hash = Queuelab.Experiment.run_spec spec in
  Printf.printf "wrote %s intents=%d book_hash=%s\n" out n hash;
  exit 0

let cmd_simulate argv = cmd_experiment argv

let cmd_benchmark argv =
  let dataset = require "--dataset" argv in
  let steps = flag_int "--synthetic-events" 50000 argv in
  let out = require "--out" argv in
  let j = Queuelab.Benchmark.run ~dataset ~steps ~out in
  print_endline (Queuelab.Jsonx.to_string j);
  exit 0

let instrument_of_filters path symbol =
  let j = Queuelab.Jsonx.of_file path in
  Queuelab.Instrument.of_json j |> fun inst ->
  if inst.symbol <> symbol && inst.symbol <> "" then inst else { inst with symbol }

let cmd_normalize argv =
  let raw = require "--raw" argv in
  let out = require "--out" argv in
  let symbol =
    match flag "--symbol" argv with Some s -> s | None -> "BTCUSDT"
  in
  let inst =
    let filters = Filename.concat raw "instrument.json" in
    if Sys.file_exists filters then instrument_of_filters filters symbol
    else
      match
        Queuelab.Instrument.make ~symbol
          ~price_scale:(flag_int "--price-scale" 2 argv)
          ~qty_scale:(flag_int "--qty-scale" 5 argv)
          ~tick:(flag_i64 "--tick" 1L argv)
          ~lot:(flag_i64 "--lot" 1L argv)
      with
      | Ok i -> i
      | Error e -> die_usage e
  in
  let lines = Queuelab.Event_log.read_lines (Filename.concat raw "raw.jsonl") in
  let events, errors =
    List.fold_left
      (fun (acc, errs) (i, line) ->
        try
          let j = Queuelab.Jsonx.of_string line in
          let kind = Queuelab.Jsonx.string_of (Queuelab.Jsonx.require "kind" j) in
          let rx = Queuelab.Jsonx.i64_of (Queuelab.Jsonx.require "rx_us" j) in
          let payload = Queuelab.Jsonx.require "payload" j in
          match
            Queuelab.Binance_decoder.decode_raw inst ~seq:(Int64.of_int i) ~synthetic:false
              ~provenance:"binance-spot-public"
              { rx_us = rx; kind; payload }
          with
          | Ok ev -> (ev :: acc, errs)
          | Error e -> (acc, e :: errs)
        with Failure e -> (acc, e :: errs))
      ([], [])
      (List.mapi (fun i l -> (i, l)) lines)
  in
  if errors <> [] then (
    List.iter (fun e -> prerr_endline ("decode: " ^ e)) errors;
    exit 1);
  let events = List.rev events in
  let replay = Queuelab.Replay.replay ~stop_on_gap:false inst events in
  let gaps =
    List.filter_map
      (fun e ->
        match (e : Queuelab.Event.t).kind with
        | Queuelab.Event.Gap g -> Some (g.expected_id, g.actual_id, g.reason)
        | _ -> None)
      events
    @ List.map (fun (a, b, r) -> (a, b, r)) replay.sync.gaps
  in
  let notes =
    "Normalized from a local Binance public capture. Not a complete order-by-order feed. "
    ^ replay.sync_mode
  in
  let _man =
    Queuelab.Manifest.write_dataset ~dir:out ~instrument:inst ~market:"spot" ~synthetic:false
      ~capture_version:"capture-1" ~trade_source:"raw" ~notes ~gaps events
  in
  Printf.printf "normalized %d events mode %s gaps %d\n" (List.length events) replay.sync_mode
    (List.length gaps);
  if gaps <> [] then exit 1 else exit 0

let cmd_capture argv =
  let symbol = require "--symbol" argv in
  let seconds = flag_int "--duration-seconds" 30 argv in
  let out =
    match flag "--out" argv with
    | Some d -> d
    | None -> Filename.concat "data/raw" (Printf.sprintf "%s-%d" symbol seconds)
  in
  if not (Sys.file_exists "scripts/capture_binance.py") then
    die_usage "scripts/capture_binance.py not found; run from the repository root";
  let py_argv =
    [|
      "python3";
      "scripts/capture_binance.py";
      "--symbol";
      symbol;
      "--duration-seconds";
      string_of_int seconds;
      "--out";
      out;
    |]
  in
  let pid = Unix.create_process py_argv.(0) py_argv Unix.stdin Unix.stdout Unix.stderr in
  let code =
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED c -> c
    | _, Unix.WSIGNALED _ | _, Unix.WSTOPPED _ -> 1
  in
  if code <> 0 then exit code;
  let norm = Filename.concat "data/processed" (Filename.basename out) in
  cmd_normalize [ "--raw"; out; "--out"; norm; "--symbol"; symbol ]

let () =
  let argv = List.tl (Array.to_list Sys.argv) in
  match argv with
  | [] | "help" :: _ | "--help" :: _ ->
      print_endline usage;
      exit 0
  | "validate" :: rest -> cmd_validate rest
  | "replay" :: rest -> cmd_replay rest
  | "generate-fixture" :: rest -> cmd_fixture rest
  | "experiment" :: rest -> cmd_experiment rest
  | "simulate" :: rest -> cmd_simulate rest
  | "benchmark" :: rest -> cmd_benchmark rest
  | "normalize" :: rest -> cmd_normalize rest
  | "capture" :: rest -> cmd_capture rest
  | cmd :: _ -> die_usage ("unknown command " ^ cmd)
