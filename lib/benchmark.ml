(** Timings of this build, on this machine. Numbers are not portable. *)

let read_first_match path f =
  try
    let ic = In_channel.open_text path in
    Fun.protect ~finally:(fun () -> In_channel.close ic) @@ fun () ->
    let rec loop () =
      match In_channel.input_line ic with
      | None -> None
      | Some line -> ( match f line with Some x -> Some x | None -> loop ())
    in
    loop ()
  with Sys_error _ -> None

let cpu_model () =
  match
    read_first_match "/proc/cpuinfo" (fun line ->
        if String.length line > 10 && String.sub line 0 10 = "model name" then
          Some (String.trim (List.nth (String.split_on_char ':' line) 1))
        else None)
  with
  | Some s -> s
  | None -> "unknown"

let mem_total_kb () =
  match
    read_first_match "/proc/meminfo" (fun line ->
        if String.length line > 9 && String.sub line 0 9 = "MemTotal:" then Some line else None)
  with
  | Some s -> s
  | None -> "unknown"

let uname () =
  let ic = Unix.open_process_in "uname -sr" in
  let s = match In_channel.input_line ic with Some x -> x | None -> "unknown" in
  ignore (Unix.close_process_in ic);
  s

let percentile xs p =
  let a = Array.of_list xs in
  Array.sort Float.compare a;
  a.(int_of_float (p *. float (Array.length a - 1)))

let repeat n thunk =
  ignore (thunk ());
  List.init n (fun _ ->
      let t0 = Unix.gettimeofday () in
      ignore (thunk ());
      Unix.gettimeofday () -. t0)

let stats xs =
  Jsonx.obj
    [
      ("repeats", Jsonx.int (List.length xs));
      ("min_s", Jsonx.float (percentile xs 0.0));
      ("median_s", Jsonx.float (percentile xs 0.5));
      ("max_s", Jsonx.float (percentile xs 1.0));
    ]

let run ~dataset ~steps ~out =
  let man, events = Manifest.load dataset in
  let n = List.length events in
  let replay_s = repeat 5 (fun () -> ignore (Replay.replay man.instrument events)) in
  let inst, big = Synthetic.session ~seed:99L ~steps in
  let big_n = List.length big in
  let big_s = repeat 3 (fun () -> ignore (Replay.replay inst big)) in
  let decode_s =
    let line = Jsonx.to_string (Event.to_json (List.hd events)) in
    repeat 3 (fun () ->
        for _ = 1 to 20000 do
          ignore (Event.of_json (Jsonx.of_string line))
        done)
  in
  let ckpt_s =
    let result = Replay.replay inst big in
    repeat 3 (fun () -> ignore (Checkpoint.roundtrip result.sync))
  in
  let one_intent : Fill_engine.intent =
    {
      id = 1L;
      release_us = 5_000L;
      side = `Buy;
      qty = 10L;
      offset_ticks = 0L;
      cancel_after_us = Some 50_000L;
    }
  in
  let cfg : Fill_engine.config =
    {
      model = Queue_model.Probabilistic;
      seed = 1L;
      md_delay_us = 0L;
      decision_delay_us = 0L;
      submit_delay_us = 0L;
      cancel_delay_us = 0L;
      fee_ppm = 100L;
      horizons_us = [ 1000L ];
      vol_low_lt = 1L;
      vol_high_ge = 5L;
      intents = [ one_intent ];
      trace = false;
    }
  in
  let sim_s = repeat 3 (fun () -> ignore (Fill_engine.run inst cfg big)) in
  let ocaml_version =
    let ic = Unix.open_process_in "ocamlc -version" in
    let s = match In_channel.input_line ic with Some x -> x | None -> "unknown" in
    ignore (Unix.close_process_in ic);
    s
  in
  let j =
    Jsonx.obj
      [
        ("schema_version", Jsonx.int 1);
        ("note", Jsonx.str "Measured on the machine that ran queuelab benchmark. Do not cite as a hardware-independent throughput.");
        ("uname", Jsonx.str (uname ()));
        ("cpu", Jsonx.str (cpu_model ()));
        ("meminfo", Jsonx.str (mem_total_kb ()));
        ("ocaml", Jsonx.str ocaml_version);
        ("profile", Jsonx.str (
           match Sys.getenv_opt "QUEUELAB_PROFILE" with
           | Some s -> s
           | None -> "unspecified"));
        ("dataset", Jsonx.str dataset);
        ("dataset_events", Jsonx.int n);
        ("dataset_synthetic", Jsonx.bool man.synthetic);
        ("dataset_replay", stats replay_s);
        ("synthetic_steps", Jsonx.int steps);
        ("synthetic_events", Jsonx.int big_n);
        ("synthetic_replay", stats big_s);
        ("json_roundtrip_20k", stats decode_s);
        ("checkpoint_roundtrip_synthetic", stats ckpt_s);
        ("queue_sim_one_order_synthetic", stats sim_s);
        ( "events_per_second_synthetic_median",
          Jsonx.float
            (float big_n
            /. max 1e-9 (percentile big_s 0.5)) );
      ]
  in
  Event_log.mkdir_p (Filename.dirname out);
  Out_channel.with_open_text out (fun oc -> Out_channel.output_string oc (Jsonx.to_string_pretty j));
  j
