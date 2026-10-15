(** Reproducible comparison of queue models and latency assumptions on one
    fixed set of order intents. Intent prices are taken from the book at the
    intent's release time. Future mids are used only for markouts. *)

type grid = {
  every_us : int64;
  qty : int64;
  offsets : int64 list;
  sides : [ `Buy | `Sell ] list;
  cancel_after_us : int64;
  max_orders : int;
  start_delay_us : int64;
}

type intent_spec =
  | Grid of grid
  | Explicit of Fill_engine.intent list

type spec = {
  dataset : string;
  seed : int64;
  models : Queue_model.model list;
  submit_grid_us : int64 list;
  md_delay_us : int64;
  decision_delay_us : int64;
  cancel_delay_us : int64;
  fee_ppm : int64;
  horizons_us : int64 list;
  vol_low_lt : int64;
  vol_high_ge : int64;
  bootstrap_samples : int;
  intents : intent_spec;
  output_dir : string;
  trace : bool;
}

let side_of = function
  | "buy" -> `Buy
  | "sell" -> `Sell
  | s -> failwith ("side " ^ s)

let i64s j = List.map Jsonx.i64_of (Jsonx.list_of j)

let load_spec path =
  let j = Jsonx.of_file path in
  let intents = Jsonx.require "intents" j in
  let mode =
    match Jsonx.member "mode" intents with
    | Some (`String "explicit") -> "explicit"
    | _ -> "passive_grid"
  in
  let intent_spec =
    if mode = "explicit" then
      Explicit
        (List.map
           (fun el ->
             {
               Fill_engine.id = Jsonx.i64_of (Jsonx.require "id" el);
               release_us = Jsonx.i64_of (Jsonx.require "release_us" el);
               side = side_of (Jsonx.string_of (Jsonx.require "side" el));
               qty = Jsonx.i64_of (Jsonx.require "qty" el);
               offset_ticks = Jsonx.i64_of (Jsonx.require "offset_ticks" el);
               cancel_after_us = Jsonx.opt Jsonx.i64_of "cancel_after_us" el;
             })
           (Jsonx.list_of (Jsonx.require "orders" intents)))
    else
      Grid
        {
          every_us = Jsonx.i64_of (Jsonx.require "every_us" intents);
          qty = Jsonx.i64_of (Jsonx.require "qty" intents);
          offsets = i64s (Jsonx.require "offsets" intents);
          sides =
            List.map
              (fun s -> side_of (Jsonx.string_of s))
              (Jsonx.list_of (Jsonx.require "sides" intents));
          cancel_after_us = Jsonx.i64_of (Jsonx.require "cancel_after_us" intents);
          max_orders = Int64.to_int (Jsonx.i64_of (Jsonx.require "max_orders" intents));
          start_delay_us = Jsonx.i64_of (Jsonx.require "start_delay_us" intents);
        }
  in
  {
    dataset = Jsonx.string_of (Jsonx.require "dataset" j);
    seed = Jsonx.i64_of (Jsonx.require "seed" j);
    models =
      List.map
        (fun s ->
          match Queue_model.model_of_string (Jsonx.string_of s) with
          | Ok m -> m
          | Error e -> failwith e)
        (Jsonx.list_of (Jsonx.require "queue_models" j));
    submit_grid_us = i64s (Jsonx.require "submit_delay_grid_us" j);
    md_delay_us = Jsonx.i64_of (Jsonx.require "md_delay_us" j);
    decision_delay_us = Jsonx.i64_of (Jsonx.require "decision_delay_us" j);
    cancel_delay_us = Jsonx.i64_of (Jsonx.require "cancel_delay_us" j);
    fee_ppm = Jsonx.i64_of (Jsonx.require "fee_ppm" j);
    horizons_us = i64s (Jsonx.require "markout_horizons_us" j);
    vol_low_lt = Jsonx.i64_of (Jsonx.require "vol_low_lt" j);
    vol_high_ge = Jsonx.i64_of (Jsonx.require "vol_high_ge" j);
    bootstrap_samples = Int64.to_int (Jsonx.i64_of (Jsonx.require "bootstrap_samples" j));
    intents = intent_spec;
    output_dir = Jsonx.string_of (Jsonx.require "output_dir" j);
    trace =
      (match Jsonx.member "trace" j with Some (`Bool b) -> b | _ -> false);
  }

let make_intents events (g : grid) =
  let t0 =
    List.fold_left (fun a e -> Int64.min a (Event.primary_time e)) Int64.max_int events
  in
  let t1 =
    List.fold_left (fun a e -> Int64.max a (Event.primary_time e)) Int64.min_int events
  in
  let rec loop t id acc =
    if t >= t1 || List.length acc >= g.max_orders then List.rev acc
    else
      let rec over_sides id acc = function
        | [] -> (id, acc)
        | side :: rest ->
            let rec over_off id acc = function
              | [] -> (id, acc)
              | off :: orest ->
                  if List.length acc >= g.max_orders then (id, acc)
                  else
                    let intent : Fill_engine.intent =
                      {
                        id;
                        release_us = t;
                        side;
                        qty = g.qty;
                        offset_ticks = off;
                        cancel_after_us = Some g.cancel_after_us;
                      }
                    in
                    over_off (Int64.succ id) (intent :: acc) orest
            in
            let id, acc = over_off id acc g.offsets in
            over_sides id acc rest
      in
      let id, acc = over_sides id acc g.sides in
      loop (Int64.add t g.every_us) id acc
  in
  loop (Int64.add t0 g.start_delay_us) 1L []

let vol_bucket v ~low ~high =
  if v < low then "low" else if v >= high then "high" else "mid"

let spread_bucket s =
  match s with
  | None -> "na"
  | Some s -> if s <= 10L then "tight" else if s >= 30L then "wide" else "normal"

let csv_escape s =
  if String.exists (fun c -> c = ',' || c = '"' || c = '\n') s then
    "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\""
  else s

let csv_line cols = String.concat "," (List.map csv_escape cols)

let opt_i64 = function None -> "" | Some x -> Int64.to_string x

let side_str = function `Buy -> "buy" | `Sell -> "sell"

let weighted_avg fills =
  let num = ref 0.0 in
  let den = ref 0.0 in
  List.iter
    (fun (q, v) ->
      match v with
      | None -> ()
      | Some x ->
          let q = Int64.to_float q in
          num := !num +. (q *. x);
          den := !den +. q)
    fills;
  if !den = 0.0 then None else Some (!num /. !den)

let fill_price_stats (o : Fill_engine.sim_order) =
  let rows =
    List.map
      (fun (f : Fill_engine.fill) ->
        let cap =
          match f.mid_at_fill with
          | None -> None
          | Some m ->
              Some
                (Int64.to_float
                   (match o.order.side with
                   | `Buy -> Int64.sub m f.price
                   | `Sell -> Int64.sub f.price m))
        in
        let is =
          match o.order.decision_mid with
          | None -> None
          | Some d ->
              Some
                (Int64.to_float
                   (match o.order.side with
                   | `Buy -> Int64.sub f.price d
                   | `Sell -> Int64.sub d f.price))
        in
        (f.qty, cap, is, f.price, f.qty))
      o.fills
  in
  let cap = weighted_avg (List.map (fun (q, c, _, _, _) -> (q, c)) rows) in
  let is_ = weighted_avg (List.map (fun (q, _, i, _, _) -> (q, i)) rows) in
  let avg_px =
    let num = List.fold_left (fun a (_, _, _, p, q) -> Int64.add a (Int64.mul p q)) 0L rows in
    let den = List.fold_left (fun a (_, _, _, _, q) -> Int64.add a q) 0L rows in
    if den = 0L then None else Some (Int64.div num den)
  in
  (cap, is_, avg_px)

let run_spec spec =
  let man, events = Manifest.load spec.dataset in
  if not man.synthetic then () else ();
  let quality = Validation.validate spec.dataset in
  if not quality.ok then
    failwith ("dataset failed validation: " ^ String.concat "; " quality.errors);
  let intents =
    match spec.intents with
    | Explicit xs -> xs
    | Grid g -> make_intents events g
  in
  if intents = [] then failwith "no intents in range";
  Event_log.mkdir_p spec.output_dir;
  let exec_path = Filename.concat spec.output_dir "executions.csv" in
  let mark_path = Filename.concat spec.output_dir "markouts.csv" in
  let sens_path = Filename.concat spec.output_dir "sensitivity.csv" in
  let book_path = Filename.concat spec.output_dir "book_series.csv" in
  let qual_path = Filename.concat spec.output_dir "data_quality.json" in
  Out_channel.with_open_text qual_path (fun oc ->
      Out_channel.output_string oc (Jsonx.to_string_pretty (Validation.report_json quality)));
  let replay = Replay.replay man.instrument events in
  Out_channel.with_open_text book_path (fun oc ->
      Out_channel.output_string oc "time_us,mid,bid,ask,bid_qty,ask_qty,spread\n";
      (* book_series is rebuilt inside the cell loop's first replay mids only;
         mids from Replay do not carry touch qty. A second walk records it. *)
      ignore replay;
      let sync = ref (Depth_sync.create man.instrument) in
      let diag = ref Replay.empty_diag in
      List.iter
        (fun ev ->
          let d, s, mid = Replay.apply !diag !sync ev in
          diag := d;
          sync := s;
          match mid with
          | None -> ()
          | Some (t, m) ->
              let bid, bq =
                match Order_book.best_bid s.book with
                | Some (p, q) -> (Int64.to_string p, Int64.to_string q)
                | None -> ("", "")
              in
              let ask, aq =
                match Order_book.best_ask s.book with
                | Some (p, q) -> (Int64.to_string p, Int64.to_string q)
                | None -> ("", "")
              in
              let spr =
                match Order_book.spread s.book with None -> "" | Some s -> Int64.to_string s
              in
              Out_channel.output_string oc
                (csv_line
                   [
                     Int64.to_string t;
                     opt_i64 m;
                     bid;
                     ask;
                     bq;
                     aq;
                     spr;
                   ]
                ^ "\n"))
        (List.sort Event.compare events));
  let exec_oc = Out_channel.open_text exec_path in
  let mark_oc = Out_channel.open_text mark_path in
  let sens_oc = Out_channel.open_text sens_path in
  let trace_oc =
    if spec.trace then
      let oc = Out_channel.open_text (Filename.concat spec.output_dir "trace.csv") in
      Out_channel.output_string oc
        "model,submit_delay_us,order_id,time_us,event_seq,kind,ahead_public,ahead_sim,displayed\n";
      Some oc
    else None
  in
  Out_channel.output_string exec_oc
    "model,submit_delay_us,order_id,side,offset_ticks,price,orig_qty,filled_qty,status,decision_us,arrival_us,first_fill_us,done_us,decision_mid,avg_fill_price,spread_capture,implementation_shortfall,fee_ppm,touch_qty,size_vs_depth_bps,vol_abs,vol_bucket,spread,spread_bucket,n_fills\n";
  Out_channel.output_string mark_oc
    "model,submit_delay_us,order_id,side,horizon_us,signed_markout,vol_bucket\n";
  Out_channel.output_string sens_oc
    "model,submit_delay_us,n_orders,n_accepted,n_filled,n_full,fill_rate,full_fill_rate,fill_rate_ci_lo,fill_rate_ci_hi,mean_filled_qty,mean_spread_capture,mean_implementation_shortfall,mean_markout,cancel_rate,partial_rate\n";
  let cells = ref [] in
  List.iter
    (fun model ->
      List.iter
        (fun submit ->
          let cfg : Fill_engine.config =
            {
              model;
              seed = spec.seed;
              md_delay_us = spec.md_delay_us;
              decision_delay_us = spec.decision_delay_us;
              submit_delay_us = submit;
              cancel_delay_us = spec.cancel_delay_us;
              fee_ppm = spec.fee_ppm;
              horizons_us = spec.horizons_us;
              vol_low_lt = spec.vol_low_lt;
              vol_high_ge = spec.vol_high_ge;
              intents;
              trace = spec.trace;
            }
          in
          let result = Fill_engine.run man.instrument cfg events in
          if result.book_hash <> replay.hash then
            failwith "simulation perturbed the observed book hash";
          let n = List.length result.orders in
          let accepted =
            List.filter
              (fun (o : Fill_engine.sim_order) ->
                match o.order.status with
                | Lifecycle.Rejected | Lifecycle.Created | Lifecycle.Pending_new -> false
                | _ -> true)
              result.orders
          in
          let filled =
            List.filter (fun (o : Fill_engine.sim_order) -> o.order.filled_qty > 0L) result.orders
          in
          let full =
            List.filter
              (fun (o : Fill_engine.sim_order) -> o.order.status = Lifecycle.Filled)
              result.orders
          in
          let canc =
            List.filter
              (fun (o : Fill_engine.sim_order) -> o.order.status = Lifecycle.Cancelled)
              accepted
          in
          let partial =
            List.filter
              (fun (o : Fill_engine.sim_order) ->
                o.order.filled_qty > 0L && o.order.filled_qty < o.order.orig_qty)
              result.orders
          in
          let rate denom num =
            if denom = 0 then 0.0 else float num /. float denom
          in
          let fill_flags =
            accepted
            |> List.map (fun (o : Fill_engine.sim_order) ->
                   if o.order.filled_qty > 0L then 1.0 else 0.0)
            |> Array.of_list
          in
          let rng = Rng.create (Int64.add spec.seed submit) in
          let block = max 1 (int_of_float (sqrt (float (Array.length fill_flags)))) in
          let ci =
            Metrics.block_bootstrap rng ~block ~samples:spec.bootstrap_samples
              ~stat:(fun a ->
                let s = Array.fold_left ( +. ) 0.0 a in
                s /. float (Array.length a))
              fill_flags
          in
          let caps = ref [] in
          let iss = ref [] in
          let marks = ref [] in
          let filled_qtys = ref [] in
          List.iter
            (fun (o : Fill_engine.sim_order) ->
              let cap, is_, avg_px = fill_price_stats o in
              (match cap with Some x -> caps := x :: !caps | None -> ());
              (match is_ with Some x -> iss := x :: !iss | None -> ());
              filled_qtys := Int64.to_float o.order.filled_qty :: !filled_qtys;
              let vb = vol_bucket o.vol_abs ~low:spec.vol_low_lt ~high:spec.vol_high_ge in
              let svb =
                match (o.touch_qty, o.order.orig_qty) with
                | Some t, q when t > 0L -> Int64.to_string (Int64.div (Int64.mul q 10000L) t)
                | _ -> ""
              in
              let off =
                match
                  List.find_opt (fun (i : Fill_engine.intent) -> i.id = o.order.id) intents
                with
                | Some i -> Int64.to_string i.offset_ticks
                | None -> ""
              in
              Out_channel.output_string exec_oc
                (csv_line
                   [
                     Queue_model.model_name model;
                     Int64.to_string submit;
                     Int64.to_string o.order.id;
                     side_str o.order.side;
                     off;
                     Int64.to_string o.order.price;
                     Int64.to_string o.order.orig_qty;
                     Int64.to_string o.order.filled_qty;
                     Lifecycle.status_name o.order.status;
                     Int64.to_string o.order.decision_time_us;
                     opt_i64 o.order.arrival_time_us;
                     opt_i64 o.first_fill_us;
                     opt_i64 o.order.done_time_us;
                     opt_i64 o.order.decision_mid;
                     opt_i64 avg_px;
                     (match cap with None -> "" | Some x -> Printf.sprintf "%.8f" x);
                     (match is_ with None -> "" | Some x -> Printf.sprintf "%.8f" x);
                     Int64.to_string spec.fee_ppm;
                     opt_i64 o.touch_qty;
                     svb;
                     Int64.to_string o.vol_abs;
                     vb;
                     opt_i64 o.spread;
                     spread_bucket o.spread;
                     string_of_int (List.length o.fills);
                   ]
                ^ "\n");
              (match trace_oc with
              | None -> ()
              | Some oc ->
                  List.iter
                    (fun (tr : Fill_engine.trace) ->
                      Out_channel.output_string oc
                        (csv_line
                           [
                             Queue_model.model_name model;
                             Int64.to_string submit;
                             Int64.to_string o.order.id;
                             Int64.to_string tr.time_us;
                             Int64.to_string tr.event_seq;
                             tr.kind;
                             Int64.to_string tr.ahead_public;
                             Int64.to_string tr.ahead_sim;
                             Int64.to_string tr.displayed;
                           ]
                        ^ "\n"))
                    o.trace);
              List.iter
                (fun (m : Fill_engine.markout) ->
                  if m.order_id = o.order.id then (
                    (match m.signed with
                    | Some s -> marks := Int64.to_float s :: !marks
                    | None -> ());
                    Out_channel.output_string mark_oc
                      (csv_line
                         [
                           Queue_model.model_name model;
                           Int64.to_string submit;
                           Int64.to_string m.order_id;
                           side_str o.order.side;
                           Int64.to_string m.horizon_us;
                           opt_i64 m.signed;
                           vb;
                         ]
                      ^ "\n")))
                result.markouts)
            result.orders;
          let mean xs =
            match xs with
            | [] -> ""
            | xs ->
                let s = List.fold_left ( +. ) 0.0 xs in
                Printf.sprintf "%.8f" (s /. float (List.length xs))
          in
          let ci_lo, ci_hi =
            match ci with None -> ("", "") | Some (a, b) -> (Printf.sprintf "%.6f" a, Printf.sprintf "%.6f" b)
          in
          Out_channel.output_string sens_oc
            (csv_line
               [
                 Queue_model.model_name model;
                 Int64.to_string submit;
                 string_of_int n;
                 string_of_int (List.length accepted);
                 string_of_int (List.length filled);
                 string_of_int (List.length full);
                 Printf.sprintf "%.6f" (rate (List.length accepted) (List.length filled));
                 Printf.sprintf "%.6f" (rate (List.length accepted) (List.length full));
                 ci_lo;
                 ci_hi;
                 mean !filled_qtys;
                 mean !caps;
                 mean !iss;
                 mean !marks;
                 Printf.sprintf "%.6f" (rate (List.length accepted) (List.length canc));
                 Printf.sprintf "%.6f" (rate n (List.length partial));
               ]
            ^ "\n");
          cells :=
            Jsonx.obj
              [
                ("model", Jsonx.str (Queue_model.model_name model));
                ("submit_delay_us", Jsonx.i64 submit);
                ("n_orders", Jsonx.int n);
                ("n_accepted", Jsonx.int (List.length accepted));
                ("n_filled", Jsonx.int (List.length filled));
                ("book_hash", Jsonx.str result.book_hash);
                ("warnings", Jsonx.int (List.length result.warnings));
                ("sync_mode", Jsonx.str result.sync_mode);
              ]
            :: !cells)
        spec.submit_grid_us)
    spec.models;
  Out_channel.close exec_oc;
  Out_channel.close mark_oc;
  Out_channel.close sens_oc;
  (match trace_oc with Some oc -> Out_channel.close oc | None -> ());
  let git =
    let ic = Unix.open_process_in "git rev-parse --short HEAD 2>/dev/null" in
    let line =
      match In_channel.input_line ic with Some s -> String.trim s | None -> "unknown"
    in
    ignore (Unix.close_process_in ic);
    if line = "" then "unknown" else line
  in
  let summary =
    Jsonx.obj
      [
        ("schema_version", Jsonx.int 1);
        ("dataset", Jsonx.str spec.dataset);
        ("dataset_sha256", Jsonx.str (List.hd man.files).sha256);
        ("symbol", Jsonx.str man.symbol);
        ("synthetic", Jsonx.bool man.synthetic);
        ("notes", Jsonx.str man.notes);
        ("git", Jsonx.str git);
        ("seed", Jsonx.i64 spec.seed);
        ("fee_ppm", Jsonx.i64 spec.fee_ppm);
        ("fee_note", Jsonx.str "Scenario assumption, not a measured Binance fee tier.");
        ("latency_note", Jsonx.str "Grid values are scenario assumptions, not measured network latency.");
        ( "queue_note",
          Jsonx.str
            "Differences across models are estimates under explicit assumptions, not observed queue positions." );
        ("n_intents", Jsonx.int (List.length intents));
        ("horizons_us", Jsonx.list (List.map Jsonx.i64 spec.horizons_us));
        ("bootstrap_samples", Jsonx.int spec.bootstrap_samples);
        ("bootstrap", Jsonx.str "circular block bootstrap, block=floor(sqrt(n_accepted)), percentile 2.5/97.5");
        ("book_hash", Jsonx.str replay.hash);
        ("data_quality_ok", Jsonx.bool quality.ok);
        ("cells", Jsonx.list (List.rev !cells));
        ("executions_csv", Jsonx.str "executions.csv");
        ("markouts_csv", Jsonx.str "markouts.csv");
        ("sensitivity_csv", Jsonx.str "sensitivity.csv");
        ("book_series_csv", Jsonx.str "book_series.csv");
      ]
  in
  let outp = Filename.concat spec.output_dir "queue_sensitivity.json" in
  Out_channel.with_open_text outp (fun oc ->
      Out_channel.output_string oc (Jsonx.to_string_pretty summary));
  (outp, List.length intents, replay.hash)
