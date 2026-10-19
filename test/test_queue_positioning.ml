open Queue_positioning

let failed = ref 0

let check name cond =
  if cond then Printf.printf "ok  %s\n%!" name
  else (
    Printf.printf "FAIL %s\n%!" name;
    incr failed)

let check_eq name a b =
  if a = b then Printf.printf "ok  %s\n%!" name
  else (
    Printf.printf "FAIL %s\n   left=%s\n  right=%s\n%!" name a b;
    incr failed)

let ev ~seq ~time kind =
  {
    Event.schema = Event.schema_version;
    symbol = "SYN";
    kind;
    receive_time_us = time;
    seq_index = seq;
    synthetic = true;
    provenance = "unit";
  }

let lvl p q = { Event.px = p; sz = q }

let snap ~seq ~time ~id bids asks =
  ev ~seq ~time
    (Event.Snapshot
       { last_update_id = id; bids; asks; exchange_time_us = time })

let depth ~seq ~time ~u ~uu bids asks =
  ev ~seq ~time
    (Event.Depth
       { first_id = u; final_id = uu; bids; asks; exchange_time_us = time })

let trade ~seq ~time ~id ~price ~qty ~maker =
  ev ~seq ~time
    (Event.Trade
       {
         trade_id = id;
         price;
         qty;
         buyer_is_maker = maker;
         exchange_time_us = time;
       })

let divergence_events () =
  [
    snap ~seq:0L ~time:1000L ~id:100L [ lvl 100L 10L; lvl 99L 20L ] [ lvl 101L 8L ];
    depth ~seq:1L ~time:2000L ~u:101L ~uu:101L [ lvl 100L 15L ] [];
    depth ~seq:2L ~time:3000L ~u:102L ~uu:102L [ lvl 100L 9L ] [];
    trade ~seq:3L ~time:4000L ~id:7L ~price:100L ~qty:5L ~maker:true;
    depth ~seq:4L ~time:4000L ~u:103L ~uu:103L [ lvl 100L 4L ] [];
  ]

let nocount_events () =
  [
    snap ~seq:0L ~time:1000L ~id:100L [ lvl 100L 10L ] [ lvl 101L 8L ];
    trade ~seq:1L ~time:2000L ~id:1L ~price:100L ~qty:6L ~maker:true;
    depth ~seq:2L ~time:2000L ~u:101L ~uu:101L [ lvl 100L 4L ] [];
    trade ~seq:3L ~time:3000L ~id:2L ~price:100L ~qty:4L ~maker:true;
    depth ~seq:4L ~time:3000L ~u:102L ~uu:102L [ lvl 100L 0L ] [];
  ]

let intent qty =
  {
    Fill_engine.id = 1L;
    release_us = 1500L;
    side = `Buy;
    qty;
    offset_ticks = 0L;
    cancel_after_us = None;
  }

let cfg model events_ignored intents =
  ignore events_ignored;
  {
    Fill_engine.model;
    seed = 42L;
    md_delay_us = 0L;
    decision_delay_us = 0L;
    submit_delay_us = 0L;
    cancel_delay_us = 0L;
    fee_ppm = 0L;
    horizons_us = [ 1000L ];
    vol_low_lt = 1L;
    vol_high_ge = 5L;
    intents;
    trace = true;
  }

let filled_of r =
  match r.Fill_engine.orders with
  | [ o ] -> o.order.filled_qty
  | _ -> -1L

let () =
  check "sha256 abc"
    (Sha256.hash "abc"
    = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
  check "sha256 empty"
    (Sha256.hash ""
    = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
  (match Decimal.parse_fixed "10.5" ~scale:2 with
  | Ok 1050L -> check "decimal 10.5" true
  | Ok n -> check_eq "decimal 10.5" (Int64.to_string n) "1050"
  | Error e -> check_eq "decimal 10.5" e "1050");
  check "decimal extra frac" (Decimal.parse_fixed "1.234" ~scale:2 = Error "extra fractional digits");
  check "decimal pad" (Decimal.parse_fixed "10" ~scale:2 = Ok 1000L);
  check "off tick" (Instrument.check_price Instrument.syn 1L = Ok ());
  (match Instrument.parse_price { Instrument.syn with tick = 2L } "3" with
  | Error _ -> check "off tick parse" true
  | Ok _ -> check "off tick parse" false);
  let book = Order_book.empty Instrument.syn in
  let book =
    match Order_book.apply_level book Order_book.Bid 10L 5L with Ok b -> b | Error _ -> book
  in
  let book =
    match Order_book.apply_level book Order_book.Ask 12L 4L with Ok b -> b | Error _ -> book
  in
  check "best bid" (Order_book.best_bid book = Some (10L, 5L));
  check "spread" (Order_book.spread book = Some 2L);
  check "mid" (Order_book.mid book = Some 11L);
  check "micro" (Order_book.microprice book = Some (Int64.div (Int64.add (Int64.mul 12L 5L) (Int64.mul 10L 4L)) 9L));
  check "neg qty rejected"
    (match Order_book.apply_level book Order_book.Bid 10L (-1L) with Error `Negative -> true | _ -> false);
  let events = divergence_events () in
  let r1 = Replay.replay Instrument.syn events in
  let r2 = Replay.replay Instrument.syn events in
  check_eq "replay deterministic" r1.hash r2.hash;
  check "replay live" (r1.sync_mode = "live");
  check "bid after divergence" (Order_book.qty_at r1.sync.book Bid 100L = 4L);
  let prefix, tail = Replay.replay_prefix Instrument.syn events 3 in
  let restored = Checkpoint.roundtrip prefix.sync in
  check_eq "checkpoint book" (Order_book.hash restored.book) prefix.hash;
  let resumed = Replay.resume restored tail in
  check_eq "checkpoint resume" resumed.hash r1.hash;
  let round = events |> List.map Event.to_json |> List.map Event.of_json in
  check_eq "event json" (Replay.replay Instrument.syn round).hash r1.hash;
  let dup =
    events
    @ [
        depth ~seq:9L ~time:5000L ~u:103L ~uu:103L [ lvl 100L 4L ] [];
      ]
  in
  let rd = Replay.replay Instrument.syn dup in
  check "duplicate counted" (rd.diagnostics.duplicates >= 1);
  check_eq "duplicate hash" rd.hash r1.hash;
  let gap =
    [
      snap ~seq:0L ~time:1L ~id:10L [ lvl 5L 1L ] [ lvl 6L 1L ];
      depth ~seq:1L ~time:2L ~u:11L ~uu:11L [] [];
      depth ~seq:2L ~time:3L ~u:13L ~uu:13L [] [];
    ]
  in
  let rg = Replay.replay ~stop_on_gap:false Instrument.syn gap in
  check "gap detected" (rg.diagnostics.gaps >= 1);
  let div = divergence_events () in
  let c_cons = cfg Queue_model.Conservative () [ intent 4L ] in
  let c_opt = { c_cons with model = Queue_model.Optimistic } in
  let a = Fill_engine.run Instrument.syn c_cons div in
  let b = Fill_engine.run Instrument.syn c_opt div in
  check "conservative fill" (filled_of a = 0L);
  check "optimistic fill" (filled_of b = 1L);
  let trace_kinds =
    match b.orders with
    | o :: _ -> List.map (fun (t : Fill_engine.trace) -> t.kind) o.trace
    | [] -> []
  in
  check "fill is traced" (List.mem "trade_at_price" trace_kinds);
  check "opt >= cons" (filled_of b >= filled_of a);
  check_eq "sim does not touch book" a.book_hash r1.hash;
  let p1 = Fill_engine.run Instrument.syn { c_cons with model = Probabilistic } div in
  let p2 = Fill_engine.run Instrument.syn { c_cons with model = Probabilistic } div in
  check "prob seed" (filled_of p1 = filled_of p2);
  let nc = nocount_events () in
  let nc_r = Fill_engine.run Instrument.syn c_cons nc in
  check "no double count" (filled_of nc_r = 0L);
  let through =
    nc
    @ [
        trade ~seq:5L ~time:4000L ~id:3L ~price:99L ~qty:1L ~maker:true;
        depth ~seq:6L ~time:4000L ~u:103L ~uu:103L [ lvl 99L 19L ] [];
      ]
  in
  (* order qty 4 was unfilled; a trade through 100 should fill the residual *)
  let th = Fill_engine.run Instrument.syn c_cons through in
  check "trade through fills" (filled_of th = 4L);
  let late_cancel =
    {
      (intent 4L) with
      cancel_after_us = Some 1000L;
    }
  in
  (* release 1500, cancel effective 2500, trade at 4000 is after cancel *)
  let cc =
    Fill_engine.run Instrument.syn
      { c_cons with cancel_delay_us = 0L; intents = [ late_cancel ] }
      div
  in
  check "cancel before fill"
    (match cc.orders with
    | [ o ] -> o.order.status = Lifecycle.Cancelled && o.order.filled_qty = 0L
    | _ -> false);
  let race_events =
    [
      snap ~seq:0L ~time:1000L ~id:100L [ lvl 100L 10L ] [ lvl 101L 1L ];
      trade ~seq:1L ~time:1800L ~id:1L ~price:100L ~qty:10L ~maker:true;
      depth ~seq:2L ~time:1800L ~u:101L ~uu:101L [ lvl 100L 0L ] [];
    ]
  in
  let racing =
    Fill_engine.run Instrument.syn
      {
        c_cons with
        cancel_delay_us = 1000L;
        intents = [ { (intent 4L) with cancel_after_us = Some 100L } ];
      }
      race_events
  in
  (* release 1500, cancel request conceptually 1600, effective 2600.
     Trade at 1800 is before cancel effective. ahead=10, trade 10, fill 0
     because we joined behind 10. Not a fill. Use a smaller queue. *)
  ignore racing;
  let race2 =
    [
      snap ~seq:0L ~time:1000L ~id:100L [ lvl 100L 1L ] [ lvl 102L 1L ];
      trade ~seq:1L ~time:1800L ~id:1L ~price:100L ~qty:4L ~maker:true;
      depth ~seq:2L ~time:1800L ~u:101L ~uu:101L [ lvl 100L 0L ] [];
    ]
  in
  let rz =
    Fill_engine.run Instrument.syn
      {
        c_cons with
        cancel_delay_us = 1000L;
        intents = [ { (intent 4L) with cancel_after_us = Some 2000L } ];
      }
      race2
  in
  check "fill stands through later cancel"
    (match rz.orders with
    | [ o ] -> o.order.filled_qty = 3L && o.order.status = Lifecycle.Cancelled
    | _ -> false);
  let o = { Lifecycle.id = 1L; side = `Buy; price = 1L; orig_qty = 5L; filled_qty = 0L;
            status = Lifecycle.Created; decision_time_us = 0L; arrival_time_us = None; done_time_us = None;
            decision_mid = None; decision_spread = None; decision_touch_qty = None; reject = None } in
  let o = Result.get_ok (Lifecycle.apply o Lifecycle.Send) in
  let o = Result.get_ok (Lifecycle.apply o Lifecycle.Accept) in
  check "overfill rejected"
    (match Lifecycle.apply o (Lifecycle.Partial 6L) with Error _ -> true | Ok _ -> false);
  let o = Result.get_ok (Lifecycle.apply o (Lifecycle.Partial 5L)) in
  check "full via partial" (o.status = Lifecycle.Filled);
  check "no fill after terminal" (match Lifecycle.apply o (Lifecycle.Partial 1L) with Error _ -> true | Ok _ -> false);
  let led =
    Ledger.empty
    |> Ledger.fill ~side:`Buy ~price:10L ~qty:2L ~fee:1L
    |> Ledger.fill ~side:`Sell ~price:12L ~qty:2L ~fee:1L
  in
  check "ledger flat" (led.pos = 0L);
  check "ledger identity" (Ledger.identity_ok led ~mark:12L);
  check "ledger pnl" (Ledger.equity led ~mark:0L = 2L);
  let flip =
    Ledger.empty
    |> Ledger.fill ~side:`Buy ~price:10L ~qty:2L ~fee:0L
    |> Ledger.fill ~side:`Sell ~price:12L ~qty:3L ~fee:3L
  in
  check "flip identity" (Ledger.identity_ok flip ~mark:12L);
  let fee = Fees.fee ~notional:1_000_000L ~fee_ppm:100L in
  check "fee 1bp" (fee = 100L);
  let rng = Rng.create 7L in
  let a = Rng.binomial rng ~n:20L ~p:0.5 ~exact_limit:4096L in
  let rng2 = Rng.create 7L in
  let b = Rng.binomial rng2 ~n:20L ~p:0.5 ~exact_limit:4096L in
  check "binomial seed" (a = b);
  let inst, evs = Synthetic.session ~seed:7L ~steps:50 in
  let inst2, evs2 = Synthetic.session ~seed:7L ~steps:50 in
  check "generator stable"
    (List.map Event.to_json evs = List.map Event.to_json evs2);
  let rep = Replay.replay inst evs in
  check "synthetic live" (rep.sync_mode = "live");
  check "synthetic invariants" (Invariants.violations rep.sync.book = []);
  let tmp = Filename.concat (Filename.get_temp_dir_name ()) "queue_positioning-unit" in
  let _ = Sys.command (Printf.sprintf "rm -rf %s" tmp) in
  let man, _ = Synthetic.write_session ~dir:tmp ~seed:3L ~steps:30 in
  let v = Validation.validate tmp in
  check "validate ok" v.ok;
  let bad = Manifest.to_json { man with files = [ { (List.hd man.files) with sha256 = "dead" } ] } in
  Out_channel.with_open_text (Filename.concat tmp "manifest.json") (fun oc ->
      Out_channel.output_string oc (Jsonx.to_string bad));
  let vbad = Validation.validate tmp in
  check "checksum fails" (not vbad.ok);
  let raw =
    {|{"e":"depthUpdate","E":1672515782136,"s":"SYN","U":11,"u":11,"b":[["10","2"]],"a":[["11","0"]]}|}
  in
  (match
     Binance_decoder.decode_one Instrument.syn ~seq:1L ~rx_us:5L ~synthetic:true
       ~provenance:"fixture" (Jsonx.of_string raw)
   with
  | Ok e ->
      check "decode depth" (
        match e.kind with
        | Event.Depth d -> d.first_id = 11L && d.bids = [ lvl 10L 2L ] && d.exchange_time_us = 1672515782136000L
        | _ -> false)
  | Error e -> check_eq "decode depth" e "ok");
  let trade_raw =
    {|{"e":"trade","E":1000,"s":"SYN","t":9,"p":"10","q":"3","T":1000,"m":true,"M":true}|}
  in
  (match
     Binance_decoder.decode_one Instrument.syn ~seq:2L ~rx_us:9L ~synthetic:false
       ~provenance:"fixture" (Jsonx.of_string trade_raw)
   with
  | Ok e ->
      check "decode trade" (
        match e.kind with
        | Event.Trade t -> t.buyer_is_maker && t.qty = 3L && t.trade_id = 9L
        | _ -> false)
  | Error e -> check_eq "decode trade" e "ok");
  let qtest =
    QCheck.Test.make ~count:100 ~name:"random book invariants"
      QCheck.(list (pair (int_range 1 40) (int_range 0 15)))
      (fun xs ->
        let b =
          List.fold_left
            (fun b (p, q) ->
              match Order_book.apply_level b Order_book.Bid (Int64.of_int p) (Int64.of_int q) with
              | Ok b -> b
              | Error _ -> b)
            (Order_book.empty Instrument.syn) xs
        in
        Invariants.violations b = [] && Invariants.touches_consistent b)
  in
  let code = QCheck_base_runner.run_tests ~verbose:false [ qtest ] in
  check "qcheck" (code = 0);
  if !failed = 0 && code = 0 then (
    Printf.printf "all tests passed\n%!";
    exit 0)
  else (
    Printf.printf "%d checks failed\n%!" !failed;
    exit 1)
