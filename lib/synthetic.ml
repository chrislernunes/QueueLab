(** Seeded synthetic books for tests, CI and the research notebooks.

    These are not Binance recordings. Every event is marked [synthetic=true].
    The generator is deterministic in [seed]. *)

let instrument =
  match Instrument.make ~symbol:"BTCUSDT" ~price_scale:2 ~qty_scale:4 ~tick:1L ~lot:1L with
  | Ok i -> i
  | Error e -> failwith e

let session ~seed ~steps =
  if steps < 1 then failwith "steps"
  else
    let rng = Rng.create seed in
    let inst = instrument in
    let n = 5 in
    let best = ref 6_500_000L in
    let spr = 10L in
    let bid_q = Array.make n 100L in
    let ask_q = Array.make n 100L in
    let fresh () = Int64.add 60L (Int64.of_int (Rng.int rng 80)) in
    for i = 0 to n - 1 do
      bid_q.(i) <- fresh ();
      ask_q.(i) <- fresh ()
    done;
    let seq = ref 0L in
    let mk ~time kind =
      let e =
        {
          Event.schema = Event.schema_version;
          symbol = inst.symbol;
          kind;
          receive_time_us = time;
          seq_index = !seq;
          synthetic = true;
          provenance = "synthetic-generator";
        }
      in
      seq := Int64.succ !seq;
      e
    in
    let bid_px i = Int64.sub !best (Int64.of_int i) in
    let ask_px i = Int64.add !best (Int64.add spr (Int64.of_int i)) in
    let pairs arr px = List.init n (fun i -> (px i, arr.(i))) in
    let prev_b = ref (pairs bid_q bid_px) in
    let prev_a = ref (pairs ask_q ask_px) in
    let levels_of px_qty = List.map (fun (price, qty) -> { Event.px = price; sz = qty }) px_qty in
    let snapshot =
      mk ~time:0L
        (Event.Snapshot
           {
             last_update_id = 1000L;
             bids = levels_of !prev_b;
             asks = levels_of !prev_a;
             exchange_time_us = 0L;
           })
    in
    let uid = ref 1000L in
    let trade_id = ref 1L in
    let out = ref [ snapshot ] in
    let emit_depth time =
      let nb = pairs bid_q bid_px in
      let na = pairs ask_q ask_px in
      let gone prev cur =
        List.filter_map
          (fun (p, _) -> if List.mem_assoc p cur then None else Some { Event.px = p; sz = 0L })
          prev
      in
      uid := Int64.succ !uid;
      let ev =
        mk ~time
          (Event.Depth
             {
               first_id = !uid;
               final_id = !uid;
               bids = levels_of nb @ gone !prev_b nb;
               asks = levels_of na @ gone !prev_a na;
               exchange_time_us = time;
             })
      in
      prev_b := nb;
      prev_a := na;
      ev
    in
    let shift_up () =
      for i = n - 1 downto 1 do
        bid_q.(i) <- bid_q.(i - 1)
      done;
      bid_q.(0) <- fresh ();
      for i = 0 to n - 2 do
        ask_q.(i) <- ask_q.(i + 1)
      done;
      ask_q.(n - 1) <- fresh ()
    in
    let shift_down () =
      for i = 0 to n - 2 do
        bid_q.(i) <- bid_q.(i + 1)
      done;
      bid_q.(n - 1) <- fresh ();
      for i = n - 1 downto 1 do
        ask_q.(i) <- ask_q.(i - 1)
      done;
      ask_q.(0) <- fresh ()
    in
    for step = 1 to steps do
      let time = Int64.mul (Int64.of_int step) 1000L in
      let roll = Rng.int rng 12 in
      if roll = 0 then (
        best := Int64.sub !best 1L;
        shift_down ())
      else if roll = 11 then (
        best := Int64.add !best 1L;
        shift_up ());
      let level = Rng.int rng n in
      let delta = Int64.of_int (Rng.int rng 21 - 10) in
      if Rng.int rng 2 = 0 then begin
        let q = Int64.add bid_q.(level) delta in
        bid_q.(level) <- if q < 5L then 5L else q
      end else begin
        let q = Int64.add ask_q.(level) delta in
        ask_q.(level) <- if q < 5L then 5L else q
      end;
      if Rng.int rng 3 = 0 then (
        let hit_bid = Rng.int rng 2 = 0 in
        let avail = if hit_bid then bid_q.(0) else ask_q.(0) in
        let want = Int64.add 1L (Int64.of_int (Rng.int rng 8)) in
        let qty = if want < avail then want else Int64.sub avail 1L in
        if qty >= 1L then (
          if hit_bid then bid_q.(0) <- Int64.sub bid_q.(0) qty
          else ask_q.(0) <- Int64.sub ask_q.(0) qty;
          let price = if hit_bid then bid_px 0 else ask_px 0 in
          let id = !trade_id in
          trade_id := Int64.succ !trade_id;
          out :=
            mk ~time
              (Event.Trade
                 {
                   trade_id = id;
                   price;
                   qty;
                   buyer_is_maker = hit_bid;
                   exchange_time_us = time;
                 })
            :: !out));
      out := emit_depth time :: !out
    done;
    (inst, List.rev !out)

let write_session ~dir ~seed ~steps =
  let inst, events = session ~seed ~steps in
  let man =
    Manifest.write_dataset ~dir ~instrument:inst ~market:"spot" ~synthetic:true
      ~capture_version:"synthetic-1" ~trade_source:"raw"
      ~notes:
        "Synthetic price-level session. Not Binance market data. Displayed size is generated, not observed. Queue position is not ground truth."
      ~gaps:[] events
  in
  (man, events)

let divergence_events () =
  let ev ~seq ~time kind =
    {
      Event.schema = Event.schema_version;
      symbol = "SYN";
      kind;
      receive_time_us = time;
      seq_index = seq;
      synthetic = true;
      provenance = "constructed-divergence";
    }
  in
  let lvl px sz = { Event.px; sz } in
  [
    ev ~seq:0L ~time:1000L
      (Event.Snapshot
         {
           last_update_id = 100L;
           bids = [ lvl 100L 10L; lvl 99L 20L ];
           asks = [ lvl 101L 8L ];
           exchange_time_us = 1000L;
         });
    ev ~seq:1L ~time:2000L
      (Event.Depth
         {
           first_id = 101L;
           final_id = 101L;
           bids = [ lvl 100L 15L ];
           asks = [];
           exchange_time_us = 2000L;
         });
    ev ~seq:2L ~time:3000L
      (Event.Depth
         {
           first_id = 102L;
           final_id = 102L;
           bids = [ lvl 100L 9L ];
           asks = [];
           exchange_time_us = 3000L;
         });
    ev ~seq:3L ~time:4000L
      (Event.Trade
         {
           trade_id = 7L;
           price = 100L;
           qty = 5L;
           buyer_is_maker = true;
           exchange_time_us = 4000L;
         });
    ev ~seq:4L ~time:4000L
      (Event.Depth
         {
           first_id = 103L;
           final_id = 103L;
           bids = [ lvl 100L 4L ];
           asks = [];
           exchange_time_us = 4000L;
         });
  ]

let write_divergence ~dir =
  Manifest.write_dataset ~dir ~instrument:Instrument.syn ~market:"spot" ~synthetic:true
    ~capture_version:"constructed-divergence-1" ~trade_source:"raw" ~gaps:[]
    ~notes:
      "Constructed scenario, not market data. One passive buy is built so optimistic and conservative cancellation rules disagree."
    (divergence_events ())
