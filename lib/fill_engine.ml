(** Passive-order simulation on top of an observed book.

    The observed book is never mutated by simulated orders. Fills produced
    here are simulated and are not written back as trades.

    Virtual time is the exchange timestamp plus [md_delay_us] when the
    exchange timestamp exists. Decision logic uses only the book visible at
    the decision timestamp. Markouts are computed after the run from later
    mids and are not an input to any transition.

    Tie policy: a simulated action and a market event at the same virtual
    time are applied action-first, so the action does not see that event. *)

module SM = Map.Make (struct
  type t = Order_book.side * int64

  let compare (s1, p1) (s2, p2) =
    match (s1, s2) with
    | Order_book.Bid, Order_book.Ask -> -1
    | Order_book.Ask, Order_book.Bid -> 1
    | _ -> Int64.compare p1 p2
end)

type intent = {
  id : int64;
  release_us : int64;
  side : [ `Buy | `Sell ];
  qty : int64;
  offset_ticks : int64;
  cancel_after_us : int64 option;
}

type fill = {
  order_id : int64;
  time_us : int64;
  price : int64;
  qty : int64;
  event_seq : int64;
  trade_id : int64;
  ahead_public_before : int64;
  ahead_sim_before : int64;
  reason : string;
  mid_at_fill : int64 option;
}

type trace = {
  time_us : int64;
  event_seq : int64;
  kind : string;
  ahead_public : int64;
  ahead_sim : int64;
  displayed : int64;
}

type sim_order = {
  order : Lifecycle.t;
  queue : Queue_model.state option;
  release_us : int64;
  arrive_us : int64;
  cancel_effective_us : int64 option;
  fills : fill list;
  trace : trace list;
  first_fill_us : int64 option;
  touch_qty : int64 option;
  vol_abs : int64;
  spread : int64 option;
}

type config = {
  model : Queue_model.model;
  seed : int64;
  md_delay_us : int64;
  decision_delay_us : int64;
  submit_delay_us : int64;
  cancel_delay_us : int64;
  fee_ppm : int64;
  horizons_us : int64 list;
  vol_low_lt : int64;
  vol_high_ge : int64;
  intents : intent list;
  trace : bool;
}

type markout = {
  order_id : int64;
  horizon_us : int64;
  signed : int64 option;
}

type result = {
  orders : sim_order list;
  markouts : markout list;
  book_hash : string;
  sync_mode : string;
  diagnostics : Replay.diagnostics;
  mids : (int64 * int64 option) list;
  ledger : Ledger.t;
  warnings : string list;
  model : Queue_model.model;
  seed : int64;
}

let book_side = function `Buy -> Order_book.Bid | `Sell -> Order_book.Ask

let hit_side ~buyer_is_maker =
  if buyer_is_maker then Order_book.Bid else Order_book.Ask

let visible_time ev delay =
  Int64.add (Event.primary_time ev) delay

let vol_of changes =
  match changes with
  | [] -> 0L
  | xs ->
      let n = List.length xs in
      let sum = List.fold_left Int64.add 0L xs in
      Int64.div sum (Int64.of_int n)

let push_change changes mid_prev mid =
  match (mid_prev, mid) with
  | Some a, Some b ->
      let d = Int64.sub b a in
      let d = if d < 0L then Int64.neg d else d in
      let xs = d :: changes in
      let rec take n acc = function
        | _ when n = 0 -> List.rev acc
        | [] -> List.rev acc
        | y :: ys -> take (n - 1) (y :: acc) ys
      in
      take 20 [] xs
  | _ -> changes

let find_mid mids t =
  let rec walk last = function
    | [] -> last
    | (ts, m) :: rest -> if ts > t then last else walk m rest
  in
  walk None mids

let find_mid_at_or_after mids t =
  let rec walk = function
    | [] -> None
    | (ts, m) :: rest -> if ts >= t then m else walk rest
  in
  walk mids

let sim_ahead orders ~side ~price ~before_id =
  List.fold_left
    (fun acc o ->
      if
        o.order.id < before_id
        && book_side o.order.side = side
        && o.order.price = price
        && (match o.order.status with Lifecycle.Working | Lifecycle.Pending_cancel -> true | _ -> false)
      then Int64.add acc (Lifecycle.remaining o.order)
      else acc)
    0L orders

let apply_fill warnings orders id ~time ~event_seq ~trade_id (fx : Queue_model.trade_effect) ~mid fee_ppm ledger =
  let rec go acc ledger = function
    | [] -> (List.rev acc, ledger)
    | o :: rest when o.order.id = id ->
        if fx.fill_qty <= 0L then go (o :: acc) ledger rest
        else
          let tr =
            if fx.fill_qty = Lifecycle.remaining o.order then
              Lifecycle.Fill fx.fill_qty
            else Lifecycle.Partial fx.fill_qty
          in
          (match Lifecycle.apply o.order tr with
          | Error e ->
              warnings := Lifecycle.reason_name e :: !warnings;
              go (o :: acc) ledger rest
          | Ok order ->
              let price = order.price in
              let notional = Fees.quote_notional ~price ~qty:fx.fill_qty in
              let fee = Fees.fee ~notional ~fee_ppm in
              let ledger =
                Ledger.fill ledger ~side:order.side ~price ~qty:fx.fill_qty ~fee
              in
              let fill =
                {
                  order_id = id;
                  time_us = time;
                  price;
                  qty = fx.fill_qty;
                  event_seq;
                  trade_id;
                  ahead_public_before = fx.ahead_public_before;
                  ahead_sim_before = fx.ahead_sim_before;
                  reason = fx.reason;
                  mid_at_fill = mid;
                }
              in
              let order =
                if order.status = Lifecycle.Filled then
                  { order with done_time_us = Some time }
                else order
              in
              let o =
                {
                  o with
                  order;
                  queue = Some fx.state;
                  fills = fill :: o.fills;
                  first_fill_us =
                    (match o.first_fill_us with None -> Some time | Some _ as x -> x);
                }
              in
              go (o :: acc) ledger rest)
    | o :: rest -> go (o :: acc) ledger rest
  in
  go [] ledger orders

let run inst (cfg : config) events =
  let rng = Rng.create cfg.seed in
  let events = List.sort Event.compare events in
  let warnings = ref [] in
  let sync = ref (Depth_sync.create inst) in
  let diag = ref Replay.empty_diag in
  let mids = ref [] in
  let last_mid = ref None in
  let changes = ref [] in
  let pending = ref SM.empty in
  let ledger = ref Ledger.empty in
  let orders =
    ref
      (List.map
         (fun (intent : intent) ->
           let arrive =
             Int64.add intent.release_us
               (Int64.add cfg.decision_delay_us cfg.submit_delay_us)
           in
           let cancel_effective =
             match intent.cancel_after_us with
             | None -> None
             | Some d ->
                 Some (Int64.add (Int64.add intent.release_us d) cfg.cancel_delay_us)
           in
           {
             order =
               {
                 Lifecycle.id = intent.id;
                 side = intent.side;
                 price = 0L;
                 orig_qty = intent.qty;
                 filled_qty = 0L;
                 status = Lifecycle.Created;
                 decision_time_us = intent.release_us;
                 arrival_time_us = None;
                 done_time_us = None;
                 decision_mid = None;
                 decision_spread = None;
                 decision_touch_qty = None;
                 reject = None;
               };
             queue = None;
             release_us = intent.release_us;
             arrive_us = arrive;
             cancel_effective_us = cancel_effective;
             fills = [];
             trace = [];
             first_fill_us = None;
             touch_qty = None;
             vol_abs = 0L;
             spread = None;
           })
         cfg.intents)
  in
  let trace_add o ~time ~seq ~kind ~displayed =
    if not cfg.trace then o
    else
      match o.queue with
      | None -> o
      | Some q ->
          {
            o with
            trace =
              {
                time_us = time;
                event_seq = seq;
                kind;
                ahead_public = q.ahead_public;
                ahead_sim = q.ahead_sim;
                displayed;
              }
              :: o.trace;
          }
  in
  let pending_at side price =
    match SM.find_opt (side, price) !pending with Some q -> q | None -> 0L
  in
  let add_pending side price qty =
    let cur = pending_at side price in
    pending := SM.add (side, price) (Int64.add cur qty) !pending
  in
  let clear_pending side price = pending := SM.add (side, price) 0L !pending in
  let offset_of id =
    match List.find_opt (fun (i : intent) -> i.id = id) cfg.intents with
    | Some i -> i.offset_ticks
    | None -> 0L
  in
  let update_order id f =
    orders := List.map (fun o -> if o.order.id = id then f o else o) !orders
  in
  let decide id =
    let o = List.find (fun o -> o.order.id = id) !orders in
    if o.order.status <> Lifecycle.Created then ()
    else
      let book = !sync.book in
      let mid = Order_book.mid book in
      let spread = Order_book.spread book in
      let vol = vol_of !changes in
      let side = book_side o.order.side in
      let touch =
        match side with
        | Order_book.Bid -> Order_book.best_bid book
        | Order_book.Ask -> Order_book.best_ask book
      in
      let other =
        match side with
        | Order_book.Bid -> Order_book.best_ask book
        | Order_book.Ask -> Order_book.best_bid book
      in
      match Lifecycle.apply o.order Lifecycle.Send with
      | Error e -> warnings := Lifecycle.reason_name e :: !warnings
      | Ok sent -> (
          match touch with
          | None -> (
              match Lifecycle.apply sent (Lifecycle.Reject Lifecycle.No_market) with
              | Ok order ->
                  update_order id (fun o ->
                      {
                        o with
                        order =
                          {
                            order with
                            decision_mid = mid;
                            decision_spread = spread;
                            done_time_us = Some o.release_us;
                          };
                        vol_abs = vol;
                        spread;
                      })
              | Error e -> warnings := Lifecycle.reason_name e :: !warnings)
          | Some (touch_px, touch_qty) ->
              let off = Int64.mul (offset_of id) inst.tick in
              let price =
                match o.order.side with
                | `Buy -> Int64.sub touch_px off
                | `Sell -> Int64.add touch_px off
              in
              let crosses =
                match (o.order.side, other) with
                | `Buy, Some (ask, _) -> price >= ask
                | `Sell, Some (bid, _) -> price <= bid
                | _ -> false
              in
              let reject =
                if crosses then Some Lifecycle.Would_cross
                else if price <= 0L then Some Lifecycle.Bad_qty
                else
                  match Instrument.check_price inst price with
                  | Error _ -> Some Lifecycle.Off_tick
                  | Ok () -> (
                      match Instrument.check_qty inst o.order.orig_qty with
                      | Error _ -> Some Lifecycle.Off_lot
                      | Ok () -> None)
              in
              match reject with
              | Some r -> (
                  match Lifecycle.apply sent (Lifecycle.Reject r) with
                  | Ok order ->
                      update_order id (fun o ->
                          {
                            o with
                            order =
                              {
                                order with
                                price;
                                decision_mid = mid;
                                decision_spread = spread;
                                decision_touch_qty = Some touch_qty;
                                done_time_us = Some o.release_us;
                              };
                            touch_qty = Some touch_qty;
                            vol_abs = vol;
                            spread;
                          })
                  | Error e -> warnings := Lifecycle.reason_name e :: !warnings)
              | None ->
                  update_order id (fun o ->
                      {
                        o with
                        order =
                          {
                            sent with
                            price;
                            decision_mid = mid;
                            decision_spread = spread;
                            decision_touch_qty = Some touch_qty;
                          };
                        touch_qty = Some touch_qty;
                        vol_abs = vol;
                        spread;
                      }))
  in
  let arrive id time =
    let o = List.find (fun o -> o.order.id = id) !orders in
    if o.order.status <> Lifecycle.Pending_new then ()
    else
      let cancel_already =
        match o.cancel_effective_us with
        | Some t -> t <= time
        | None -> false
      in
      if cancel_already then
        match Lifecycle.apply o.order (Lifecycle.Reject Lifecycle.Cancel_before_arrival) with
        | Ok order ->
            update_order id (fun o ->
                { o with order = { order with done_time_us = Some time; arrival_time_us = Some time } })
        | Error e -> warnings := Lifecycle.reason_name e :: !warnings
      else
        match Lifecycle.apply o.order Lifecycle.Accept with
        | Error e -> warnings := Lifecycle.reason_name e :: !warnings
        | Ok order ->
            let side = book_side order.side in
            let raw = Order_book.qty_at !sync.book side order.price in
            let effective = max 0L (Int64.sub raw (pending_at side order.price)) in
            let sim = sim_ahead !orders ~side ~price:order.price ~before_id:id in
            let q = Queue_model.join ~displayed_effective:effective ~sim_ahead:sim in
            update_order id (fun o ->
                let o =
                  {
                    o with
                    order = { order with arrival_time_us = Some time };
                    queue = Some q;
                  }
                in
                trace_add o ~time ~seq:(-1L) ~kind:"join" ~displayed:effective)
  in
  let cancel id time =
    let o = List.find (fun o -> o.order.id = id) !orders in
    match o.order.status with
    | Lifecycle.Pending_new -> ()
    | Lifecycle.Working -> (
        let rem = Lifecycle.remaining o.order in
        match Lifecycle.apply o.order Lifecycle.Cancel_request with
        | Error e -> warnings := Lifecycle.reason_name e :: !warnings
        | Ok order -> (
            match Lifecycle.apply order Lifecycle.Cancel_ack with
            | Error e -> warnings := Lifecycle.reason_name e :: !warnings
            | Ok order ->
                update_order id (fun o ->
                    { o with order = { order with done_time_us = Some time } });
                orders :=
                  List.map
                    (fun other ->
                      if
                        other.order.id > id
                        && other.order.price = o.order.price
                        && other.order.side = o.order.side
                      then
                        match other.queue with
                        | None -> other
                        | Some q ->
                            { other with queue = Some (Queue_model.on_sim_cancel_ahead q rem) }
                      else other)
                    !orders))
    | Lifecycle.Filled -> warnings := "cancel_lost_race" :: !warnings
    | Lifecycle.Pending_cancel | Lifecycle.Cancelled | Lifecycle.Rejected | Lifecycle.Created
      ->
        ()
  in
  let on_trade ~time ~seq ~trade_id ~price ~qty ~buyer_is_maker =
    let side = hit_side ~buyer_is_maker in
    add_pending side price qty;
    let mid = Order_book.mid !sync.book in
    let targets =
      List.filter
        (fun o ->
          match o.order.status with
          | Lifecycle.Working | Lifecycle.Pending_cancel ->
              book_side o.order.side = side && o.queue <> None
          | _ -> false)
        !orders
    in
    List.iter
      (fun o ->
        match o.queue with
        | None -> ()
        | Some q ->
            let through =
              match o.order.side with
              | `Buy -> price < o.order.price
              | `Sell -> price > o.order.price
            in
            let at = price = o.order.price in
            if (not through) && not at then ()
            else
              let fx =
                Queue_model.on_trade q ~trade_qty:qty
                  ~our_remaining:(Lifecycle.remaining o.order)
                  ~through
              in
              let updated, led =
                apply_fill warnings !orders o.order.id ~time ~event_seq:seq ~trade_id
                  fx ~mid cfg.fee_ppm !ledger
              in
              orders := updated;
              ledger := led;
              if fx.fill_qty = 0L then
                update_order o.order.id (fun cur ->
                    let cur = { cur with queue = Some fx.state } in
                    trace_add cur ~time ~seq ~kind:fx.reason ~displayed:(pending_at side price))
              else
                update_order o.order.id (fun cur ->
                    trace_add cur ~time ~seq ~kind:fx.reason ~displayed:(pending_at side price)))
      targets
  in
  let on_depth_level ~time ~seq side price new_qty =
    clear_pending side price;
    let targets =
      List.filter
        (fun o ->
          match (o.order.status, o.queue) with
          | (Lifecycle.Working | Lifecycle.Pending_cancel), Some _ ->
              book_side o.order.side = side && o.order.price = price
          | _ -> false)
        !orders
    in
    List.iter
      (fun o ->
        match o.queue with
        | None -> ()
        | Some q ->
            let fx = Queue_model.on_depth q cfg.model rng ~new_displayed:new_qty in
            update_order o.order.id (fun cur ->
                let cur = { cur with queue = Some fx.state } in
                trace_add cur ~time ~seq ~kind:"depth" ~displayed:new_qty))
      targets
  in
  let apply_market ev =
    let time = visible_time ev cfg.md_delay_us in
    let diag', sync', mid = Replay.apply !diag !sync ev in
    diag := diag';
    sync := sync';
    (match mid with
    | None -> ()
    | Some (ts, m) ->
        mids := (ts, m) :: !mids;
        changes := push_change !changes !last_mid m;
        last_mid := m);
    match ev.kind with
    | Event.Trade t ->
        on_trade ~time ~seq:ev.seq_index ~trade_id:t.trade_id ~price:t.price ~qty:t.qty
          ~buyer_is_maker:t.buyer_is_maker
    | Event.Agg_trade _ -> ()
    | Event.Depth d ->
        List.iter
          (fun (l : Event.level) ->
            on_depth_level ~time ~seq:ev.seq_index Order_book.Bid l.px l.sz)
          d.bids;
        List.iter
          (fun (l : Event.level) ->
            on_depth_level ~time ~seq:ev.seq_index Order_book.Ask l.px l.sz)
          d.asks
    | Event.Snapshot _ ->
        pending := SM.empty;
        let working =
          List.filter
            (fun o ->
              match o.order.status with
              | Lifecycle.Working | Lifecycle.Pending_cancel -> true
              | _ -> false)
            !orders
        in
        if working <> [] then warnings := "snapshot_resync_rejoins_at_back" :: !warnings;
        List.iter
          (fun o ->
            let side = book_side o.order.side in
            let raw = Order_book.qty_at !sync.book side o.order.price in
            let sim = sim_ahead !orders ~side ~price:o.order.price ~before_id:o.order.id in
            let q = Queue_model.join ~displayed_effective:raw ~sim_ahead:sim in
            update_order o.order.id (fun cur ->
                let cur = { cur with queue = Some q } in
                trace_add cur ~time ~seq:ev.seq_index ~kind:"resync_join" ~displayed:raw))
          working
    | Event.Gap g -> warnings := g.reason :: !warnings
  in
  let actions =
    let acc = ref [] in
    List.iter
      (fun (i : intent) ->
        acc := (i.release_us, 0, i.id, `Decide) :: !acc;
        let arrive =
          Int64.add i.release_us (Int64.add cfg.decision_delay_us cfg.submit_delay_us)
        in
        acc := (arrive, 1, i.id, `Arrive) :: !acc;
        match i.cancel_after_us with
        | None -> ()
        | Some d ->
            let t = Int64.add (Int64.add i.release_us d) cfg.cancel_delay_us in
            acc := (t, 2, i.id, `Cancel) :: !acc)
      cfg.intents;
    List.sort
      (fun (t1, p1, i1, _) (t2, p2, i2, _) ->
        let c = Int64.compare t1 t2 in
        if c <> 0 then c else let c = Int.compare p1 p2 in if c <> 0 then c else Int64.compare i1 i2)
      !acc
  in
  let rec loop actions events =
    match (actions, events) with
    | [], [] -> ()
    | (t, _, id, kind) :: arest, [] ->
        (match kind with
        | `Decide -> decide id
        | `Arrive -> arrive id t
        | `Cancel -> cancel id t);
        loop arest []
    | [], ev :: erest ->
        apply_market ev;
        loop [] erest
    | ((t, _, id, kind) :: arest as acts), (ev :: erest as evs) ->
        let vt = visible_time ev cfg.md_delay_us in
        if t <= vt then (
          (match kind with
          | `Decide -> decide id
          | `Arrive -> arrive id t
          | `Cancel -> cancel id t);
          loop arest evs)
        else (
          apply_market ev;
          loop acts erest)
  in
  loop actions events;
  let mids = List.rev !mids in
  let markouts =
    List.concat_map
      (fun o ->
        List.concat_map
          (fun (f : fill) ->
            List.map
              (fun h ->
                let target = Int64.add f.time_us h in
                let later = find_mid_at_or_after mids target in
                let signed =
                  match (f.mid_at_fill, later) with
                  | Some m0, Some m1 ->
                      Some
                        (match o.order.side with
                        | `Buy -> Int64.sub m1 m0
                        | `Sell -> Int64.sub m0 m1)
                  | _ -> None
                in
                { order_id = o.order.id; horizon_us = h; signed })
              cfg.horizons_us)
          (List.rev o.fills))
      !orders
  in
  {
    orders =
      List.map
        (fun o -> { o with fills = List.rev o.fills; trace = List.rev o.trace })
        !orders;
    markouts;
    book_hash = Order_book.hash !sync.book;
    sync_mode = Depth_sync.mode_name !sync.mode;
    diagnostics = { !diag with warnings = List.rev !diag.warnings };
    mids;
    ledger = !ledger;
    warnings = List.rev !warnings;
    model = cfg.model;
    seed = cfg.seed;
  }
