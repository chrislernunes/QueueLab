(** Decode Binance spot public JSON into canonical events.

    Exchange timestamps in the public payload are milliseconds. They are stored
    as microseconds ([ms * 1000]). That is a unit conversion, not extra
    precision. Receive timestamps are supplied by the capture layer and are a
    different clock. Their difference is not a one-way latency. *)

type raw = { rx_us : int64; kind : string; payload : Jsonx.json }

let ms_field name j =
  match Jsonx.member name j with
  | None -> 0L
  | Some v -> Int64.mul (Jsonx.i64_of v) 1000L

let unwrap j =
  match Jsonx.member "data" j with
  | Some inner when Jsonx.member "stream" j <> None -> inner
  | _ -> j

let levels inst side_name j =
  match Jsonx.member side_name j with
  | None -> Ok []
  | Some v ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | el :: rest -> (
            match Jsonx.list_of el with
            | [ p; q ] -> (
                match
                  ( Instrument.parse_price inst (Jsonx.string_of p),
                    Instrument.parse_qty inst (Jsonx.string_of q) )
                with
                | Ok price, Ok qty -> go ({ Event.px = price; sz = qty } :: acc) rest
                | Error e, _ | _, Error e -> Error (side_name ^ ": " ^ e))
            | _ -> Error "level is not a pair")
      in
      go [] (Jsonx.list_of v)

let decode_one inst ~seq ~rx_us ~synthetic ~provenance payload =
  let j = unwrap payload in
  let kind =
    match Jsonx.member "e" j with
    | Some (`String s) -> s
    | _ -> ""
  in
  let symbol_of default =
    match Jsonx.member "s" j with Some (`String s) -> s | _ -> default
  in
  match kind with
  | "depthUpdate" -> (
      match (levels inst "b" j, levels inst "a" j) with
      | Error e, _ | _, Error e -> Error e
      | Ok bids, Ok asks ->
          Ok
            {
              Event.schema = Event.schema_version;
              symbol = symbol_of (inst : Instrument.t).symbol;
              receive_time_us = rx_us;
              seq_index = seq;
              synthetic;
              provenance;
              kind =
                Depth
                  {
                    first_id = Jsonx.i64_of (Jsonx.require "U" j);
                    final_id = Jsonx.i64_of (Jsonx.require "u" j);
                    bids;
                    asks;
                    exchange_time_us = ms_field "E" j;
                  };
            })
  | "trade" -> (
      match
        ( Instrument.parse_price inst (Jsonx.string_of (Jsonx.require "p" j)),
          Instrument.parse_qty inst (Jsonx.string_of (Jsonx.require "q" j)) )
      with
      | Error e, _ | _, Error e -> Error e
      | Ok price, Ok qty ->
          Ok
            {
              Event.schema = Event.schema_version;
              symbol = symbol_of (inst : Instrument.t).symbol;
              receive_time_us = rx_us;
              seq_index = seq;
              synthetic;
              provenance;
              kind =
                Trade
                  {
                    trade_id = Jsonx.i64_of (Jsonx.require "t" j);
                    price;
                    qty;
                    buyer_is_maker = Jsonx.bool_of (Jsonx.require "m" j);
                    exchange_time_us = ms_field "T" j;
                  };
            })
  | "aggTrade" -> (
      match
        ( Instrument.parse_price inst (Jsonx.string_of (Jsonx.require "p" j)),
          Instrument.parse_qty inst (Jsonx.string_of (Jsonx.require "q" j)) )
      with
      | Error e, _ | _, Error e -> Error e
      | Ok price, Ok qty ->
          Ok
            {
              Event.schema = Event.schema_version;
              symbol = symbol_of (inst : Instrument.t).symbol;
              receive_time_us = rx_us;
              seq_index = seq;
              synthetic;
              provenance;
              kind =
                Agg_trade
                  {
                    agg_id = Jsonx.i64_of (Jsonx.require "a" j);
                    first_trade_id = Jsonx.i64_of (Jsonx.require "f" j);
                    last_trade_id = Jsonx.i64_of (Jsonx.require "l" j);
                    price;
                    qty;
                    buyer_is_maker = Jsonx.bool_of (Jsonx.require "m" j);
                    exchange_time_us = ms_field "T" j;
                  };
            })
  | "" -> (
      match Jsonx.member "lastUpdateId" j with
      | None -> Error "unrecognized payload"
      | Some _ -> (
          match (levels inst "bids" j, levels inst "asks" j) with
          | Error e, _ | _, Error e -> Error e
          | Ok bids, Ok asks ->
              Ok
                {
                  Event.schema = Event.schema_version;
                  symbol = (inst : Instrument.t).symbol;
                  receive_time_us = rx_us;
                  seq_index = seq;
                  synthetic;
                  provenance;
                  kind =
                    Snapshot
                      {
                        last_update_id = Jsonx.i64_of (Jsonx.require "lastUpdateId" j);
                        bids;
                        asks;
                        exchange_time_us = rx_us;
                      };
                }))
  | other -> Error ("unsupported event type " ^ other)

let decode_raw (inst : Instrument.t) ~seq ~synthetic ~provenance (raw : raw) =
  match raw.kind with
  | "interrupt" ->
      let reason =
        match Jsonx.member "reason" raw.payload with
        | Some (`String s) -> s
        | _ -> "capture interrupt"
      in
      Ok
        {
          Event.schema = Event.schema_version;
          symbol = (inst : Instrument.t).symbol;
          receive_time_us = raw.rx_us;
          seq_index = seq;
          synthetic;
          provenance;
          kind = Gap { expected_id = 0L; actual_id = 0L; reason };
        }
  | "snapshot" | "ws" ->
      decode_one inst ~seq ~rx_us:raw.rx_us ~synthetic ~provenance raw.payload
  | other -> Error ("unknown raw kind " ^ other)
