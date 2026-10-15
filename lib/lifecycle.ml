(** Simulated order lifecycle. These states are not exchange acknowledgements. *)

type status =
  | Created
  | Pending_new
  | Working
  | Pending_cancel
  | Filled
  | Cancelled
  | Rejected

type reason =
  | No_market
  | Off_tick
  | Off_lot
  | Bad_qty
  | Cancel_before_arrival
  | Too_late
  | Would_cross
  | Illegal of string

type t = {
  id : int64;
  side : [ `Buy | `Sell ];
  price : int64;
  orig_qty : int64;
  filled_qty : int64;
  status : status;
  decision_time_us : int64;
  arrival_time_us : int64 option;
  done_time_us : int64 option;
  decision_mid : int64 option;
  decision_spread : int64 option;
  decision_touch_qty : int64 option;
  reject : reason option;
}

let remaining o = Int64.sub o.orig_qty o.filled_qty

let is_terminal = function
  | Filled | Cancelled | Rejected -> true
  | Created | Pending_new | Working | Pending_cancel -> false

let status_name = function
  | Created -> "created"
  | Pending_new -> "pending_new"
  | Working -> "working"
  | Pending_cancel -> "pending_cancel"
  | Filled -> "filled"
  | Cancelled -> "cancelled"
  | Rejected -> "rejected"

let reason_name = function
  | No_market -> "no_market"
  | Off_tick -> "off_tick"
  | Off_lot -> "off_lot"
  | Bad_qty -> "bad_qty"
  | Cancel_before_arrival -> "cancel_before_arrival"
  | Too_late -> "too_late"
  | Would_cross -> "would_cross"
  | Illegal s -> "illegal:" ^ s

type transition =
  | Send
  | Accept
  | Reject of reason
  | Partial of int64
  | Fill of int64
  | Cancel_request
  | Cancel_ack

let apply o tr =
  match (o.status, tr) with
  | Created, Send -> Ok { o with status = Pending_new }
  | Pending_new, Accept -> Ok { o with status = Working }
  | Pending_new, Reject r -> Ok { o with status = Rejected; reject = Some r }
  | Working, Partial q | Pending_cancel, Partial q ->
      if q <= 0L then Error (Illegal "non-positive partial")
      else if q > remaining o then Error (Illegal "overfill")
      else
        let filled_qty = Int64.add o.filled_qty q in
        let status = if filled_qty = o.orig_qty then Filled else o.status in
        Ok { o with filled_qty; status }
  | Working, Fill q | Pending_cancel, Fill q ->
      if q <> remaining o then Error (Illegal "fill is not the residual")
      else if q <= 0L then Error (Illegal "empty fill")
      else Ok { o with filled_qty = o.orig_qty; status = Filled }
  | Working, Cancel_request -> Ok { o with status = Pending_cancel }
  | Pending_cancel, Cancel_ack -> Ok { o with status = Cancelled }
  | (Filled | Cancelled | Rejected), _ -> Error (Illegal "terminal")
  | _, _ -> Error (Illegal "illegal transition")
