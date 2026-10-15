(** Average-cost ledger.

    Cash is in quote units of [price * qty] (both already scaled) and is net
    of fees. Position is signed quantity, positive long.

    Identity, for any mark in price units:
      equity = cash + mark * position
      equity = realized + unrealized(mark)

    Fees reduce cash immediately and reduce [realized] immediately, including
    fees on opens. A fill that flips through zero does not split the fee
    between the closing and opening pieces; the whole fee hits [realized].
    Average cost of the new position is the flip price. *)

type t = {
  cash : int64;
  pos : int64;
  avg : int64;
  realized : int64;
  fees : int64;
}

let empty = { cash = 0L; pos = 0L; avg = 0L; realized = 0L; fees = 0L }

let equity t ~mark = Int64.add t.cash (Int64.mul mark t.pos)

let unrealized t ~mark =
  if t.pos = 0L then 0L else Int64.mul (Int64.sub mark t.avg) t.pos

let identity_ok t ~mark =
  Int64.add t.realized (unrealized t ~mark) = equity t ~mark

let fill t ~side ~price ~qty ~fee =
  if qty < 0L then failwith "negative fill qty"
  else
    let q = match side with `Buy -> qty | `Sell -> Int64.neg qty in
    let notional = Int64.mul price qty in
    let cash =
      match side with
      | `Buy -> Int64.sub (Int64.sub t.cash notional) fee
      | `Sell -> Int64.sub (Int64.add t.cash notional) fee
    in
    let fees = Int64.add t.fees fee in
    if t.pos = 0L || Int64.mul t.pos q > 0L then
      let new_pos = Int64.add t.pos q in
      let num = Int64.add (Int64.mul t.avg t.pos) (Int64.mul price q) in
      let avg = if new_pos = 0L then 0L else Int64.div num new_pos in
      { cash; pos = new_pos; avg; realized = Int64.sub t.realized fee; fees }
    else
      let close = if Int64.abs t.pos < qty then Int64.abs t.pos else qty in
      let pnl =
        if t.pos > 0L then Int64.mul (Int64.sub price t.avg) close
        else Int64.mul (Int64.sub t.avg price) close
      in
      let new_pos = Int64.add t.pos q in
      let realized = Int64.sub (Int64.add t.realized pnl) fee in
      if new_pos = 0L then { cash; pos = 0L; avg = 0L; realized; fees }
      else if Int64.mul t.pos new_pos < 0L then
        { cash; pos = new_pos; avg = price; realized; fees }
      else { cash; pos = new_pos; avg = t.avg; realized; fees }
