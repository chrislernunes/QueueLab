(** Book invariants checked by validation and tests.

    - no non-positive displayed quantity
    - prices sit on the instrument tick
    - best bid is the maximum bid and best ask is the minimum ask
      (implied by the ordered map)
    A crossed book is reported as a diagnostic, not an invariant failure:
    public feeds can print a transient cross and the engine must not hide it. *)

let violations book = Order_book.invariant_violations book

let touches_consistent book =
  match (Order_book.best_bid book, Order_book.top book Order_book.Bid 1) with
  | None, [] -> true
  | Some (p, q), [ (p2, q2) ] -> p = p2 && q = q2
  | _ -> false
