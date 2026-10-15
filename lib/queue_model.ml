(** Queue-position models for a hypothetical order that is not in the public feed.

    Shared assumption (all three models): the order joins at the back of the
    *displayed* queue. Earlier simulated orders at the same price are known
    and sit ahead of later ones ([ahead_sim]). Public cancellations are not
    identified by the feed.

    Let [C] be the displayed-size reduction at our price that is not explained
    by prints already observed at that price since the previous depth update.

    - Conservative: remove [C] from behind the order first. Only the residual
      that cannot fit behind is taken from ahead. This is the pessimistic
      bound. It is not "never reduce ahead": volume identity forces it when
      [C] exceeds the estimated quantity behind.
    - Probabilistic: each unit of [C] is independently ahead with probability
      [ahead_public / (ahead_public + behind)], sampled with a seeded binomial
      draw. See [Rng.binomial] for the exact-vs-normal cutoff, which is part
      of the definition.
    - Optimistic: remove [C] from ahead first. Favorable sensitivity bound,
      not an estimate of the true queue.

    Adds join behind us and do not change [ahead_public]. Trades consume the
    front of the queue and are the only events that create fills. A trade
    strictly through the limit fills the residual order (price priority over
    the worse price). That assumption is biased if the print is off-book or
    the visible book was inconsistent.

    A decrease in displayed depth is not itself a fill. The same print is not
    also counted as a cancellation: unexplained size is
    [max(0, baseline - pending_trades - new_displayed)]. *)

type model = Conservative | Probabilistic | Optimistic

let model_name = function
  | Conservative -> "conservative"
  | Probabilistic -> "probabilistic"
  | Optimistic -> "optimistic"

let model_of_string = function
  | "conservative" -> Ok Conservative
  | "probabilistic" | "baseline" -> Ok Probabilistic
  | "optimistic" -> Ok Optimistic
  | s -> Error ("unknown queue model " ^ s)

type state = {
  ahead_public : int64;
  ahead_sim : int64;
  pending_trades : int64;
  last_displayed : int64;
}

let ahead s = Int64.add s.ahead_public s.ahead_sim

let join ~displayed_effective ~sim_ahead =
  {
    ahead_public = max 0L displayed_effective;
    ahead_sim = max 0L sim_ahead;
    pending_trades = 0L;
    last_displayed = max 0L displayed_effective;
  }

type trade_effect = {
  state : state;
  fill_qty : int64;
  ahead_public_before : int64;
  ahead_sim_before : int64;
  reason : string;
}

let on_trade s ~trade_qty ~our_remaining ~through =
  let ahead_public_before = s.ahead_public in
  let ahead_sim_before = s.ahead_sim in
  if through then
    {
      state =
        {
          s with
          ahead_public = 0L;
          ahead_sim = 0L;
        };
      fill_qty = our_remaining;
      ahead_public_before;
      ahead_sim_before;
      reason = "trade_through";
    }
  else
    let total = ahead s in
    let raw_fill = if trade_qty > total then Int64.sub trade_qty total else 0L in
    let fill_qty = if raw_fill < our_remaining then raw_fill else our_remaining in
    let use = trade_qty in
    let from_pub = if use < s.ahead_public then use else s.ahead_public in
    let left = Int64.sub use from_pub in
    let from_sim = if left < s.ahead_sim then left else s.ahead_sim in
    {
      state =
        {
          s with
          ahead_public = Int64.sub s.ahead_public from_pub;
          ahead_sim = Int64.sub s.ahead_sim from_sim;
          pending_trades = Int64.add s.pending_trades trade_qty;
        };
      fill_qty;
      ahead_public_before;
      ahead_sim_before;
      reason = "trade_at_price";
    }

type depth_effect = {
  state : state;
  cancel_qty : int64;
  from_ahead : int64;
  add_qty : int64;
}

let on_depth s model rng ~new_displayed =
  let new_displayed = max 0L new_displayed in
  let baseline = s.last_displayed in
  let traded = s.pending_trades in
  let visible_after = max 0L (Int64.sub baseline traded) in
  let ahead_public =
    if s.ahead_public < visible_after then s.ahead_public else visible_after
  in
  let behind = max 0L (Int64.sub visible_after ahead_public) in
  let cancel = max 0L (Int64.sub visible_after new_displayed) in
  let add_qty = max 0L (Int64.sub new_displayed visible_after) in
  let from_ahead =
    match model with
    | Conservative ->
        let from_behind = if behind < cancel then behind else cancel in
        Int64.sub cancel from_behind
    | Optimistic -> if ahead_public < cancel then ahead_public else cancel
    | Probabilistic ->
        let denom = Int64.add ahead_public behind in
        if denom = 0L || cancel = 0L then 0L
        else
          let p = Int64.to_float ahead_public /. Int64.to_float denom in
          let draw = Rng.binomial rng ~n:cancel ~p ~exact_limit:4096L in
          let draw = if draw < 0L then 0L else draw in
          let draw = if draw > cancel then cancel else draw in
          if draw < ahead_public then draw else ahead_public
  in
  let from_ahead = if from_ahead < 0L then 0L else from_ahead in
  let ahead_public = Int64.sub ahead_public from_ahead in
  {
    state =
      {
        ahead_public;
        ahead_sim = s.ahead_sim;
        pending_trades = 0L;
        last_displayed = new_displayed;
      };
    cancel_qty = cancel;
    from_ahead;
    add_qty;
  }

let on_sim_cancel_ahead s qty =
  let qty = if qty < 0L then 0L else qty in
  let take = if qty < s.ahead_sim then qty else s.ahead_sim in
  { s with ahead_sim = Int64.sub s.ahead_sim take }
