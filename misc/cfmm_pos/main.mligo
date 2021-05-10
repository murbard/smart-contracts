#include "transfers.mligo"
#include "math.mligo"

(* TODO implement error strings as nat error codes *)


[@inline] let const_max_tick : nat = 1048575n

(* Some contract specific constants, to be edited per deployment
 todo implement burn [@inline] let const_ctez_burn_fee_bps : nat = 5n *)

(* Invariant : const_fee_bps + const_one_minus_fee_bps = 10000n *)
[@inline] let const_fee_bps : nat = 10n  (* CHANGEME if need be *)
[@inline] let const_one_minus_fee_bps : nat = 9990n (* CHANGEME if need be*)

(* Tick types, representing pieces of the curve offered between different tick segments. *)
type tick_index = {i : int}
type balance_nat = {x : nat ; y : nat}
type balance_int = {x : int ; y : int}

(* Information stored for every initialized tick. *)
type tick_state = {
    prev : tick_index ;
    next : tick_index ;
    delta_liquidity : int ;
    n_positions : nat ;
    fee_growth_outside : balance_nat ;
    sqrt_price : nat
}

(* Useful for initial state, TODO, move out of this file *)
let max_tick_state = {
    prev = {i=-const_max_tick} ;
    next = {i=int(const_max_tick)} ;
    delta_liquidity = 0 ;
    n_positions = 1n ; (* prevents garbage collection *)
    fee_growth_outside = {x = 0n ; y = 0n} ;
    sqrt_price = 71107673757466966990985105047137336834554167630n ; (* Round[Exp[5/100000*(2^20-1)]*2^80] *)
} (* TODO consider using 2^90 precision instead so that every tick has a distinct sqrt_price *)

let min_tick_state = {
    prev = {i=-const_max_tick} ;
    next = {i=int(const_max_tick)} ;
    delta_liquidity = 0 ;
    n_positions = 1n ; (* prevents garbage collection *)
    fee_growth_outside = {x = 0n ; y = 0n} ;
    sqrt_price = 21n ; (* Round[Exp[-5/100000*(2^20-1)]*2^80] *)
}

(* </initial_state> *)

type tick_map = (tick_index, tick_state) big_map

(* Position types, representing LP positions. *)
type position_index = {owner : address ; lo : tick_index ; hi : tick_index}
type position_state = {liquidity : nat ; fee_growth_inside : balance_nat ; fee_growth_inside_last : balance_nat}
type position_map = (position_index, position_state) big_map

type storage = {
    liquidity : nat ; (* virtual liquidity, the value L for which the curve locally looks like x * y = L^2 *)
    sqrt_price : nat ; (* square root of the virtual price, the value P for which P = x / y *)
    i_c : int ;(* the highest tick corresponding to a price less than or equal to sqrt_price^2, does not necessarily corresponds to a boundary *)
    lo : tick_index ; (* the highest initialized tick lower than or equal to i_c *)
    fee_growth : balance_nat ;
    balance : balance_nat ;
    ticks : tick_map ;
    positions : position_map ;
}

(* Entrypoints types *)
type set_position_param = {
    i_l : tick_index ;
    i_u : tick_index ;
    i_l_l : tick_index ;
    i_u_l : tick_index ;
    delta_liquidity : int ;
    to_x : address ;
    to_y : address ;
}

type x_to_y_param = {
    dx : nat ;
    deadline : timestamp ;
    min_dy : nat ;
    to_ : address ; (* Recipient of dy *)
}

type y_to_x_param = {
    dy : nat ;
    deadline : timestamp ;
    min_dx : nat ;
    to_ : address ; (* Recipient of dy *)
}

type result = (operation list) * storage

(* Helper function to grab a tick we know exists in the tick indexed state. *)
let get_tick (ticks : (tick_index, tick_state) big_map) (index: tick_index) : tick_state =
    match Big_map.find_opt index ticks with
    | None -> failwith "Assertion error, tick should be initialized"
    | Some state -> state


let sqrt_price_move (liquidity : nat) (sqrt_price : nat) (dx : nat) =
    (* floordiv because we want to overstate how much this trade lowers the price *)
    floordiv
        (Bitwise.shift_left (liquidity * sqrt_price) 90n)
        ((Bitwise.shift_left liquidity 90n) + dx * sqrt_price)


type x_to_y_rec_param = {s : storage ; dx : nat ; dy : nat}

(* Helper function for x_to_y, recursively loops over ticks to execute a trade. *)
let rec x_to_y_rec (p : x_to_y_rec_param) : x_to_y_rec_param =
    if p.s.liquidity = 0n then
        p
    else
        (* The fee that would be extracted from selling dx. *)
        let fee  = ceildiv (p.dx * const_fee_bps) 10000n in
        (* The what the new price will be, assuming it's within the current tick. *)
        let sqrt_price_new = sqrt_price_move p.s.liquidity p.s.sqrt_price (assert_nat (p.dx - fee)) in
        (* What the new value of ic will be. *)
        let i_c_new = p.s.i_c + floor_log_half_bps(sqrt_price_new, p.s.sqrt_price) in
        if i_c_new >= p.s.lo.i then
            (* The trade did not push us past the current tick. *)
            let dy = Bitwise.shift_right ((assert_nat (p.s.sqrt_price - sqrt_price_new)) * p.s.liquidity) 90n in
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                i_c = i_c_new ;
                fee_growth = {p.s.fee_growth with x = p.s.fee_growth.x + fee / p.s.liquidity}} in
            {p with s = s_new ; dx = 0n ; dy = p.dy + dy}
        else
            (*We did cross the tick. *)
            (* The tick we are currently in. *)
            let tick  = (get_tick p.s.ticks p.s.lo) in
            (* The tick index below that. *)
            let lo_new = tick.prev in
            (* The cached price corresponding to lo. *)
            let sqrt_price_new = tick.sqrt_price in
            (* How much dY will we receive for going all the way to lo. *)
            let dy = Bitwise.shift_right (p.s.liquidity * (assert_nat (p.s.sqrt_price - sqrt_price_new))) 90n in
            (* How much dX does that correspond to. *)
            let dx_for_dy = ceildiv (Bitwise.shift_left dy 180n) (p.s.sqrt_price * sqrt_price_new) in
            (* We will have to consumme more dx than that because a fee will be applied. *)
            let dx_consummed = ceildiv (dx_for_dy * 10000n) const_one_minus_fee_bps in
            (* Deduct the fee we will actually be paying. *)
            let fee = assert_nat (dx_consummed - dx_for_dy) in
            let fee_growth_x_new = p.s.fee_growth.x + (floordiv (Bitwise.shift_left fee 128n) p.s.liquidity) in
            (* Flip fee growth. *)
            let fee_growth_outside_new = {tick.fee_growth_outside with x = assert_nat (fee_growth_x_new - tick.fee_growth_outside.x)} in
            let fee_growth_new = {p.s.fee_growth with x=fee_growth_x_new} in
            let tick_new = {tick with fee_growth_outside = fee_growth_outside_new} in
            let ticks_new = Big_map.update p.s.lo (Some tick_new) p.s.ticks  in
            (* Update global state. *)
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                lo = lo_new ;
                i_c = p.s.lo.i ;
                ticks = ticks_new ;
                fee_growth = fee_growth_new ;
                (* Update liquidity as we enter new tick region. *)
                liquidity = assert_nat (p.s.liquidity - tick.delta_liquidity)
                } in
            let p_new = {p with s = s_new ; dx = assert_nat (p.dx - dx_consummed) ; dy = p.dy + dy} in
            x_to_y_rec p_new

(* Trade up to a quantity dx of asset x, receives dy *)
let x_to_y (s : storage) (p : x_to_y_param) : result =
    if Tezos.now > p.deadline then
        (failwith "Past deadline" : result)
    else
        let r = x_to_y_rec {s = s ; dx = p.dx ; dy = 0n} in
        let dx_spent = assert_nat (p.dx - r.dx) in
        let dy_received = r.dy in
        let s_new = {s with balance = {x = s.balance.x + dx_spent ;  y = assert_nat (s.balance.y - dy_received)}} in
        if dy_received < p.min_dy then
            (failwith "dy received < min_dy" : result)
        else
            let op_receive_x = x_transfer Tezos.sender Tezos.self_address dx_spent in
            let op_send_y = y_transfer Tezos.self_address p.to_ dy_received in
            ([op_receive_x ; op_send_y], s_new)

let y_to_x (s : storage) (dy : nat) : result =
    (failwith "not implemented" : result)


let rec initialize_tick ((ticks, i, i_l, initial_fee_growth_outside) : tick_map * tick_index * tick_index * balance_nat) : tick_map =
    if Big_map.mem i ticks then
        ticks
    else if i_l.i > i.i then
        (failwith "Invalid witness" : tick_map)
    else
        let tick = get_tick ticks i_l in
        let i_next = tick.next in
        if i_next.i > i.i then
            let tick_next = get_tick ticks i_next in
            let ticks = Big_map.update i_l (Some {tick with next = i}) ticks in
            let ticks = Big_map.update i_next (Some {tick_next with prev = i}) ticks in
            let ticks = Big_map.update i (Some {
                prev = i_l ;
                next = i_next ;
                delta_liquidity = 0 ;
                n_positions = 0n ;
                fee_growth_outside = initial_fee_growth_outside;
                sqrt_price = half_bps_pow i.i}) ticks in
            ticks
        else
            initialize_tick (ticks, i, i_next, initial_fee_growth_outside)

let incr_n_positions (ticks : tick_map) (i : tick_index) (incr : int) =
    let tick = get_tick ticks i in
    let n_pos = assert_nat (tick.n_positions + incr) in
    if n_pos = 0n then
        (*  Garbage collect the tick.
            The largest and smallest tick are initialized with n_positions = 1 so they cannot
            be accidentally garbage collected. *)
        let prev = get_tick ticks tick.prev in
        let next = get_tick ticks tick.next in
        let prev = {prev with next = tick.next} in
        let next = {next with prev = tick.prev} in
        let ticks = Big_map.update i (None : tick_state option) ticks in
        let ticks = Big_map.update tick.prev (Some prev) ticks in
        let ticks = Big_map.update tick.next (Some next) ticks in
        ticks
    else
        Big_map.update i (Some {tick with n_positions = n_pos}) ticks

let collect_fees (s : storage) (key : position_index) : storage * balance_nat =
    let position = match Big_map.find_opt key s.positions with
    | None -> (failwith "position does not exist" : position_state)
    | Some position -> position in
    let tick_lo = get_tick s.ticks key.lo in
    let tick_hi = get_tick s.ticks key.hi in
    let f_a = if s.i_c >= key.hi.i then
        { x = assert_nat (s.fee_growth.x - tick_hi.fee_growth_outside.x);
          y = assert_nat (s.fee_growth.y - tick_hi.fee_growth_outside.y)}
    else
        tick_hi.fee_growth_outside in
    let f_b = if s.i_c >= key.lo.i then
        tick_lo.fee_growth_outside
    else
        { x = assert_nat (s.fee_growth.x - tick_lo.fee_growth_outside.x) ;
          y = assert_nat (s.fee_growth.y - tick_lo.fee_growth_outside.y) } in
    let fee_growth_inside = {
        x = assert_nat (s.fee_growth.x - f_a.x - f_b.x) ;
        y = assert_nat (s.fee_growth.y - f_a.y - f_b.y) } in
    let fees = {
        x = Bitwise.shift_right ((assert_nat (fee_growth_inside.x - position.fee_growth_inside_last.x)) * position.liquidity) 128;
        y = Bitwise.shift_right ((assert_nat (fee_growth_inside.y - position.fee_growth_inside_last.y)) * position.liquidity) 128} in
    let position = {position with fee_growth_inside_last = fee_growth_inside} in
    let positions = Big_map.update key (Some position) s.positions in
    ({s with positions = positions}, fees)


let set_position (s : storage) (i_l : tick_index) (i_u : tick_index) (i_l_l : tick_index) (i_u_l : tick_index) (delta_liquidity : int) (to_x : address) (to_y : address) : result =
    (* Initialize ticks if need be. *)
    let ticks = s.ticks in
    let ticks = initialize_tick (ticks, i_l, i_l_l, (if s.i_c >= i_l.i then s.fee_growth else {x = 0n ; y = 0n})) in
    let ticks = initialize_tick (ticks, i_u, i_u_l, (if s.i_c >= i_u.i then s.fee_growth else {x = 0n ; y = 0n})) in
    (* Form position key. *)
    let position_key = {owner=Tezos.sender ; lo=i_l; hi=i_u} in
    (* Grab existing position or create an empty one *)
    let (position, is_new) = match (Big_map.find_opt position_key s.positions) with
    | Some position -> (position, false)
    | None -> ({liquidity = 0n ; fee_growth_inside = {x = 0n ; y = 0n} ; fee_growth_inside_last = {x = 0n; y = 0n}}, true) in
    (* Get accumulated fees for this position. *)
    let s, fees = collect_fees s position_key in
    (* Update liquidity of position. *)
    let liquidity_new = assert_nat (position.liquidity + delta_liquidity) in
    let position = {position with liquidity = liquidity_new} in
    (* Reference counting the positions associated with a tick *)
    let ticks = (if liquidity_new = 0n then
        if is_new then
            ticks
        else
            let ticks = incr_n_positions ticks i_l (-1) in
            let ticks = incr_n_positions ticks i_u (-1) in
            ticks
    else
        if is_new then
            let ticks = incr_n_positions ticks i_l (1) in
            let ticks = incr_n_positions ticks i_u (1) in
            ticks
        else
            ticks) in
    let position_entry : position_state option = if liquidity_new = 0n then None else Some {position with liquidity = liquidity_new} in
    let positions = Big_map.update position_key position_entry s.positions in
    (* Compute how much should be deposited / withdrawn to change liquidity by delta_liquidity *)

    (* Grab cached prices for the interval *)
    let tick_u = get_tick ticks i_u in
    let tick_l = get_tick ticks i_l in
    let srp_u = tick_u.sqrt_price in
    let srp_l = tick_l.sqrt_price in

    (* Add or remove liquidity above the current tick *)
    let (s, delta) =
    if s.i_c < i_l.i then
        (s, {
            (* If I'm adding liquidity, x will be positive, I want to overestimate it, if x I'm taking away
                liquidity, I want to to underestimate what I'm receiving. *)
            x = ceildiv_int (delta_liquidity * (int (Bitwise.shift_left (assert_nat (srp_u - srp_l)) 90n))) (int (srp_l * srp_u)) ;
            y = 0})
    else if i_l.i <= s.i_c && s.i_c < i_u.i then
        (* update interval we are in, if need be ... *)
        let s = {s with lo = if i_l.i > s.lo.i then i_l else s.lo ; liquidity = assert_nat (s.liquidity + delta_liquidity)} in
        (s, {
            x = ceildiv_int (delta_liquidity * (int (Bitwise.shift_left (assert_nat (srp_u - s.sqrt_price)) 90n))) (int (s.sqrt_price * srp_u)) ;
            y = shift_int (delta_liquidity * (s.sqrt_price - srp_l)) (-80)
            })
    else (* i_c >= i_u *)
        (s, {x = 0 ; y = shift_int (delta_liquidity * (srp_u - srp_l)) (-80) }) in

    (* Collect fees to increase withdrawal or reduce required deposit. *)
    let delta = {x = delta.x - fees.x ; y = delta.y - fees.y} in

    let op_x = if delta.x > 0 then
        x_transfer Tezos.sender Tezos.self_address (abs delta.x)
    else
        x_transfer Tezos.self_address to_x (abs delta.x) in

    let op_y = if delta.y > 0 then
        y_transfer Tezos.sender Tezos.self_address (abs delta.y)
    else
        y_transfer Tezos.self_address to_y (abs delta.y) in

    ([op_x ; op_y], {s with positions = positions; ticks = ticks})

type parameter =
| X_to_Y of x_to_y_param
| Y_to_X of nat
| Set_position of set_position_param (* TODO add deadline, maximum tokens contributed, and maximum liquidity present *)
| X_to_X_prime of address (* equivalent to token_to_token *)

let main ((p, s) : parameter * storage) : result = match p with
| X_to_Y dx -> x_to_y s dx
| Y_to_X dy -> y_to_x s dy
| Set_position p -> set_position s p.i_l p.i_u p.i_l_l p.i_u_l p.delta_liquidity p.to_x p.to_y
| X_to_X_prime -> (failwith "not implemented" : result) (*TODO implement*)