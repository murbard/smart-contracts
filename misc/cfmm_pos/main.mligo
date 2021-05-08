#include "transfers.mligo"
#include "math.mligo"

(* FIXME: implement fixed point arithmetic for sqrt_price! *)
(* TODO implement error strings as nat error codes *)


[@inline] let const_infinity : nat = 4294967296n

(* Some contract specific constants, to be edited per deployment
 todo implement burn [@inline] let const_ctez_burn_fee_bps : nat = 5n *)

[@inline] let const_fee_bps : nat = 10n  (* CHANGEME *)


(* Tick types, representing pieces of the curve offered between different tick segments. *)
type tick_index = {i : int}
type nat_balance = {x : nat ; y : nat}
type int_balance = {x : int ; y : int}

(* Information stored for every initialized tick. *)
type tick_state = {
    prev : tick_index ;
    next : tick_index ;
    delta_liquidity : int ;
    n_positions : nat ;
    fee_growth_outside : nat_balance ;
    sqrt_price : nat
}
type tick_map = (tick_index, tick_state) big_map

(* Position types, representing LP positions. *)
type position_index = {owner : address ; lo : tick_index ; hi : tick_index}
type position_state = {liquidity : nat ; fee_growth_inside : nat_balance ; fee_growth_inside_last : nat_balance}
type position_map = (position_index, position_state) big_map

type storage = {
    liquidity : nat ; (* virtual liquidity, the value L for which the curve locally looks like x * y = L^2 *)
    sqrt_price : nat ; (* square root of the virtual price, the value P for which P = x / y *)
    i_c : int ;(* the highest tick corresponding to a price less than or equal to sqrt_price^2, does not necessarily corresponds to a boundary *)
    lo : tick_index ; (* the highest initialized tick lower than or equal to i_c *)
    fee_growth : nat_balance ;
    balance : nat_balance ;
    ticks : tick_map ;
    positions : position_map ;
}

type result = storage * (operation list)

(* Helper function to grab a tick we know exists in the tick indexed state. *)
let get_tick (ticks : (tick_index, tick_state) big_map) (index: tick_index) : tick_state =
    match Big_map.find_opt tick_index ticks with
    | None -> failwith "Assertion error, tick should be initialized"
    | Some state -> state


type x_to_y_rec_param = {s : storage ; dx : nat ; dy : nat}

(* Helper function for x_to_y, recursively loops over ticks to execute a trade. *)
let rec x_to_y_rec (p : x_to_y_rec_param) : x_to_y_rec_param =
    if p.s.liquidity = 0 then
        p
    else
        (* The fee that would be extracted from selling dx. *)
        let fee  = ceildiv (p.dx * const_fee_bps) 10000n in
        (* The what the new price will be, assuming it's within the current tick. *)
        let sqrt_price_new = floordiv (p.s.liquidity * p.s.sqrt_price) (p.s.liquidity + (p.dx - fee) * p.s.sqrt_price) in
        (* What the new value of ic will be. *)
        let i_c_new = assert_nat (p.s.i_c + floor_log_half_bps(sqrt_price_new, p.s.sqrt_price)) in
        if i_c_new >= p.s.tick.i then
            (* The trade did not push us past the current tick. *)
            let dy = (sqrt_price_new - s.sqrt_price) * s.liquidity in
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                i_c = i_c_new ;
                fee_growth = p.s.fee_growth + fee / p.s.liquidity} in
            {p with s = s_new ; dx = 0 ; dy = p.dy + dy}
        else
            (*We did cross the tick. *)
            (* The tick we are currently in. *)
            let tick = (get_tick p.s.ticks p.s.lo) in
            (* The tick index below that. *)
            let lo_new = tick.prev in
            (* The cached price corresponding to lo. *)
            let sqrt_price_new = tick.sqrt_price in
            (* How much dY will we receive for going all the way to lo. *)
            let dy = p.s.liquidty * (p.s.sqrt_price - sqrt_price_new) in
            (* How much dX does that correspond to. *)
            let dx_for_dy = dy / (p.s.sqrt_price * sqrt_price_new) in
            (* We will have to consumme more dx than that because a fee will be applied. *)
            let dx_consummed = ceildiv (dx_for_dy * 10000n) (10000n - const_fee_bps) in
            (* Deduct the fee we will actually be paying. *)
            let fee = assert_nat (dx_consummed - dx_for_dy) in
            let fee_growth_new = p.s.fee_growth + fee / p.s.liquidity in
            (* Flip fee growth. *)
            let tick_new = {tick with fee_growth_outside = asset_nat (fee_growth_new - ticks.fee_growth_outside)} in
            let ticks_new = Big_map.update p.s.lo tick_new p.s.ticks  in
            (* Update global state. *)
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                lo = lo_new ;
                i_c = p.s.lo ;
                ticks = tick_new ;
                fee_growth =  fee_growth_new ;
                (* Update liquidity as we enter new tick region. *)
                liquidity = assert_nat (p.s.liquidity - tick.delta_liquidty)
                } in
            let p_new = {p with s = s_new ; dx = assert_nat (p.dx - dx_consummed) ; dy = p.dy + dy} in
            x_to_y_rec p_new

(* Trade up to a quantity dx of asset x, receives dy *)
let x_to_y (s : storage) (p : x_to_y_param) : result =
    if Tezos.now > deadline then
        (failwith "Past deadline" : result)
    else
        let r = x_to_y_rec {s = s ; dx = p.dx ; dy = 0n} in
        let dx_spent = dx - r.dx in
        let dy_received = r.dy in
        let s_new = {s with balance = {x = s.balance.x + dx_psent ;  y = assert_nat (s.balance.y - dy_received)}} in
        if dy_received < p.min_dy then
            (failwith "dy received < min_dy" : result)
        else
            let op_receive_x = x_transfer Tezos.sender Tezos.self_address dx_spent in
            let op_send_y = y_transfer Tezos.self_address to_ dy_received in
            ([op_receive_x ; op_send_y], s_new)

let y_to_x (s : storage) (dy : nat) : result =
    (failwith "not implemented" : result)

let rec initialize_tick ((ticks, i, i_l, initial_fee_growth_outside) : tick_map * tick_index * tick_index * nat_balance) : tick_map =
    if Big_map.mem i ticks then
        ticks
    else if i_l.i > i.i then
        (failwith "Invalid witness" : nat)
    else
        let tick = get_tick i_l in
        let i_next = tick.next in
        if i_next.i > i.i then
            let tick_next = get_tick i_next in
            let ticks = Big_map.update i_l (Some {tick with i_next = i}) in
            let ticks = Big_map.update i_next (Some {tick_next with prev = i}) in
            let ticks = Big_map.update i (Some {
                prev = i_l ;
                next = i_next ;
                delta_liquidity = 0n ;
                n_positions = 0n ;
                fee_growth_outside = initial_fee_growth_outside;
                sqrt_price = half_bps_pow i}) in
            ticks
        else
            initialize_tick (ticks, i, i_next)

let incr_n_positions (ticks : tick_map) (i : tick_index) (incr : int) =
    let tick = get_tick ticks i in
    let n_pos = assert_nat (tick.n_positions + incr) in
    let tick = if pos = 0n then None else Some {tick with n_positions = n_pos} in
    (*TODO, repair linked list *)
    Big_map.update i tick ticks

let set_position (s : storage) (i_l : nat) (i_u : nat) (i_l_l : nat) (i_u_l : nat) (delta_liquity : int) : result =
    (* Initialize ticks if need be. *)
    let ticks = s.ticks in
    let ticks = initialize_tick (ticks, i_l, i_l_l, (if s.i_c >= i_l then s.fee_growth else {x = 0n ; y = 0n})) in
    let ticks = initialize_tick (ticks, i_u, i_u_l, (if s.i_c >= i_u then s.fee_growth else {x = 0n ; y = 0n})) in
    (* Form position key. *)
    let position_key = (Tezos.sender, i_l, i_u) in
    (* Grab existing position or create an empty one *)
    let (position, is_new) = match (Big_map.find_opt position_key s.positions) with
    | Some position -> (position, False)
    | None -> ({liquidity = 0n ; fee_growth_inside = {x = 0n ; y = 0n} ; fee_growth_inside_last = {x = 0n; y = 0n}}, True) in
    (* Get accumulated fees for this position. *)
    let fees = collect_fees s position_key in
    (* Update liquidity of position. *)
    let liquidity_new = assert_nat (position.liquidity + delta_liquidity) in
    let position = {position with liquidity = liquidity_new} in
    (* Reference counting the positions associated with a tick *)
    let ticks = (if liquidity_new = 0n and then
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
    let position_entry = if liquidity_new = 0n then None else Some {position with liquidity = liquidity_new} in
    let positions = Big_map.update position_key position_entry s.positions in
    (* Compute how much should be deposited / withdrawn to change liquidity by delta_liquidity * )

    (* Add or remove liquidity above the current tick *)
    let (s, delta) =
    if s.i_c < i_l then
        (s, {x = delta_liquidity  * (srp_u - srp_l) / (srp_l * srp_u) ; y = 0})
    else if i_l <= s.i_c and s.i_c < i_u then
        (* update interval we are in, if need be ... *)
        let s = {s with i_l = if i_l > s.i_l then i_l else s.i_l ; liquidity = liquidity + delta_liquidity} in
        (s, {x = delta_liquidity * (srp_u - srp) / (srp * srp_u) ; y = delta_liquidity * (srp - srp_l)})
    else (* i_c >= i_u *)
        (s, {x = 0 ; y = delta_liquidity ; y = delta_liquidity * (srp_u - srp_l)}) in

    (* Collect fees to increase withdrawal or reduce required deposit. *)
    let delta = {x = delta.x - fees.x ; y = delta.y - fees.y} in

    let op_x = if delta.x > 0 then
        x_transfer Tezos.sender Tezos.self_address delta.x
    else
        x_transfer Tezos.self_address to_ delta.x in

    let op_y = if delta.x > 0 then
        y_transfer Tezos.sender Tezos.self_address delta.y
    else
        y_transfer Tezos.self_address to_ delta.y in

    ([op_x, op_y], {s with positions = positions; ticks = ticks})

(* Entrypoints types *)
type set_position_param = {
    i_l : tick_index ;
    i_u : tick_index ;
    i_l_l : tick_index ;
    i_u_l : tick_index ;
    delta_liquidity : int
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

type parameter =
| X_to_Y of x_to_y_param (* TODO add deadline and minimum token bought. *)
| Y_to_X of nat
| Set_position of set_position_param (* TODO add deadline, maximum tokens contributed, and maximum liquidity present *)
| X_to_X_prime of address (* equivalent to token_to_token *)

let main ((s, p) : storage * parameter) : result = match parameter with
| X_to_Y dx -> x_to_y s dx
| Y_to_X dy -> y_to_x s dy
| Set_position p -> set_position p.i_l p.i_u p.i_l_l p.i_l_u p.delta_liquidity
| X_to_X_prime -> (failwith "not implemented" : result) (*TODO implement*)