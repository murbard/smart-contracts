(* Some contract specific constants, to be edited per deployment
 todo implement burn [@inline] let const_ctez_burn_fee_bps : nat = 5n *)
[@inline] let const_fee_bps : nat = 10n

(* Tick types, representing pieces of the curve offered between different tick segments. *)
type tick_index = {i : int}
type nat_balance = {x : nat ; y : nat}
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
type position = {liquidity : nat ; fee_growth : nat_balance}
type position_map = (position_index, position) big_map

type storage = {
    liquidity : nat ; (* virtual liquidity, the value L for which the curve locally looks like x * y = L^2 *)
    sqrt_price : nat ; (* square root of the virtual price, the value P for which P = x / y *)
    lo : tick_index ;
    i_c : int ;(* the highest tick corresponding to a price less than or equal to sqrt_price^2, does not necessarily corresponds to a boundary *)
    fee_growth : nat_balance ;
    balance : nat_balance ;
    ticks : tick_map ;
    positions : position_map ;
}

let get_tick (ticks : (tick_index, tick_state) big_map) (index: tick_index) : tick_state =
    match Big_map.find_opt tick_index ticks with
    | None -> failwith "Assertion error, tick should be initialized"
    | Some state -> state

type x_to_y_rec_param = {
    s : storage ;
    dx : nat ;
    dy : nat}

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

let set_position (storage : s) (i_l : nat) (i_u : nat) (i_l_l : nat) (i_u_l : nat) =
    (* Initialize ticks if need be. *)
    let ticks = s.ticks in
    let ticks = initialize_tick (ticks, i_l, i_l_l, if s.i_c >= i_l then s.fee_growth else {x = 0n ; y = 0n}) in
    let ticks = initialize_tick (ticks, i_u, i_u_l, if s.i_c >= i_u then s.fee_growth else {x = 0n ; y = 0n}) in
    (* Form position key. *)
    let position_key = (Tezos.sender, i_l, i_u) in
    (* Get accumulated fees for this position. *)
    let fees = collect_fees position_key in

    (* add delta_l to position *)







type result = storage * (operation list)

(* Trade up to a quantity dx of asset x, receives dy *)
let x_to_y (s : storage) (dx : nat) : result =
    let r = x_to_y_rec {s = s ; dx = dx ; dy = 0n} in
    let dx_spent = dx - r.dx in
    let dy_received = r.dy in
    let s_new = {s with balance = {x = s.balance.x + dx_psent ;  y = assert_nat (s.balance.y - dy_received)}} in
    let op_receive_x = abc in
    let op_send_y = def in
    (s_new, [op_receive_x ; op_send_y])

type parameter =
| X_to_Y of nat
| Y_to_X of nat
| Set_position of unit

let main ((s, p) : storage * parameter) : result = match parameter with
| X_to_Y dx -> x_to_y s dx
| Y_to_X dy -> y_to_x s dY
| Set_position -> (failwith "moo" : result)
