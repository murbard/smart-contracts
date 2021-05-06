import "math.mligo"

(* Some contract specific constants, to be edited per deployment *)
[@inline] let const_tick_spacing : nat = 2n
[@inline] let const_ctez_burn_fee_bps : nat = 5n
[@inline] let const_fee_bps : nat = 10n

[@inline] let const_sqrt_price_2_14 : nat = 2n

type position = {liquidity : nat ; feeGrowthInsideLast : nat}
type xy = {x : nat ; y : nat}

(* Tick types, representing pieces of the curve offered between different tick segments. *)
type tick_index = {i : int}
type tick_state = {iPrev : int ; iNext : int ; dL : int ; feeGrowthOutside : xy ; nPos : nat}
type tick_map = (tick_index, tick_state) big_map

(* Position types, representing LP positions. *)
type position_index = {owner : address ; iLo : nat ; iHi : bat}
type position = {liquidity : nat ; feeGrowthInsideLast : nat}
type position_map = (position_index, position) big_map

type fixed_point : { v : nat ; offset : int }

type storage_constants = {
    sqrt_price_from_log2_tick : (tick_index, fixed_point) big_map
}

type storage = {
    liquidity : nat ; (* virtual liquidity, the value L for which the curve locally looks like x * y = L^2 *)
    sqrt_price : nat ; (* square root of the virtual price, the value P for which P = x / y *)
    tick : tick_index ;
    ic : int ; (* the highest tick corresponding to a price less than or equal to sqrt_price^2, does not necessarily corresponds to a boundary *)
    feeGrowth : xy ;
    ticks : tick_map ;
    positions : position_map ;
}

def assert_nat (i : int) : nat =
    match is_nat i with
    | None -> failwith ("assertion error")
    | Some n -> n

type x_to_y_rec_no_fees_param = {s : storage ; dx : nat ; sqrt_price_new : nat ; total_dy }

let rec x_to_y_rec_no_fees (p : x_to_y_rec_no_fees_param) : storage * nat =
    let s : storage = p.s in
    let dx : nat = p.dx in
    let sqrt_price_new = p.sqrt_price_new in
    let total_dy = p.total_dy

    let ic_new : nat = assert_nat (s.ic + floor_log_half_bps(sqrt_price_new, sqrt_price)) in
    let prev_tick = s.tick_state[s.tick.i].prev  in

    if ic_new >= prev_tick.i then (* we didn't cross the tick *)
        let dy = (sqrt_price_new - s.sqrt_price) * s.liquidity in
        ({s with sqrt_price = sqrt_price_new ; ic = ic_new}, dy + total_dy)

    else (*ah naah, we did cross the tick *)
        (* first we need to update the liquidity *)
        (* we cleared the range from prev_tick to tick, how does that affect our liquidity? *)
        let delta_liquidity = s.ticks_down[s.tick].net_liquidity in
        liquidity_new = s.liquidity - delta_liquidity in
        (* how much was filled, for how much ? *)
        let filled_dx = delta_liquidity * sqrt_price * (exp(-ic)/sqrt_price - exp(-ic_low)/sqrt_price)
        let dy = delta_liquidity * (exp(ic) - exp(ic_low)) in
        (* clear the tick range *)


        (* now call recursively *)
        let s = {s with liquidity = liquidity_new ; ic = ic_new}

        ctez_to_token_rec {p with s = s ; dx = dx - filled_dx  ; total_dy = dy + total_dy}






