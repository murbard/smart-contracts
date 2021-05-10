#include "types.mligo"
#include "consts.mligo"
#include "math.mligo"
#include "helpers.mligo"


(* Helper function for x_to_y, recursively loops over ticks to execute a trade. *)
let rec x_to_y_rec (p : x_to_y_rec_param) : x_to_y_rec_param =
    if p.s.liquidity = 0n then
        p
    else
        (* The fee that would be extracted from selling dx. *)
        let fee  = ceildiv (p.dx * const_fee_bps) 10000n in
        (* What the new price will be, assuming it's within the current tick. *)
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


let rec y_to_x_rec (p : y_to_x_rec_param) : y_to_x_rec_param =
 if p.s.liquidity = 0n then
        p
    else
        (* The fee that would be extracted from selling dy. *)
        let fee  = ceildiv (p.dy * const_fee_bps) 10000n in
        (* What the new price will be, assuming it's within the current tick. *)
        let sqrt_price_new = sqrt_price_move p.s.liquidity p.s.sqrt_price (assert_nat (p.dy - fee)) in
        (* What the new value of ic will be. *)
        let i_c_new = p.s.i_c + floor_log_half_bps(sqrt_price_new, p.s.sqrt_price) in
        let tick = get_tick p.s.ticks p.s.lo  in
        let i_u = tick.next in
        if i_c_new < i_u.i then
            (* The trade did not push us past the current tick. *)
            let dx = Bitwise.shift_right ((assert_nat (sqrt_price_new - p.s.sqrt_price)) * p.s.liquidity) 90n in
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                i_c = i_c_new ;
                fee_growth = {p.s.fee_growth with y = p.s.fee_growth.y + fee / p.s.liquidity}} in
            {p with s = s_new ; dy = 0n ; dx = p.dx + dx}
        else
            (*We did cross the tick. *)
            (* The cached price corresponding to hi. *)
            let next_tick = get_tick p.s.ticks i_u in
            let sqrt_price_new = next_tick.sqrt_price in
            (* How much dx will we receive for going all the wax to lo. *)

            (* FIXME this is wrong, invert prices etc *)
            let dx = Bitwise.shift_right (p.s.liquidity * (assert_nat (sqrt_price_new - p.s.sqrt_price))) 90n in
            (* How much dy does that correspond to. *)
            let dy_for_dx = ceildiv (Bitwise.shift_left dx 180n) (p.s.sqrt_price * sqrt_price_new) in
            (* plouf *)


            (* We will have to consumme more dy than that because a fee will be applied. *)
            let dy_consummed = ceildiv (dy_for_dx * 10000n) const_one_minus_fee_bps in
            (* Deduct the fee we will actually be paying. *)
            let fee = assert_nat (dy_consummed - dy_for_dx) in
            let fee_growth_y_new = p.s.fee_growth.y + (floordiv (Bitwise.shift_left fee 128n) p.s.liquidity) in
            (* Flip fee growth. *)
            let fee_growth_outside_new = {tick.fee_growth_outside with y = assert_nat (fee_growth_y_new - tick.fee_growth_outside.y)} in
            let fee_growth_new = {p.s.fee_growth with y=fee_growth_y_new} in
            let tick_new = {tick with fee_growth_outside = fee_growth_outside_new} in
            let ticks_new = Big_map.update p.s.lo (Some tick_new) p.s.ticks  in
            (* Update global state. *)
            let s_new = {p.s with
                sqrt_price = sqrt_price_new ;
                lo = i_u ;
                i_c = i_u.i ;
                ticks = ticks_new ;
                fee_growth = fee_growth_new ;
                (* Update liquidity as we enter new tick region. *)
                liquidity = assert_nat (p.s.liquidity - tick.delta_liquidity)
                } in
            let p_new = {p with s = s_new ; dy = assert_nat (p.dy - dy_consummed) ; dx = p.dx + dx} in
            y_to_x_rec p_new


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
            let op_send_y = y_transfer Tezos.self_address p.to_dy dy_received in
            ([op_receive_x ; op_send_y], s_new)


(* Trade up to a quantity dy of asset y, receives dx *)
let y_to_x (s : storage) (p : y_to_x_param) : result =
    if Tezos.now > p.deadline then
        (failwith "Past deadline" : result)
    else
        let r = y_to_x_rec {s = s ; dy = p.dy ; dx = 0n} in
        let dy_spent = assert_nat (p.dy - r.dy) in
        let dx_received = r.dx in
        let s_new = {s with balance = {y = s.balance.y + dy_spent ;  x = assert_nat (s.balance.x - dx_received)}} in
        if dx_received < p.min_dx then
            (failwith "dx received < min_dx" : result)
        else
            let op_receive_y = y_transfer Tezos.sender Tezos.self_address dy_spent in
            let op_send_x = x_transfer Tezos.self_address p.to_dx dx_received in
            ([op_receive_y ; op_send_x], s_new)
