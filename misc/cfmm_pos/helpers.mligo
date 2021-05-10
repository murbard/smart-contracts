#if HELPERS_MLIGO
#else
#define HELPERS_MLIGO

#include "math.mligo"

let sqrt_price_move (liquidity : nat) (sqrt_price : nat) (dx : nat) =
    (* floordiv because we want to overstate how much this trade lowers the price *)
    floordiv
        (Bitwise.shift_left (liquidity * sqrt_price) 90n)
        ((Bitwise.shift_left liquidity 90n) + dx * sqrt_price)


(* Helper function to grab a tick we know exists in the tick indexed state. *)
let get_tick (ticks : (tick_index, tick_state) big_map) (index: tick_index) : tick_state =
    match Big_map.find_opt index ticks with
    | None -> failwith "Assertion error, tick should be initialized"
    | Some state -> state

#endif