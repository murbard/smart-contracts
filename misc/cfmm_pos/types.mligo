#if TYPES_MLIGO
#else
#define TYPES_MLIGO

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
    to_dy : address ; (* Recipient of dy *)
}

type x_to_y_rec_param = {s : storage ; dx : nat ; dy : nat}

type y_to_x_param = {
    dy : nat ;
    deadline : timestamp ;
    min_dx : nat ;
    to_dx : address ; (* Recipient of dx *)
}

type y_to_x_rec_param = x_to_y_rec_param

type result = (operation list) * storage

#endif