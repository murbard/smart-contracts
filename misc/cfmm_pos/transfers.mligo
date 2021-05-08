#define X_IS_FA2
//#define Y_IS_FA2

type token_id = nat

#if X_IS_FA2
[@inline] let const_x_token_id = 0n (* CHANGEME *)
type x_contract_transfer = (address * (address * (token_id * nat)) list) list
#else
type x_contract_transfer = address * (address * nat)
#endif

#if Y_IS_FA2
[@inline] let const_y_token_id = 0n (* CHANGEME *)
type y_contract_transfer = (address * (address * (token_id * nat)) list) list
#else
type y_contract_transfer = address * (address * nat)
#endif

[@inline] let const_x_token_entrypoint = ("KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn" : address) (* CHANGEME *)
[@inline] let const_y_token_entrypoint = ("KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn" : address) (* CHANGEME *)

(* Helper functions to make transfers in asset x and y. *)
let x_transfer (from : address) (to_ : address) (amnt : nat) : operation =
    let x_contract: x_contract_transfer contract =
    match (Tezos.get_contract_opt const_x_token_entrypoint : x_contract_transfer contract option) with
    | None -> (failwith "Invalid entrypoint for x contract transfer" : x_contract_transfer contract)
    | Some contract -> contract in
#if X_IS_FA2
    Tezos.transaction [(from, [(to_, (const_x_token_id, amnt))])] 0mutez x_contract
#else
    Tezos.transaction (from, (to_, amnt)) 0mutez x_contract
#endif

let y_transfer (from : address) (to_ : address) (amnt : nat) : operation =
    let y_contract: y_contract_transfer contract =
    match (Tezos.get_contract_opt const_y_token_entrypoint : y_contract_transfer contract option) with
    | None -> (failwith "Invalid entrypoint for x contract transfer" : y_contract_transfer contract)
    | Some contract -> contract in
#if Y_IS_FA2
    Tezos.transaction [(from, [(to_, (const_y_token_id, amnt))])] 0mutez y_contract
#else
    Tezos.transaction (from, (to_, amnt)) 0mutez y_contract
#endif
