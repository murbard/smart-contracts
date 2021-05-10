
#if CONSTS_MLIGO
#else
#define CONSTS_MLIGO
(* TODO implement error strings as nat error codes *)


[@inline] let const_max_tick : nat = 1048575n

(* Some contract specific constants, to be edited per deployment
 todo implement burn [@inline] let const_ctez_burn_fee_bps : nat = 5n *)

(* Invariant : const_fee_bps + const_one_minus_fee_bps = 10000n *)
[@inline] let const_fee_bps : nat = 10n  (* CHANGEME if need be *)
[@inline] let const_one_minus_fee_bps : nat = 9990n (* CHANGEME if need be*)
#endif