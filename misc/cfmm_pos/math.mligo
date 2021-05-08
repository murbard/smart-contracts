type fixed_point = { v : nat ; offset : int }

[@inline] let fixed_point_mul (a : fixed_point) (b : fixed_point) : fixed_point =
    { v = a.v * b.v ; offset = a.offset + b.offset }


let ceildiv (numerator : nat) (denominator : nat) : nat = abs ((- numerator) / (int denominator))
let floordiv (numerator : nat) (denominator : nat) : nat =  numerator / denominator

(* accurate for x/y in [0.7, 1.5] *)
let floor_log_half_bps ((x, y) : nat * nat) : int =
    let tenx = 10n * x in
    if tenx < 7n * y or tenx > 15n * y then
        (failwith "Log out of bounds" : int)
    else
        let x_plus_y = x + y in
        let num : int = 60003 * (x - y) * (int x_plus_y) in
        let denom = 2n * (x_plus_y * x_plus_y + 2n * x * y) in
        num / (int denom)


let assert_nat (x : int) : nat =
    match is_nat x with
    | None -> (failwith "x should be positive" : nat)
    | Some n -> n

let half_bps_pow (tick : int) : nat=
    (* let leading = Bitwise.shift_right tick 8n;
    let approx = Map.get leading lookup; *)
    (failwith "not implemented" : nat)


(* tick is going to be between -2^12 and 2^12,
   if we shift the tick by 8, we can have 16 possible
   values, we can look them up directly, the rest
   is less than 2^8 - 1, and (exp(0.0005)^(255)) = 1.01283
   so a single refinement through a taylor approximation gives us an
   excellent result *)


(*

2^0 :  {38687560557337355742483221,-85}
2^1 :  {38689494983725479307861971,-85}
2^2 :  {38693364126677775184793561,-85}
2^3 :  {38701103573421987005215721,-85}
2^4 :  {38716587111352494729706462,-85}
2^5 :  {38747572773653928660613512,-85}
2^6 :  {38809618513447185627569983,-85}
2^7 :  {38934008210058939100663682,-85}
2^8 :  {39183984934869404935943141,-85}
2^9 :  {39688763633815974521145659,-85}
2^10:  {40717912888646086984030507,-85}
2^11:  {42856962434838368098529959,-85}
2^12:  {47478079282778087338933597,-85}
2^13:  {29134438707490415855866100,-84}
2^14:  {43882733799120415566608322,-84}
2^15:  {49778031622173924435819796,-83}
2^16:  {32025492072892644517427309,-80}
2^17:  {53023938993515524338629870,-76}
2^18:  {36338278329035183585718600,-66}
2^19:  {34133361681864713959105863,-47}
2^20:  {30116777038798852995368017,-9} = 30116777038798852995368017 * 2^(-9) * (1 + err) with |err| < 2^(-86)


In the worst case, the product of all the mantissas is

264282414466372656233620232085\
891076152819178199188386987876\
130399611230776317562564102356\
510887554505841649869995115306\
153657257592287884078198212867\
452334369951588805003086042064\
419487861191053453237580032868\
756342299942389805587432570501\
862667904215683136301357731738\
924376078014888356296733854672\
700413177506511695535173325976\
383558174492550937991343710044\
641323722927702345262447930316\
774974009739628156118404725209\
505333623465138071036374956137\
115703347618301958744836243752\
685553646224208937741458987598\
9769554995549619185305600000

A 1786 bit number.

Also in the worse case, the product of all the error terms is (1 + total_err) with |total_err| < 2^(-81)
This ensures that the tick index can be matched to a square root price with over 80 bits of precision

*)

