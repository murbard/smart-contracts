#if MATH_MLIGO
#else
#define MATH_MLIGO

type fixed_point = { v : nat ; offset : int }

[@inline] let fixed_point_mul (a : fixed_point) (b : fixed_point) : fixed_point =
    { v = a.v * b.v ; offset = a.offset + b.offset }

let ceildiv (numerator : nat) (denominator : nat) : nat = abs ((- numerator) / (int denominator))
let ceildiv_int (numerator : int) (denominator : int) : int = - ((- numerator) /  denominator)
let floordiv (numerator : nat) (denominator : nat) : nat =  numerator / denominator

(* accurate for x/y in [0.7, 1.5] *)
(* Note, for simplify, our sqrt_prices are not on a grid of 0.5 bps, they are on a grid of 10000 (Exp[0.0005] - 1) bps *)
let floor_log_half_bps ((x, y) : nat * nat) : int =
    let tenx = 10n * x in
    if tenx < 7n * y or tenx > 15n * y then
        (failwith "Log out of bounds" : int)
    else
        let x_plus_y = x + y in
        let num : int = 60003 * (x - y) * (int x_plus_y) in
        let denom = 2n * (x_plus_y * x_plus_y + 2n * x * y) in
        num / (int denom)

let shift_int (x : int) (n : int): int =
    (if x < 0 then -1 else 1) * (int (if n > 0 then Bitwise.shift_left (abs x) (abs n) else Bitwise.shift_right (abs x) (abs n)))


let assert_nat (x : int) : nat =
    match is_nat x with
    | None -> (failwith "x should be positive" : nat)
    | Some n -> n

(* TODO move ladders to a bigmap and load lazily *)
let positive_ladder = [
    {v=38687560557337355742483221n; offset=-85}; (* 2^0 *)
    {v=38689494983725479307861971n; offset=-85};
    {v=38693364126677775184793561n; offset=-85};
    {v=38701103573421987005215721n; offset=-85};
    {v=38716587111352494729706462n; offset=-85};
    {v=38747572773653928660613512n; offset=-85};
    {v=38809618513447185627569983n; offset=-85};
    {v=38934008210058939100663682n; offset=-85};
    {v=39183984934869404935943141n; offset=-85};
    {v=39688763633815974521145659n; offset=-85};
    {v=40717912888646086984030507n; offset=-85};
    {v=42856962434838368098529959n; offset=-85};
    {v=47478079282778087338933597n; offset=-85};
    {v=29134438707490415855866100n; offset=-84};
    {v=43882733799120415566608322n; offset=-84};
    {v=49778031622173924435819796n; offset=-83};
    {v=32025492072892644517427309n; offset=-80};
    {v=53023938993515524338629870n; offset=-76};
    {v=36338278329035183585718600n; offset=-66};
    {v=34133361681864713959105863n; offset=-47}]
    (* {v=30116777038798852995368017;offset=-9}]  2^20  *)

let negative_ladder = [
    {v=19341845997356488514015570n; offset=-84}; (* -2^0 *)
    {v=2417609866154190654524678n; offset=-81};
    {v=38677889876083546261210550n; offset=-85};
    {v=38670155071614559132217310n; offset=-85};
    {v=19327345051392939314248854n; offset=-84};
    {v=19311889358453304431405214n; offset=-84};
    {v=77124060166079386301517011n; offset=-86};
    {v=38438828813936263312862610n; offset=-85};
    {v=76387211720013513967242610n; offset=-86};
    {v=75415686436335201065707301n; offset=-86};
    {v=73509547540888574991368714n; offset=-86};
    {v=17460146398643019245576278n; offset=-84};
    {v=126085780994910985395717054n; offset=-87};
    {v=102735988268212419722671870n; offset=-87};
    {v=68208042073114503830679361n; offset=-87};
    {v=60130046442422405275353178n; offset=-88};
    {v=11682706336100247487260846n; offset=-88};
    {v=56449132412055094618915006n; offset=-95};
    {v=20592303012757789234393034n; offset=-103};
    {v=1370156647050591448120178n; offset=-118}]
    (* {v=24846245577653162038756966;offset=-160} : 2^20 *)

let rec half_bps_pow_rec ((tick, acc, ladder) : nat * fixed_point * (fixed_point list)) : fixed_point =
    if tick = 0n then
        acc
    else
        let (half, rem) = match ediv tick 2n with | None -> (failwith "impossible" : nat * nat) | Some d -> d in
        match ladder with
        | [] -> (failwith "should not reach end of ladder" : fixed_point)
        | h :: t -> half_bps_pow_rec (half, (if rem = 0n then acc else fixed_point_mul h acc), t)

let half_bps_pow (tick : int) : nat =
    let product = half_bps_pow_rec (abs tick, {v=0n;offset=0}, (if tick > 0  then positive_ladder  else negative_ladder)) in
    let doffset = -80 - product.offset in
    if doffset > 0 then
        Bitwise.shift_right product.v (abs doffset)
    else
        (* This branch should almost never happen, in general the price we get is not a round number. *)
        Bitwise.shift_left product.v (abs doffset)


(* ladder explanation

relative error for each ladder element is  2^(-86)
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

#endif