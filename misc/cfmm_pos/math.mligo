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


let positive_ladder = [
    {v=38687560557337355742483221;offset=-85}; (* 2^0 *)
    {v=38689494983725479307861971;offset=-85};
    {v=38693364126677775184793561;offset=-85};
    {v=38701103573421987005215721;offset=-85};
    {v=38716587111352494729706462;offset=-85};
    {v=38747572773653928660613512;offset=-85};
    {v=38809618513447185627569983;offset=-85};
    {v=38934008210058939100663682;offset=-85};
    {v=39183984934869404935943141;offset=-85};
    {v=39688763633815974521145659;offset=-85};
    {v=40717912888646086984030507;offset=-85};
    {v=42856962434838368098529959;offset=-85};
    {v=47478079282778087338933597;offset=-85};
    {v=29134438707490415855866100;offset=-84};
    {v=43882733799120415566608322;offset=-84};
    {v=49778031622173924435819796;offset=-83};
    {v=32025492072892644517427309;offset=-80};
    {v=53023938993515524338629870;offset=-76};
    {v=36338278329035183585718600;offset=-66};
    {v=34133361681864713959105863;offset=-47};
    {v=30116777038798852995368017;offset=-9}] (* 2^20 *)

let negative_ladder = [
    {v=19341845997356488514015570;offset=-84}; (* -2^0 *)
    {v=2417609866154190654524678;offset=-81};
    {v=38677889876083546261210550;offset=-85};
    {v=38670155071614559132217310;offset=-85};
    {v=19327345051392939314248854;offset=-84};
    {v=19311889358453304431405214;offset=-84};
    {v=77124060166079386301517011;offset=-86};
    {v=38438828813936263312862610;offset=-85};
    {v=76387211720013513967242610;offset=-86};
    {v=75415686436335201065707301;offset=-86};
    {v=73509547540888574991368714;offset=-86};
    {v=17460146398643019245576278;offset=-84};
    {v=126085780994910985395717054;offset=-87};
    {v=102735988268212419722671870;offset=-87};
    {v=68208042073114503830679361;offset=-87};
    {v=60130046442422405275353178;offset=-88};
    {v=11682706336100247487260846;offset=-88};
    {v=56449132412055094618915006;offset=-95};
    {v=20592303012757789234393034;offset=-103};
    {v=1370156647050591448120178;offset=-118};
    {v=24846245577653162038756966;offset=-160}] (* -2^20 *)

let half_bps_pow (tick : nat) : nat=
    pass

let rec half_bps_pow_rec (tick, acc, ladder) : nat * nat) : nat=
    if tick = 0n then
        acc
    else
        let b = tick mod 2n in
        match ladder with
        | [] -> acc
        | h :: t -> half_bps_pow_rec (tick / 2, {v = h.v * acc.v  ; offset = h.offset + acc.offset}, t)


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

