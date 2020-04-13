// Fraction type
type fraction = {num : int, den : nat};
type storage = fraction;
type parameter = fraction;

// Compute a^n by repeated squaring
let pow = (a : nat, n : nat) : nat => {    
    let rec pow_aux = ((base, accu, n): (nat, nat, nat)): nat  => 
        if (n > 0n) {
            if (n mod 2n == 1n) {
                pow_aux((base * base, accu * base, n / 2n));
            } else {
                pow_aux((base * base, accu, n / 2n));
            };
        } else {
            accu
        };
    pow_aux((a, 1n, n));    
};

let exp = (x : fraction) : fraction => {
    // log(2) ~ 25469 / 36744
    let log2 : fraction = {num: 25469, den: 36744n};
    let r : int =  int(log2.den) * x.num;
    let s : nat =  abs(log2.num) * x.den;
    let ab = {num: r / int(s), den: r mod s};
    let (i, f) = 
        if (2n * ab.den <= s) { 
            (ab.num, {num: int(ab.den), den: log2.den * x.den}); 
        } 
        else {
            (ab.num + 1, {num: int(ab.den) - int(s), den: log2.den * x.den});
        };    
    let pow : nat = pow(2n, abs(i));
    let tp = if (i > 0) {{num:int(pow), den:1n};} else {{num:1, den:pow};};
    let k_u2_12v2 : int = f.num * f.num + int(12n * f.den * f.den);
    let k_6_uv : int    = 6 * f.num * int(f.den);
    let num : int =  (k_u2_12v2 + k_6_uv) * tp.num;
    let den : int =  (k_u2_12v2 - k_6_uv) * int(tp.den);
    if (den < 0)
    {
        {num:-num, den:abs(den)};
    } else {
        {num:num, den:abs(den)};
    };
};

let main = ((p, s) : (parameter, storage)):(list(operation), storage) =>
{
    let r = exp(p);
    ([]: list(operation), r)
};
