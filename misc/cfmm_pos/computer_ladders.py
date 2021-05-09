from sage.all import *

b_range = range(Integer(0), Integer(20)) # powers 2^0 to 2^19

precision_desired = Integer(90)
precision_per_rung = Integer(94) # adjust until check_err_product passes

# check the worst case scenario for precision loss under pessimistic assumptions
def check_err_product(err_product):
    print("------")
    print(((err_product - 1)*2**(precision_desired+1)).n(10000), " should be < 1 ...")
    assert((err_product - 1).n(10000) < 2**(-precision_desired-1))
    print("------")
    return

# positive ladder
print("positive ladder")
err_product = Integer(1)
for b in b_range:
    x = exp(Integer(5)/Integer(100000)*(Integer(2)**b))
    for offset in range(-Integer(200), Integer(200)):
        v = round(x * Integer(2)**offset)
        err = abs(v / (x * Integer(2)**offset) - Integer(1))
        if (Integer(2)**precision_per_rung * err).n(digits=10000) < Integer(1)/Integer(2):
            print(b, v, offset)
            break
    err_product *= (Integer(1) + err)

check_err_product(err_product)

# negative ladder
print("negative ladder")
err_product = Integer(1)
for b in b_range:
    x = exp(-Integer(5)/Integer(100000)*(Integer(2)**b))
    for offset in range(-Integer(200), Integer(200)):
        v = round(x * Integer(2)**offset)
        err = abs(v / (x * Integer(2)**offset) - 1)
        if (Integer(2)**precision_per_rung * err).n(digits=10000) < Integer(1)/Integer(2):
            print(b, v, offset)
            break
    err_product *= (Integer(1) + err)

check_err_product(err_product)

# todo, assertions to show that ticks 1-2^20 and 2-2^20 have distinct sqrt_price