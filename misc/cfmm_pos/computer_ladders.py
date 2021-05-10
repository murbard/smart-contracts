from sage.all import *

b_range = range(Integer(0), Integer(20)) # powers 2^0 to 2^19

precision_desired = Integer(90)
precision_per_rung = Integer(166) # adjust until check_err_product passes
# 165 is not enough
# 166 works

class Fixed(object):
    def __init__(self, v=0, offset=0):
        self.v = v
        self.offset = offset
    def __repr__(self):
        return f"{{v={self.v}; offset={self.offset}}}"
    def __mul__(self, other):
        return Fixed(self.v * other.v, self.offset + other.offset)

# check the worst case scenario for precision loss under pessimistic assumptions
def check_err_product(err_product):
    print("------")
    print(((err_product - 1)*2**(precision_desired+1)).n(10000), " should be < 1 ...")
    assert((err_product - 1).n(10000) < 2**(-precision_desired-1))
    print("------")
    return

def half_bps_exact_pow(tick):
    return exp(Integer(5)/Integer(100000)*tick)

# positive ladder

positive_ladder = []
err_product = Integer(1)
for b in b_range:
    x = half_bps_exact_pow(Integer(2)**b)
    for offset in range(-Integer(300), Integer(300)):
        v = round(x * Integer(2)**offset)
        err = abs(v / (x * Integer(2)**offset) - Integer(1))
        if (Integer(2)**precision_per_rung * err).n(digits=10000) < Integer(1)/Integer(2):
            positive_ladder.append(Fixed(v,offset))
            break
    err_product *= (Integer(1) + err)

print("positive_ladder = [")
for rung in positive_ladder:
    print(rung,";")
print("]")

check_err_product(err_product)

# negative ladder

negative_ladder = []
err_product = Integer(1)
for b in b_range:
    x = x = half_bps_exact_pow(-Integer(2)**b)
    for offset in range(-Integer(300), Integer(300)):
        v = round(x * Integer(2)**offset)
        err = abs(v / (x * Integer(2)**offset) - 1)
        if (Integer(2)**precision_per_rung * err).n(digits=10000) < Integer(1)/Integer(2):
            negative_ladder.append(Fixed(v,offset))
            break
    err_product *= (Integer(1) + err)

print("negative_ladder = [")
for rung in negative_ladder:
    print(rung,";")
print("]")

check_err_product(err_product)

def half_bps_pow_rec (tick, acc, ladder):
    if tick == 0:
        return acc
    else:
        (half, rem) = (tick // 2, tick - 2 * (tick // 2))
        return half_bps_pow_rec(half, acc if rem == 0 else acc * ladder[0], ladder[1:])


def half_bps_pow (tick):
    product = half_bps_pow_rec (abs(tick), Fixed(v=1,offset=0), (positive_ladder if tick > 0 else negative_ladder))
    doffset = precision_desired - product.offset
    if doffset > 0:
        return  product.v << (doffset)
    else:
        # This branch should almost never happen, in general the price we get is not a round number.
        return product.v >> (-doffset)

# run model checks
last_a = 10**1000
for tick in list(range(-2**20+1,2**20))[-1::-1]:
    if (2**20 + tick) % 40000 == 0:
        print("%.2f%%" % (100 * (tick + 2**20) / 2**21))
    try:
        a = half_bps_pow(tick)
        b = round(half_bps_exact_pow(tick) * (Integer(2)**Integer(precision_desired)))
        assert (abs(a-b) <= 1) # we're off by at most 1
        assert (a < last_a) # the sequence is strictly decreasing
    except Exception as e:
        print(tick,a,b)
        print(e)
        exit()
    last_a = a

# -194032
# 49086