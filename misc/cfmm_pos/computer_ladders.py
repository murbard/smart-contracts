from sage.all import *

b_range = range(Integer(0), Integer(20)) # powers 2^0 to 2^19

precision_per_rung = Integer(86)

# positive ladder
print("positive ladder")
for b in b_range:
    x = exp(Integer(5)/Integer(100000)*(Integer(2)**b))
    for offset in range(-Integer(200), Integer(200)):
        v = round(x * Integer(2)**offset)
        err = abs(v / (x * Integer(2)**offset) - 1)
        if (Integer(2)**precision_per_rung * err).n(digits=10000) < Integer(1)/Integer(2):
            print(b, v, offset)
            break

# negative ladder
print("negative ladder")
for b in b_range:
    x = exp(-Integer(5)/Integer(100000)*(Integer(2)**b))
    for offset in range(-Integer(200), Integer(200)):
        v = round(x * Integer(2)**offset)
        err = abs(v / (x * Integer(2)**offset) - 1)
        if (Integer(2)**precision_per_rung * err).n(digits=10000) < Integer(1)/Integer(2):
            print(b, v, offset)
            break