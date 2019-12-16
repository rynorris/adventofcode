import math
from utils import *
import itertools

inp = get_input(2019, 16)
#inp = "03081770884921959731165446850517"

nums = [int(c) for c in inp.strip()]

L = len(inp)

def lcm(a, b):
    return abs(a*b) // math.gcd(a, b)

def digit(ns, pat, ix):
    dig = 0

    pat = [x for c in pat for x in [c] * ix]
    pat = itertools.cycle(pat)
    next(pat)

    for x, y in zip(ns, pat):
        dig += x*y

    return abs(dig) % 10

def phase(ns, pat):
    out = []
    for ix in range(len(ns)):
        dig = digit(ns, pat, ix+1)
        out.append(dig)

    return out

PAT = [0,1,0,-1]
sig = [n for n in nums] * 10000
siglen = len(sig)



def digit2(ns, dig, sums):
    # First we would skip ix-1 digits.
    ix = dig-1
    # Then alternate between adding, ignoring and subtracting the partial sums.
    phase = 1
    final = 0
    while ix < siglen:
        top = ix + dig
        if top >= siglen:
            top = siglen

        psum = sums[top] - sums[ix]
        if phase == 1:
            final += psum
        elif phase == 3:
            final -= psum
        phase = (phase + 1) % 4
        ix += dig

    return abs(final) % 10

def phase2(ns):
    out = []
    # Partial sums.
    sums = []
    s = 0
    sums.append(0)
    for n in ns:
        s += n
        sums.append(s)
    for ix in range(len(ns)):
        dig = digit2(ns, ix+1, sums)
        out.append(dig)

    return out


offset = int(inp[:7])
for ix in range(100):
    sig = phase2(sig)
    print(ix, sig[:8])

print(sig[offset-8:offset])
print(sig[offset:offset+8])
print(sig[offset+8:offset+16])
