import re
from utils import *

inp = get_input(2020, 14)

mask = "X" * 36

def apply(mask, val):
    bits = "{:036b}".format(val)
    final = ""
    for ix, c in enumerate(mask):
        if c == "X":
            final += bits[ix]
        else:
            final += c

    return int(final, 2)

PAT = re.compile(r"mem\[([0-9]+)\] = ([0-9]+)")

mem = {}

for l in inp.split("\n"):
    if l.startswith("mask ="):
        mask = l.split()[2]
        continue

    m = PAT.match(l)
    if not m:
        raise Exception(l)

    addr = int(m.group(1))
    val = int(m.group(2))
    mem[addr] = apply(mask, val)

print(sum(mem.values()))

def addrs(mask, addr):
    def _rec(binmask, binaddr, prefix):
        if not binaddr:
            return [prefix]

        c = binmask[0]
        if c == "0":
            return _rec(binmask[1:], binaddr[1:], prefix + binaddr[0])
        elif c == "1":
            return _rec(binmask[1:], binaddr[1:], prefix + "1")
        else:
            return _rec(binmask[1:], binaddr[1:], prefix + "0") + _rec(binmask[1:], binaddr[1:], prefix + "1")

    bits = "{:036b}".format(addr)
    return _rec(mask, bits, "")

mem = {}

for l in inp.split("\n"):
    if l.startswith("mask ="):
        mask = l.split()[2]
        continue

    m = PAT.match(l)
    if not m:
        raise Exception(l)

    addr = int(m.group(1))
    val = int(m.group(2))
    for a in addrs(mask, addr):
        mem[a] = val

print(sum(mem.values()))

