import math
from utils import *

inp = get_input(2019, 14)

ginp ="""
171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX
"""

reactions = []
for line in inp.split("\n"):
    spl = line.split("=>")
    if len(spl) != 2:
        continue
    rhs = spl[1].strip().split()
    out = (int(rhs[0]), rhs[1])
    ins = []
    for x in spl[0].strip().split(", "):
        ins.append((int(x.split()[0]), x.split()[1]))

    reactions.append((ins, out))


total_ore = 0
total_fuel = 0
extra = {}
import sys
req = {"FUEL": int(sys.argv[1])}

while True:
    if len(req) == 1 and "ORE" in req:
        print(req["ORE"])
        if req["ORE"] > 1000000000000:
            print("TOO HIGH")
        else:
            print("TOO LOW")
        break

    t = [x for x in list(req.keys()) if x != "ORE"][0]
    q = req[t]
    if t in extra:
        ex = extra[t]
        if ex < q:
            q -= ex
            extra[t] = 0
        elif ex >= q:
            extra[t] -= q
            del req[t]
            continue

    react = [r for r in reactions if r[1][1] == t][0]
    times = math.ceil(q / react[1][0])
    for iq, it in react[0]:
        if it not in req:
            req[it] = 0
        req[it] += iq * times

    made = times * react[1][0]
    if made > q:
        if t not in extra:
            extra[t] = 0
        extra[t] += made - q
    del req[t]


print(extra)
