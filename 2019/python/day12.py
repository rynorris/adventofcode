from utils import *

inp = get_input(2019, 12)

print("Start of input:\n")
print(inp[:500])

lines = inp.split("\n")

def parse_pos(line):
    line = line.replace("<", "")
    line = line.replace(">", "")
    line = line.replace("=", "")
    line = line.replace("x", "")
    line = line.replace("y", "")
    line = line.replace("z", "")
    return [int(x.strip()) for x in line.split(",")]

INIT_LOCS = [parse_pos(l) for l in lines if l != ""]
INIT_VELS = [[0, 0, 0], [0,0,0],[0,0,0],[0,0,0]]

def adjust_vels(locs, vels):
    for ix in range(len(locs)):
        for jx in range(len(locs)):
            if jx == ix:
                continue
            for k in range(3):
                if locs[jx][k] > locs[ix][k]:
                    vels[ix][k] += 1
                elif locs[jx][k]< locs[ix][k]:
                    vels[ix][k] -= 1

    return locs, vels


def move(locs, vels):
    for ix in range(len(locs)):
        for k in range(3):
            locs[ix][k] += vels[ix][k]

    return locs, vels


def pe(loc):
    return sum([abs(x) for x in loc])

def ke(vel):
    return sum([abs(x) for x in vel])


def energy(loc, vel):
    return pe(loc) * ke(vel)


def total_energy(locs, vels):
    tot = 0
    for ix in range(len(locs)):
        tot += energy(locs[ix], vels[ix])

    return tot




import copy
def freq(coord):
    L = copy.deepcopy(INIT_LOCS)
    #L = [[-1, 0, 2], [2, -10, -7], [4, -8, 8], [3, 5, -1]]
    V = [[0, 0, 0], [0,0,0],[0,0,0],[0,0,0]]
    seen = {}
    ix = 0
    print("STARTING", ix, seen)
    print(L, V)
    while True:
        L, V = adjust_vels(L, V)
        L, V = move(L, V)

        X = str([l[coord] for l in L]) + str([v[coord] for v in V])

        if X in seen:
            print(ix, X, seen[X])
            return ix - seen[X]

        seen[X] = ix
        if ix % 10 == 0 and ix < 100:
            print(L, V)
        ix += 1

mX = freq(0)
mY = freq(1)
mZ = freq(2)

print(mX, mY, mZ)


import math

def lcm(a, b):
    return abs(a*b) // math.gcd(a, b)

lcm1 = lcm(mX, mY)
print(lcm(lcm1, mZ))
