import math
from utils import get_input

inp = get_input(2019, 3)

lines = inp.split("\n")

wire1 = lines[0].split(",")
wire2 = lines[1].split(",")

def wire_positions(wire):
    locs = [(0,0)]
    pos = (0,0)

    for move in wire:
        d = move[0]
        l = int(move[1:])

        x, y = pos
        if d == "R":
            new = [(x+a+1,y) for a in range(l)]
        elif d == "L":
            new = [(x-a-1,y) for a in range(l)]
        elif d == "U":
            new = [(x,y+a+1) for a in range(l)]
        elif d == "D":
            new = [(x,y-a-1) for a in range(l)]

        locs += new
        pos = new[-1]

    return locs

def man_dist(pos):
    x, y = pos
    return abs(x) + abs(y)


pos1 = set(wire_positions(wire1))
print(len(pos1))
pos2 = set(wire_positions(wire2))
print(len(pos2))

intersections = [p for p in pos1 if p in pos2]

print(len(intersections))
print(intersections)

p = None
d = 10000000
for pos in intersections:
    if man_dist(pos) < d and pos != (0,0):
        d = man_dist(pos)
        p = pos

print(d)


def wire_positionsb(wire):
    locs = {}
    pos = (0,0)
    steps = 0

    for move in wire:
        d = move[0]
        l = int(move[1:])

        x, y = pos
        if d == "R":
            new = [(x+a+1,y) for a in range(l)]
        elif d == "L":
            new = [(x-a-1,y) for a in range(l)]
        elif d == "U":
            new = [(x,y+a+1) for a in range(l)]
        elif d == "D":
            new = [(x,y-a-1) for a in range(l)]

        for ix, p in enumerate(new):
            if p not in locs:
                locs[p] = steps + ix + 1

        steps += len(new)

        pos = new[-1]

    return locs


dists1 = wire_positionsb(wire1)
dists2 = wire_positionsb(wire2)

intersects = {}
for p, d in dists1.items():
    if p in dists2:
        intersects[p] = d + dists2[p]

print(intersects)

D = 10000000
P = None
for p, d in intersects.items():
    if d < D:
        D = d
        P = p

print(D)

