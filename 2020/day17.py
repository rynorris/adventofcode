from utils import *
import itertools

inp = get_input(2020, 17)

state = {}
for x, row in enumerate(inp.split("\n")):
    for y, c in enumerate(row):
        state[(x, y, 0, 0)] = c


def neighbours(x, y, z, w):
    return [(x+dx, y+dy, z+dz, w+dw) for dx, dy, dz, dw in itertools.product([-1, 0,1],[-1, 0,1],[-1, 0,1], [-1,0,1]) if not dx == dy == dz == dw == 0]


def step(state):
    new = {}

    allcoords = set([c for x, y, z, w in state.keys() for c in neighbours(x, y, z, w)])
    for x, y, z, w in allcoords:
        c = state.get((x, y, z, w), ".")
        num_active = len([1 for c in neighbours(x, y, z, w) if state.get(c, ".") == "#"])
        if c == "#" and num_active in [2, 3]:
            new[(x, y, z, w)] = "#"
        elif c == "." and num_active == 3:
            new[(x, y, z, w)] = "#"

    return new

for ix in range(6):
    state = step(state)
    print(len([c for c in state.values() if c == "#"]))

