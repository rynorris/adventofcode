import copy
import heapq
from utils import *

inp = get_input(2019, 18)
dinp = """#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################"""
dinp = """########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################"""


def iskey(c):
    return c >= 'a' and c <= 'z'

def isdoor(c):
    return c >= 'A' and c <= 'Z'


MAP = {}
KEYS = {}
DOORS = {}
start = None
for y, line in enumerate(inp.split("\n")):
    for x, c in enumerate(line):
        MAP[(x, y)] = c
        if c == "@":
            start = (x, y)
            MAP[(x, y)] = "."
        if iskey(c):
            KEYS[c] = (x, y)
        if isdoor(c):
            DOORS[c.lower()] = (x, y)

print_grid_dict(MAP, default=" ")

all_keys = [c for c in MAP.values() if c >= 'a' and c <= 'z']
print(all_keys)
print(len(all_keys))

def up(p):
    x, y = p
    return (x, y-1)

def down(p):
    x, y = p
    return (x, y+1)

def left(p):
    x, y = p
    return (x-1, y)

def right(p):
    x, y = p
    return (x+1, y)

def neighbours(p):
    return [up(p), down(p), left(p), right(p)]

def accessible_keys(pos, keys):
    grid = copy.deepcopy(MAP)
    for k in keys:
        grid[KEYS[k]] = "."
        grid[DOORS[k]] = "."

    ks = []
    seen = set()
    boundary = set([pos])
    d = 0
    while True:
        d += 1

        # Unseen neighbours.
        for b in boundary:
            seen.add(b)
        nb = set([n for p in boundary for n in neighbours(p) if n not in seen and n not in boundary])

        # Only passable tiles.
        nb = set([n for n in nb if n in grid and (grid[n] == "." or iskey(grid[n]))])

        ks += [(d, n, grid[n]) for n in nb if iskey(grid[n])]

        if len(nb) == 0:
            return ks
        boundary = nb


def accessible_keys2(pos, keys):
    grid = copy.deepcopy(MAP)
    for k in keys:
        grid[KEYS[k]] = "."
        grid[DOORS[k]] = "."

    ks = []
    seen = set()
    d = 0
    q = [(d, pos)]
    heapq.heapify(q)
    while len(q) > 0:
        pd, p = heapq.heappop(q)
        if p in seen:
            continue
        seen.add(p)
        nb = neighbours(p)
        for n in nb:
            if n in grid and (grid[n] == '#' or isdoor(grid[n])):
                continue
            if iskey(grid[n]):
                ks.append((pd+1, n, grid[n]))
                continue
            heapq.heappush(q, (pd+1, n))
    return ks



def next_states(state):
    _, dist, pos, keys = state
    ks = accessible_keys2(pos, keys)
    ns = []
    for d, p, k in ks:
        nd = dist + d
        nks = keys + [k]
        ns.append((nd, nd, p, nks))
    return ns


# (score, dist, pos, keys)
routes = [(0, 0, start, [])]
heapq.heapify(routes)

def keystring(keys):
    if len(keys) == 0:
        return ""
    if len(keys) == 1:
        return keys[0]

    return "".join(sorted(keys[:-1]) + keys[-1:])

seen = set()

BESTD = 1000000
while True:
    state = heapq.heappop(routes)
    if state[1] >= BESTD:
        continue

    ks = keystring(state[3])
    if ks in seen:
        continue
    seen.add(ks)

    if len(ks) >= 10:
        print(len(state[3]), state)
        print(len(seen))
    if len(state[3]) == len(all_keys):
        print(state)
        if state[1] < BESTD:
            BESTD = state[1]
            print(BESTD)
            break

    ns = next_states(state)
    for s in ns:
        heapq.heappush(routes, s)

