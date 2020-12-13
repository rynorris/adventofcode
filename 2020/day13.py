from utils import *

inp = get_input(2020, 13)

earliest = int(inp.split("\n")[0])
buses = [int(x) for x in inp.split("\n")[1].split(",") if x != "x"]

def bus_times(bus):
    x = 0
    while True:
        yield x
        x += bus

def first_bus(bus):
    times = bus_times(bus)
    t = next(times)
    while t < earliest:
        t = next(times)

    return t

first = None
best_bus = None
for bus in buses:
    f = first_bus(bus)
    if not first or f < first:
        first = f
        best_bus = bus

print(best_bus * (first - earliest))


# -- Part 2

buses = [int(x) if x != "x" else None for x in inp.split("\n")[1].split(",")]
conditions = [(ix, b) for ix, b in enumerate(buses) if b is not None]

"""
for ix, b in conditions:
    for jx, c in conditions:
        if (ix - jx) % c == 0 and ix != jx:
            print(ix, b, jx, c)
"""

multiplier = 607 * 37 * 23 * 29
T = 0
n = 0
while True:
    T += multiplier
    t = T - 48
    n += 1
    if n % 100 == 0:
        print(n, t)
    if (t + 17) % (449 * 17 * 13 * 19 * 41) != 0:
        continue

    correct = True
    for ix, b in conditions:
        if (t + ix) % b != 0:
            correct = False
            break

    if correct:
        print(t)
        break

