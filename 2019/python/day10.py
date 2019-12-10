import math
from utils import get_input

inp = get_input(2019, 10)

lines = inp.split("\n")

locs = []
for y, line in enumerate(lines):
    for x, c in enumerate(line):
        if c == '#':
            locs.append((x, y))


def get_line(base, other):
    X, Y = other
    x, y = base
    ox = X - x
    oy = Y - y
    gcd = math.gcd(ox, oy)
    mx = ox / gcd
    my = oy / gcd
    return (mx, my), gcd



def count_visible(locs, pt):
    lines = [get_line(pt, p) for p in locs if p != pt]
    uniq = set([grad for (grad, dist) in lines])
    return len(uniq)


C = 0
BEST = None
for pt in locs:
    c = count_visible(locs, pt)
    if c > C:
        C = c
        BEST = pt
        print(C, BEST)


print("DONE")
print(C, BEST)





def angle(grad):
    x, y = grad
    if y == 0:
        if x > 0:
            return math.pi / 2
        else:
            return -(math.pi / 2)
    return math.atan(x/y)

X = 19
Y = 14
STATION = (X, Y)

lines = {}
locs = [p for p in locs if p != STATION]
for pt in locs:
    grad, dist = get_line(STATION, pt)
    if grad not in lines:
        lines[grad] = []

    lines[grad].append(dist)

print(lines)


for grad in lines.keys():
    lines[grad] = sorted(lines[grad])


def fix_angle(g):
    x, y = g
    return math.atan2(-x, y)


grads = sorted(lines.keys(), key=fix_angle)

lix = 0
for ix in range(205):
    line = []
    while len(line) == 0:
        grad = grads[lix % len(grads)]
        line = lines[grad]
        lix += 1

    d = line[0]
    print(f"{ix}: {grad} {d} {angle(grad)} {X + grad[0]*d} {Y + grad[1] *d}")
    lines[grad] = line[1:]

