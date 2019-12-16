from utils import *
from intcode import VM

inp = get_input(2019, 15)
code = [int(c) for c in inp.split(",")]

grid = {(0,0): 3}


vm = VM(code)
vm.output = None

pos = (0,0)

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

def reverse(m):
    if m == 1:
        return 2
    elif m == 2:
        return 1
    elif m == 3:
        return 4
    elif m == 4:
        return 3

class OneValue():
    def __init__(self, v):
        self.v = v

    def get(self):
        c = self.v
        self.v = None
        return c

moves = []
path = []
dist = 10000
OXY = None
while True:
    BACK = False
    # Choose move.
    if up(pos) not in grid:
        move = 1
        tgt = up(pos)
    elif down(pos) not in grid:
        move = 2
        tgt = down(pos)
    elif left(pos) not in grid:
        move = 3
        tgt = left(pos)
    elif right(pos) not in grid:
        move = 4
        tgt = right(pos)
    else:
        # Backtrack
        if moves:
            BACK=True
            move = reverse(moves.pop())
            path.pop()
            if path:
                tgt = path[-1]
            else:
                break
        else:
            break

    # Send move.
    I = OneValue(move)
    vm.input = I.get

    # Get Output.
    vm.run()
    out = vm.out_val

    # Update map
    if out == 0:
        grid[tgt] = 0
        if BACK:
            raise Exception("Hit wall on backtrack")
    elif out == 1:
        grid[tgt] = 1
        if not BACK:
            moves.append(move)
            path.append(tgt)
        pos = tgt
    else:
        grid[tgt] = 2
        OXY = tgt
        if not BACK:
            moves.append(move)
            path.append(tgt)
        pos = tgt
        if len(path) < dist:
            dist = len(path)
            print(dist)
    #print_grid_dict(grid, charmap={0:"##", 1: "..", 2: "@@", 3: "00", 5: "  "}, default=5)

print("Got map")
print_grid_dict(grid, charmap={0:"##", 1: "..", 2: "@@", 3: "00", 5: "  "}, default=5)
print(dist)


t = 0
boundary = [OXY]
filled = set()

while True:
    ns = set([n for b in boundary for n in [up(b), down(b), left(b), right(b)]])
    to_fill = []
    for n in ns:
        if n in boundary:
            continue
        if n in filled:
            continue

        if n not in grid or grid[n] not in [1, 3]:
            continue

        to_fill.append(n)
        grid[n] = 2

    if to_fill:
        boundary = to_fill
        t += 1
        print(t)
    else:
        break

    print(to_fill)
    print_grid_dict(grid, charmap={0:"##", 1: "..", 2: "@@", 3: "00", 5: "  "}, default=5)



