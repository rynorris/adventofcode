from utils import *
from intcode import VM

inp = get_input(2019, 17)
print(inp[:50])

code = [int(c) for c in inp.split(",")]

class OneValue():
    def __init__(self, v):
        self.v = v

    def get(self):
        c = self.v
        self.v = None
        return c


class Lister():
    def __init__(self, vals):
        self.vals = vals

    def get(self):
        if not self.vals:
            return None
        c = self.vals[0]
        self.vals = self.vals[1:]
        return c

code[0] = 2
vm = VM(code)
vm.input = lambda: None
vm.output = None

"""
R 4 L 12 L 8 R 4 (A)

L 8 R 10 R 10 R 6 (C)

R 4 L 12 L 8 R 4 (A)

R 4 R 10 L 12 (B)

R 4 L 12 L 8 R 4 (A)

L 8 R 10 R 10 R 6 (C)

R 4 L 12 L 8 R 4 (A)

R 4 R 10 L 12 (B)

L 8 R 10 R 10 R 6 (C)

R 4 R 10 L 12 (B)
"""

main = "A,C,A,B,A,C,A,B,C,B"
A = "R,4,L,12,L,8,R,4"
B = "R,4,R,10,L,12"
C = "L,8,R,10,R,10,R,6"


grid = {}

x = 0
y = 0
X = 0
Y = 0

invals = []
for c in main:
    invals.append(ord(c))
invals.append(10)
for c in A:
    invals.append(ord(c))
invals.append(10)
for c in B:
    invals.append(ord(c))
invals.append(10)
for c in C:
    invals.append(ord(c))
invals.append(10)
invals.append(ord("n"))
invals.append(10)


lister = Lister(invals)
vm.input = lister.get
while True:
    vm.run()
    if vm.halted:
        break
    v = vm.out_val
    if v > 50 and v < 255:
        print(chr(v))
    elif v >= 255:
        print(v)


#print_grid_dict(grid, charmap={"#": "#", ".":".", "^": "^", " ":" "}, default=" ")


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

"""

ans = 0
print(X, Y)
for x in range(X):
    for y in range(Y):
        pt = (x, y)
        if pt not in grid or up(pt) not in grid or down(pt) not in grid or left(pt) not in grid or right(pt) not in grid:
            continue
        if grid[pt] == "#" and grid[up(pt)] == "#" and grid[down(pt)] == "#" and grid[left(pt)] == "#" and grid[right(pt)] == "#":
            ans += x * y

print(ans)
"""
