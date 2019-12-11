import sys
from collections import defaultdict
from utils import get_input
from intcode import VM

inp = get_input(2019, 11)
code = [int(c) for c in inp.split(",")]

class Robot():
    def __init__(self):
        self.p = (0, 0)
        self.v = (0, 1)
        self.colors = defaultdict(int)
        self.input_buf = []

        self.cpu = VM(code)
        self.cpu.output = None
        self.cpu.input = self.give_input

    def recv_output(self, v):
        self.input_buf.append(v)

    def give_input(self):
        return self.colors[self.p]

    def step(self):
        if self.cpu.halted:
            raise Exception("DONE")
        self.cpu.run()
        color = self.cpu.out_val
        self.cpu.run()
        turn = self.cpu.out_val

        self.colors[self.p] = color
        if turn == 0:
            if self.v == (0, 1):
                self.v = (-1, 0)
            elif self.v == (-1, 0):
                self.v = (0, -1)
            elif self.v == (0, -1):
                self.v = (1, 0)
            elif self.v == (1, 0):
                self.v = (0, 1)
        elif turn == 1:
            if self.v == (0, 1):
                self.v = (1, 0)
            elif self.v == (1, 0):
                self.v = (0, -1)
            elif self.v == (0, -1):
                self.v = (-1, 0)
            elif self.v == (-1, 0):
                self.v = (0, 1)

        v1, v2 = self.v
        x, y = self.p
        self.p = (x+v1, y+v2)


R = Robot()
R.colors[(0, 0)] = 1
for ix in range(10000):
    num_white = len([p for p, c in R.colors.items() if c == 1])
    painted = len(R.colors)
    print(f"{ix}: {painted}")
    try:
        R.step()
    except:
        break

for y in range(-50, 50):
    for x in range(-50, 50):
        if R.colors[(x, -y)] == 1:
            sys.stdout.write("#")
        else:
            sys.stdout.write(" ")
    sys.stdout.write("\n")


