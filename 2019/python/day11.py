import sys
from collections import defaultdict
from utils import get_input, print_grid_dict
from intcode import VM

inp = get_input(2019, 11)
code = [int(c) for c in inp.split(",")]

class Robot():
    DIRS = [(0, 1), (-1, 0), (0, -1), (1, 0)]

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
        self.cpu.run()
        color = self.cpu.out_val
        self.cpu.run()
        turn = self.cpu.out_val

        if color is not None:
            self.colors[self.p] = color

        dir_ix = self.DIRS.index(self.v)
        if turn == 0:
            self.v = self.DIRS[(dir_ix + 1) % 4]
        elif turn == 1:
            self.v = self.DIRS[(dir_ix - 1) % 4]

        v1, v2 = self.v
        x, y = self.p
        self.p = (x+v1, y+v2)

        if self.cpu.halted:
            raise Exception("DONE")


R = Robot()
R.colors[(0, 0)] = 1
for ix in range(10000):
    painted = len(R.colors)
    print(f"{ix}: {painted}")
    try:
        R.step()
    except:
        break

print_grid_dict(R.colors, charmap={0: "  ", 1: "##"})

