from utils import *

inp = get_input(2021, 2)

class Submarine:
    def __init__(self):
        self.x = 0
        self.depth = 0
        self.aim = 0

    def forward(self, n):
        self.x += n
        self.depth += self.aim * n

    def up(self, n):
        self.aim -= n

    def down(self, n):
        self.aim += n

sub = Submarine()

for line in inp.split("\n"):
    cmd, rawVal = line.split()
    getattr(sub, cmd)(int(rawVal))

print(sub.x * sub.depth)

