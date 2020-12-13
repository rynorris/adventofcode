from utils import *

inp = get_input(2020, 12)

actions = [(l[0], int(l[1:])) for l in inp.split("\n")]

dirs = {
    "N": (0, -1),
    "S": (0, 1),
    "E": (1, 0),
    "W": (-1, 0),
}

DIR_ORDER = ["N", "E", "S", "W"]

class Ship():
    def __init__(self):
        self.facing = 1  # 1 = East
        self.x = 0
        self.y = 0

    def do(self, inst):
        action, value = inst
        if action in ["L", "R"]:
            self.turn(action, value)
        else:
            self.move(action, value)

    def move(self, d, l):
        if d in dirs:
            dx, dy = dirs[d]
            self.x += dx * l
            self.y += dy * l
        elif d == "F":
            dx, dy = dirs[DIR_ORDER[self.facing]]
            self.x += dx * l
            self.y += dy * l

    def turn(self, d, amt):
        steps = amt // 90  # GUESS
        if d == "L":
            steps *= -1

        self.facing = (self.facing + steps) % 4


class Ship2():
    def __init__(self):
        self.x = 0
        self.y = 0
        self.wpx = 10
        self.wpy = -1

    def do(self, inst):
        action, value = inst
        if action in ["L", "R"]:
            self.rot(action, value)
        elif action in ["N", "S", "E", "W"]:
            self.move_wp(action, value)
        else:
            self.move(value)

    def move(self, value):
        self.x += self.wpx * value
        self.y += self.wpy * value

    def move_wp(self, action, value):
        dx, dy = dirs[action]
        self.wpx += dx * value
        self.wpy += dy * value

    def rot(self, action, value):
        steps = value // 90  # GUESS
        if action == "L":
            steps *= -1
        steps = steps % 4
        if steps == 0:
            y = self.wpy
            x = self.wpx
        elif steps == 1:
            y = self.wpx
            x = -self.wpy
        elif steps == 2:
            y = -self.wpy
            x = -self.wpx
        elif steps == 3:
            y = -self.wpx
            x = self.wpy
        self.wpx = x
        self.wpy = y


s = Ship2()
for inst in actions:
    s.do(inst)
    print(inst, s.x, s.y, s.wpx, s.wpy)

print(abs(s.x) + abs(s.y))
