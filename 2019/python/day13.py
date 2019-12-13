from collections import defaultdict
from utils import *
from intcode import VM

inp = get_input(2019, 13)

print("Start of input:")
print(inp[:500])

screen = defaultdict(int)

code = [int(c) for c in inp.split(",")]
vm = VM(code)

vm.memory[0] = 2
vm.input = lambda: None
vm.output = None
score = 0


class OneValue():
    def __init__(self, v):
        self.v = v

    def get(self):
        c = self.v
        self.v = None
        return c

while not vm.halted:
    vm.run()
    if vm.waiting is not None:
        print("Score: ", score)
        #print_grid_dict(screen, { 0: "  ", 1: "##", 2: "[]", 3: "==", 4: "@@" })
        pp = [pos for pos, c in screen.items() if c == 3][0][0]
        bp = [pos for pos, c in screen.items() if c == 4][0][0]

        if pp > bp:
            vm.input = OneValue(-1).get
        elif bp > pp:
            vm.input = OneValue(1).get
        else:
            vm.input = OneValue(0).get
    else:
        x = vm.out_val
        vm.run()
        y = vm.out_val
        vm.run()
        c = vm.out_val

    if x == -1 and y == 0:
        score = c
    else:
        screen[(x, y)] = c

print("HALTED")
print("Score: ", score)
print(len([c for p, c in screen.items() if c == 2]))


