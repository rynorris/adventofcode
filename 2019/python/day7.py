import itertools
from utils import get_input

from intcode import VM

inp = get_input(2019, 7)

CODE = [int(c) for c in inp.split(",")]

class Input():
    def __init__(self, vals):
        self.vals = vals

    def get_input(self):
        if self.vals:
            x = self.vals[0]
            self.vals = self.vals[1:]
            return x
        else:
            return None

class Output():
    def __init__(self):
        self.val = 0

    def give_output(self, val):
        self.val = val


def run(code, settings):
    vms = []
    inputs = []
    outputs = []
    for s in settings:
        vm = VM(code)
        i = Input([s])
        o = Output()
        vm.input = i.get_input
        vm.output = o.give_output
        vm.run()
        vms.append(vm)
        inputs.append(i)
        outputs.append(o)

    inputs[0].vals = [0]
    while True:
        for ix in range(len(vms)):
            vms[ix].run()
            o = outputs[ix].val
            inputs[(ix + 1)  % len(inputs)].vals = [o]

        if all([vm.halted for vm in vms]):
            break

    return outputs[-1].val


test_settings = [4,3,2,1,0]
test_code = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]



MAX = 0
BEST = []
for setting in itertools.permutations([5,6,7,8,9]):
    val = run(CODE, setting)
    print(val, setting)
    if val > MAX:
        BEST = setting
        MAX = val

print(MAX, BEST)
