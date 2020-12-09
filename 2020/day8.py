from utils import *

inp = get_input(2020, 8)

def parse_instruction(l):
    parts = l.split()
    return (parts[0], int(parts[1]))

orig_instructions = [parse_instruction(l) for l in inp.split("\n")]

def test(ix):
    acc = 0
    ptr = 0
    instructions = [l for l in orig_instructions]
    if instructions[ix][0] == "acc":
        return False
    instructions[ix] = ("jmp" if instructions[ix][0] == "nop" else "nop", instructions[ix][1])

    seen = set()

    while True:
        if ptr in seen:
            return False

        seen.add(ptr)
        if ptr >= len(instructions):
            break
        ins = instructions[ptr]
        if ins[0] == "acc":
            acc += ins[1]
            ptr += 1
        elif ins[0] == "jmp":
            ptr += ins[1]
        else:
            ptr += 1

    print(acc)
    return True

for ix in range(len(orig_instructions)):
    test(ix)
