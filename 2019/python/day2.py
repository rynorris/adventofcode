import sys

from utils import get_input

import intcode

inp = get_input(2019, 2)
code = [int(x) for x in inp.split(",")]

def part_a():
    vm = intcode.VM(code)
    vm.debug = True
    vm.memory[1] = 12
    vm.memory[2] = 2
    vm.run()
    print(f"Answer is: {vm.memory[0]}")


def part_b():
    guess = 0
    while True:
        n = guess // 99
        v = guess % 99

        vm = intcode.VM(code)
        vm.memory[1] = n
        vm.memory[2] = v
        vm.run()

        if vm.memory[0] == 19690720:
            print(f"Answer is: {n} * 100 + {v} = {n * 100 + v}")
            break
        else:
            guess += 1



if __name__ == "__main__":
    if sys.argv[1] == "a":
        part_a()
    elif sys.argv[1] == "b":
        part_b()
    else:
        print("Please enter a or b")

