from utils import get_input

from intcode import VM

inp = get_input(2019, 9)

code = [int(c) for c in inp.split(",")]

vm = VM(code)
vm.run()
