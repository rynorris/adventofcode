from utils import get_input

import intcode

inp = get_input(2019, 5)

code = [int(c) for c in inp.split(",")]

vm = intcode.VM(code)
vm.debug = True

vm.run()
