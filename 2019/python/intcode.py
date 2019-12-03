

class VM:
    def __init__(self, code):
        self.memory = [x for x in code]
        self.pc = 0
        self.halted = False
        self.debug = False


        self.OPS = {
            1: (self.__add, 3),
            2: (self.__mul, 3),
            99: (self.__halt, 0),
        }

    def step(self):
        opcode = self.memory[self.pc]
        if opcode not in self.OPS:
            raise Exception(f"Invalid opcode: {opcode}")

        (op, nargs) = self.OPS[opcode]
        args = [self.memory[self.pc + x + 1] for x in range(nargs)]
        op(self, *args)
        self.pc += nargs + 1

    def run(self):
        while not self.halted:
            self.step()

    def _debug(self, msg):
        if self.debug:
            print(msg)

    @staticmethod
    def __halt(vm):
        vm._debug("HALT")
        vm.halted = True

    @staticmethod
    def __add(vm, a1, a2, a3):
        vm._debug(f"ADD [{a1:3}] [{a2:3}] {a3:3}")
        vm.memory[a3] = vm.memory[a1] + vm.memory[a2]

    @staticmethod
    def __mul(vm, a1, a2, a3):
        vm._debug(f"MUL [{a1:3}] [{a2:3}] {a3:3}")
        vm.memory[a3] = vm.memory[a1] * vm.memory[a2]

