import inspect


class VM:
    def __init__(self, code):
        self.memory = [x for x in code]
        self.pc = 0
        self.halted = False
        self.debug = False


        self.OPS = {
            1: self.__add,
            2: self.__mul,
            3: self.__inp,
            4: self.__out,
            5: self.__jit,
            6: self.__jif,
            7: self.__lt,
            8: self.__eq,
            99: self.__halt,
        }

    def step(self):
        opcode = self.memory[self.pc]
        if self._operation(opcode) not in self.OPS:
            raise Exception(f"Invalid opcode: {opcode}")

        op = self.OPS[self._operation(opcode)]
        modes = self._param_modes(opcode)
        self._call_op(op, modes)

    def run(self):
        while not self.halted:
            self.step()

    def _debug(self, msg):
        if self.debug:
            print(msg)

    def _param_modes(self, opcode):
        mode_string = str(opcode // 100)
        return [c for c in mode_string[::-1]]

    def _operation(self, opcode):
        return opcode % 100

    def _call_op(self, op, modes):
        before_pc = self.pc

        op_args = [a for a in inspect.getargspec(op).args if a != "vm"]
        raw_args = [self.memory[self.pc + x + 1] for x in range(len(op_args))]
        argvals = []
        for ix, a in enumerate(op_args):
            if a.startswith("a"):
                argvals.append(self._arg(raw_args, modes, ix))
            elif a.startswith("o"):
                argvals.append(raw_args[ix])
            else:
                raise Exception(f"Invalid argument name: {a}")

        self._print_op(op, raw_args, argvals, modes)
        op(self, *argvals)

        if self.pc == before_pc:
            self.pc += len(op_args) + 1

    def _print_op(self, op, raw_args, argvals, modes):
        name = op.__name__[2:].upper()
        arg_strings = []
        for ix, raw in enumerate(raw_args):
            mode = modes[ix] if ix < len(modes) else '0'
            if mode == '0':
                arg_strings.append(f"[{raw:4}]")
            elif mode == '1':
                arg_strings.append(f"{raw:6}")
            else:
                raise Exception(f"Unknown param mode: {mode}")

        val_strings = [f"{v}" for v in argvals]

        self._debug(f"{name:4} " + " ".join(arg_strings) + " (" + ", ".join(val_strings) + ")")


    def _arg(self, args, modes, ix):
        mode = modes[ix] if ix < len(modes) else '0'
        if mode == '0':
            return self.memory[args[ix]]
        elif mode == '1':
            return args[ix]
        else:
            raise Exception(f"Unknown param mode: {mode}")

    @staticmethod
    def __halt(vm):
        vm.halted = True

    @staticmethod
    def __add(vm, a1, a2, o):
        vm.memory[o] = a1 + a2

    @staticmethod
    def __mul(vm, a1, a2, o):
        vm.memory[o] = a1 * a2

    @staticmethod
    def __inp(vm, o):
        val = int(input("Enter input: "))
        vm.memory[o] = val

    @staticmethod
    def __out(vm, a1):
        print(a1)

    @staticmethod
    def __jit(vm, a1, a2):
        if a1 != 0:
            vm.pc = a2

    @staticmethod
    def __jif(vm, a1, a2):
        if a1 == 0:
            vm.pc = a2

    @staticmethod
    def __lt(vm, a1, a2, o):
        vm.memory[o] = 1 if a1 < a2 else 0

    @staticmethod
    def __eq(vm, a1, a2, o):
        vm.memory[o] = 1 if a1 == a2 else 0

