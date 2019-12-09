import inspect


class VM:
    def __init__(self, code):
        self.memory = [x for x in code]
        self.memory += [0] * 10000
        self.pc = 0
        self.halted = False
        self.debug = False
        self.input = lambda: input(">> ")
        self.output = print
        self.waiting = None
        self.rel_base = 0


        self.OPS = {
            1: self.__add,
            2: self.__mul,
            3: self.__inp,
            4: self.__out,
            5: self.__jit,
            6: self.__jif,
            7: self.__lt,
            8: self.__eq,
            9: self.__arb,
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
        if self.waiting is not None:
            v = self.waiting
            self.waiting = None
            self.__inp(v)
        while not self.halted and not self.waiting:
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

        op_args = [a for a in inspect.getargspec(op).args if a != "self"]
        raw_args = [self.memory[self.pc + x + 1] for x in range(len(op_args))]
        argvals = []
        for ix, a in enumerate(op_args):
            if a.startswith("a"):
                argvals.append(self._arg(raw_args, modes, ix))
            elif a.startswith("o"):
                argvals.append(self._out_arg(raw_args, modes, ix))
            else:
                raise Exception(f"Invalid argument name: {a}")

        self._print_op(op, raw_args, argvals, modes)
        op(*argvals)

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
            elif mode == '2':
                arg_strings.append(f"@{raw:<+4}")
            else:
                raise Exception(f"Unknown param mode: {mode}")

        val_strings = [f"{v}" for v in argvals]

        self._debug(f"{self.pc:4}: {name:4} " + " ".join(arg_strings) + " (" + ", ".join(val_strings) + ")")


    def _arg(self, args, modes, ix):
        mode = modes[ix] if ix < len(modes) else '0'
        if mode == '0':
            return self.memory[args[ix]]
        elif mode == '1':
            return args[ix]
        elif mode == '2':
            addr = self.rel_base + args[ix]
            return self.memory[addr]
        else:
            raise Exception(f"Unknown param mode: {mode}")

    def _out_arg(self, args, modes, ix):
        mode = modes[ix] if ix < len(modes) else '0'
        if mode == '0':
            return args[ix]
        elif mode == '2':
            addr = self.rel_base + args[ix]
            return addr
        else:
            raise Exception(f"Unknown output param mode: {mode}")

    # Operations
    def __halt(self):
        self.halted = True

    def __add(self, a1, a2, o):
        self.memory[o] = a1 + a2

    def __mul(self, a1, a2, o):
        self.memory[o] = a1 * a2

    def __inp(self, o):
        v = self.input()
        if v is None:
            self.waiting = o
        else:
            self.memory[o] = int(v)

    def __out(self, a1):
        self.output(a1)

    def __jit(self, a1, a2):
        if a1 != 0:
            self.pc = a2

    def __jif(self, a1, a2):
        if a1 == 0:
            self.pc = a2

    def __lt(self, a1, a2, o):
        self.memory[o] = 1 if a1 < a2 else 0

    def __eq(self, a1, a2, o):
        self.memory[o] = 1 if a1 == a2 else 0

    def __arb(self, a1):
        self.rel_base += a1

