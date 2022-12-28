import operator


class IncodeMachine:
    def __init__(self, program, input_queue=None, output_queue=None):
        self.debug = False
        self.cursor = 0
        self.relative_base = 0
        self.program = program
        self.input_queue = [] if input_queue is None else input_queue
        self.output_queue = [] if output_queue is None else output_queue

    def get(self, idx, mode):
        if idx >= len(self.program):
            self.program += [0] * (idx + 1 - len(self.program))
        if mode == 0:
            return self.get(self.program[idx], 1)
        elif mode == 1:
            return self.program[idx]
        elif mode == 2:
            return self.get(self.relative_base + self.program[idx], 1)
        raise ValueError(f"Invalid mode: {mode}")

    def set(self, idx, mode, value):
        if self.debug:
            print(f"Setting {idx} to {value} (mode {mode})")
        if idx >= len(self.program):
            self.program += [0] * (idx + 1 - len(self.program))
        if mode == 0:
            self.set(self.program[idx], 1, value)
        elif mode == 1:
            self.program[idx] = value
        elif mode == 2:
            self.set(self.relative_base + self.program[idx], 1, value)
        else:
            raise ValueError(f"Invalid mode: {mode}")

    def run(self):
        while self.step() is None:
            pass

    def step(self):
        code = self.program[self.cursor]
        mode3 = code // 10000
        code -= mode3 * 10000
        mode2 = code // 1000
        code -= mode2 * 1000
        mode1 = code // 100
        code -= mode1 * 100

        if code == 99:
            return True

        op_map = {
            1: self.add,
            2: self.mul,
            3: self.input,
            4: self.output,
            5: self.jump_true,
            6: self.jump_false,
            7: self.lt,
            8: self.eq,
            9: self.update_relative_base,
        }

        return op_map[code](mode1, mode2, mode3)

    def binary_op(self, mode1, mode2, mode3, op):
        value = op(self.get(self.cursor + 1, mode1), self.get(self.cursor + 2, mode2))
        self.set(self.cursor + 3, mode3, value)
        self.cursor += 4
    
    def add(self, mode1, mode2, mode3):
        self.binary_op(mode1, mode2, mode3, operator.add)

    def mul(self, mode1, mode2, mode3):
        self.binary_op(mode1, mode2, mode3, operator.mul)

    def input(self, mode1, mode2, mode3):
        del mode2, mode3
        if len(self.input_queue) == 0:
            return False
        self.set(self.cursor + 1, mode1, self.input_queue.pop(0))
        self.cursor += 2

    def output(self, mode1, mode2, mode3):
        del mode2, mode3
        self.output_queue.append(self.get(self.cursor + 1, mode1))
        self.cursor += 2

    def jump_true(self, mode1, mode2, mode3):
        del mode3
        if self.get(self.cursor + 1, mode1):
            self.cursor = self.get(self.cursor + 2, mode2)
        else:
            self.cursor += 3

    def jump_false(self, mode1, mode2, mode3):
        del mode3
        if self.get(self.cursor + 1, mode1) == 0:
            self.cursor = self.get(self.cursor + 2, mode2)
        else:
            self.cursor += 3

    def lt(self, mode1, mode2, mode3):
        self.binary_op(mode1, mode2, mode3, lambda a, b: int(a < b))

    def eq(self, mode1, mode2, mode3):
        self.binary_op(mode1, mode2, mode3, lambda a, b: int(a == b))

    def update_relative_base(self, mode1, mode2, mode3):
        del mode2, mode3
        self.relative_base += self.get(self.cursor + 1, mode1)
        self.cursor += 2


class Io:
    def __init__(self):
        self.buffer = []

    def __len__(self):
        return 1
    
    def pop(self, _):
        if self.buffer:
            return self.buffer.pop(0)
        print(f"{machine.cursor} > ", end="", flush=True)
        data = input()
        if data == "debug":
            machine.debug = True
            data = input()
        self.buffer = [ord(c) for c in data] + [ord('\n')]
        return self.buffer.pop(0)
 
    def append(self, value):
        print(chr(value), end="")


program = list(map(int, open("day25/day25.txt").read().split(",")))
io = Io()
io.buffer = [ord(c) for c in open("day25/steps.txt").read()]
machine = IncodeMachine(program, input_queue=io, output_queue=io)
machine.run()
