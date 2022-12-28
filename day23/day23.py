import operator


class IncodeMachine:
    def __init__(self, program, input_queue=None):
        self.cursor = 0
        self.relative_base = 0
        self.program = program
        self.input_queue = [] if input_queue is None else input_queue
        self.output_queue = []

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
        self.binary_op(mode1, mode2, mode3, operator.lt)

    def eq(self, mode1, mode2, mode3):
        self.binary_op(mode1, mode2, mode3, operator.eq)

    def update_relative_base(self, mode1, mode2, mode3):
        del mode2, mode3
        self.relative_base += self.get(self.cursor + 1, mode1)
        self.cursor += 2


class Network:
    def __init__(self, program):
        self.prev_nat = None
        self.nat = None
        self.machines = []
        self.messages = []
        for n in range(50):
            self.machines.append(IncodeMachine(list(program), input_queue=[n]))
            self.messages.append([])

    def loop(self):
        if all(len(m) == 0 for m in self.messages) and self.nat:
            if self.prev_nat and self.prev_nat[1] == self.nat[1]:
                print("Part 2:", self.nat[1])
                return False
            self.machines[0].input_queue += self.nat
            self.prev_nat = self.nat
        for n, machine in enumerate(self.machines):
            if not machine.input_queue:
                if self.messages[n]:
                    machine.input_queue += self.messages[n]
                    self.messages[n] = []
                else:
                    machine.input_queue.append(-1)
            machine.run()
            if machine.output_queue:
                for i in range(0, len(machine.output_queue), 3):
                    address = machine.output_queue[i]
                    if address == 255:
                        if self.nat is None:
                            print("Part 1:", machine.output_queue[i+2])
                        self.nat = machine.output_queue[i+1:i+3]
                    else:
                        self.messages[address] += machine.output_queue[i+1:i+3]
                machine.output_queue = []
        return True

program = list(map(int, open("day23/day23.txt").read().split(",")))
network = Network(program)
while network.loop():
    pass
