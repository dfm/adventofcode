def run(data, registers):
    instructions = data.splitlines()
    cursor = 0
    while cursor < len(instructions):
        instr = instructions[cursor]
        parts = instr.split()
        if parts[0] == "hlf":
            registers[parts[1]] //= 2
            cursor += 1
        elif parts[0] == "tpl":
            registers[parts[1]] *= 3
            cursor += 1
        elif parts[0] == "inc":
            registers[parts[1]] += 1
            cursor += 1
        elif parts[0] == "jmp":
            cursor += int(parts[1])
        elif parts[0] == "jie":
            if registers[parts[1][:-1]] % 2 == 0:
                cursor += int(parts[2])
            else:
                cursor += 1
        elif parts[0] == "jio":
            if registers[parts[1][:-1]] == 1:
                cursor += int(parts[2])
            else:
                cursor += 1
    return registers["b"]


def part1(data):
    return run(data, {"a": 0, "b": 0})


def part2(data):
    return run(data, {"a": 1, "b": 0})
