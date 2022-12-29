def evaluate(ops, values, name):
    if name in values:
        return values[name]
    else:
        func, a, b = ops[name]
        values[name] = func(evaluate(ops, values, a), evaluate(ops, values, b))
        return values[name]


def context(data):
    op_map = {
        "IDENT": lambda _, x: x,
        "NOT": lambda _, x: ~x,
        "AND": lambda x, y: x & y,
        "OR": lambda x, y: x | y,
        "LSHIFT": lambda x, y: x << y,
        "RSHIFT": lambda x, y: x >> y,
    }
    ops = {}
    values = {None: 0}
    for line in data.splitlines():
        left, right = line.split(" -> ")
        try:
            values[right] = int(left)
        except ValueError:
            args = left.split()
            op = "IDENT"
            a = None
            b = args[-1]

            try:
                values[b] = int(b)
            except ValueError:
                pass

            if len(args) >= 2:
                op = args[-2]

            if len(args) == 3:
                a = left.split()[0]
                try:
                    values[a] = int(a)
                except ValueError:
                    pass
            else:
                a = None

            ops[right] = (op_map[op], a, b)
    return ops, values


def part1(data):
    ops, values = context(data)
    return evaluate(ops, values, "a")


def part2(data):
    ops, values = context(data)
    values["b"] = evaluate(ops, dict(values), "a")
    return evaluate(ops, values, "a")


def test_part1():
    data = """123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> a
"""
    assert part1(data) == 65079
