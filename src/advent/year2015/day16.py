tape = {
    "children": 3,
    "cats": 7,
    "samoyeds": 2,
    "pomeranians": 3,
    "akitas": 0,
    "vizslas": 0,
    "goldfish": 5,
    "trees": 3,
    "cars": 2,
    "perfumes": 1,
}


def parse(data):
    results = []
    for line in data.splitlines():
        args = " ".join(line.split()[2:]).split(", ")
        results.append({arg.split(":")[0]: int(arg.split(":")[1]) for arg in args})
    return results


def part1(data):
    data = parse(data)
    for i, aunt in enumerate(data):
        if all(tape[k] == v for k, v in aunt.items()):
            return i + 1


def check(item):
    k, v = item
    if k in ("cats", "trees"):
        return v > tape[k]
    if k in ("pomeranians", "goldfish"):
        return v < tape[k]
    return v == tape[k]


def part2(data):
    data = parse(data)
    for i, aunt in enumerate(data):
        if all(map(check, aunt.items())):
            return i + 1
