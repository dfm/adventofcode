def step(s):
    result = ""
    cursor = 0
    while cursor < len(s):
        start = cursor
        v = s[cursor]
        cursor += 1
        while cursor < len(s) and s[cursor] == v:
            cursor += 1
        result += f"{cursor - start}{v}"
    return result


def part1(data):
    s = data.strip()
    for _ in range(40):
        s = step(s)
    return len(s)


def part2(data):
    s = data.strip()
    for _ in range(50):
        s = step(s)
    return len(s)
