def count_characters(s):
    count = 0
    n = 1
    while n < len(s) - 1:
        if s[n] == "\\":
            if s[n + 1] == "x":
                n += 3
            else:
                n += 1
        n += 1
        count += 1
    return len(s) - count


def part1(data):
    return sum(count_characters(s) for s in data.splitlines())


def encode(s):
    result = ""
    for c in s:
        if c == "\\" or c == '"':
            result += "\\"
        result += c
    return len(result) - len(s) + 2


def part2(data):
    return sum(encode(s) for s in data.splitlines())
