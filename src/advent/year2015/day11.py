def valid(pw):
    if not any(
        (ord(a) + 1 == ord(b)) and (ord(b) + 1 == ord(c))
        for a, b, c in zip(pw[:-2], pw[1:-1], pw[2:])
    ):
        return False

    if "i" in pw or "o" in pw or "l" in pw:
        return False

    if len(set(a for a, b in zip(pw[:-1], pw[1:]) if a == b)) < 2:
        return False

    return True


def increment(pw):
    for i in range(len(pw) - 1, -1, -1):
        if pw[i] == "z":
            pw[i] = "a"
        else:
            pw[i] = chr(ord(pw[i]) + 1)
            break
    return pw


def part1(data):
    pw = list(data.strip())
    while not valid(pw):
        pw = increment(pw)
    return "".join(pw)


def part2(data):
    pw = increment(list(part1(data)))
    while not valid(pw):
        pw = increment(pw)
    return "".join(pw)
