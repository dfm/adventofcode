def neighbors(translations, current):
    possible = []
    for k, v in translations:
        for i in range(len(current)):
            if current[i:].startswith(k):
                possible.append(current[:i] + v + current[i + len(k) :])
    return set(possible)


def part1(data):
    translations, target = data.split("\n\n")
    translations = [x.split(" => ") for x in translations.splitlines()]
    return len(neighbors(translations, target.strip()))


def part2(data):
    _, target = data.strip().split("\n\n")
    cursor = 0
    elements = []
    while cursor < len(target):
        n = cursor + 1
        while n < len(target) and target[n].islower():
            n += 1
        elements.append(target[cursor:n])
        cursor = n
    num_rn = sum(x == "Rn" for x in elements)
    num_ar = sum(x == "Ar" for x in elements)
    num_y = sum(x == "Y" for x in elements)
    return len(elements) - num_rn - num_ar - 2 * num_y - 1


def test_part1():
    test_data = """H => HO
H => OH
O => HH

HOH
"""
    assert part1(test_data) == 4
