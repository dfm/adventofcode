from itertools import combinations


def part1(data):
    data = list(map(int, data.splitlines()))
    result = 0
    for n in range(1, len(data)):
        result += sum(x == 150 for x in map(sum, combinations(data, n)))
    return result


def part2(data):
    data = list(map(int, data.splitlines()))
    for n in range(1, len(data)):
        result = sum(x == 150 for x in map(sum, combinations(data, n)))
        if result:
            return result
    return result
