from itertools import combinations
from operator import mul
from functools import reduce


def run(weights, num_groups):
    target = sum(weights) // num_groups
    boxes = set()
    for n in range(1, len(weights)):
        for box in combinations(weights, n):
            if sum(box) == target:
                boxes.add((len(box), reduce(mul, box, 1)))
        if len(boxes):
            break
    return sorted(boxes)[0][1]


def part1(data):
    return run(list(map(int, data.splitlines())), 3)


def part2(data):
    return run(list(map(int, data.splitlines())), 4)


def test_part1():
    assert run([1, 2, 3, 4, 5, 7, 8, 9, 10, 11], 3) == 99
