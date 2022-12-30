from functools import reduce
from itertools import product
import operator


def parse(data):
    result = []
    for line in data.splitlines():
        result.append(tuple(int(p.split()[-1]) for p in line.split(", ")))
    return result


def solve(data, calories=0):
    result = 0
    ingredients = parse(data)
    for amounts in product(range(101), repeat=len(ingredients) - 1):
        total = sum(amounts)
        if total > 100:
            continue
        amounts = list(amounts) + [100 - total]
        values = [
            sum(a * i[n] for i, a in zip(ingredients, amounts))
            for n in range(len(ingredients[0]))
        ]
        if (calories and values[-1] != calories) or any(v <= 0 for v in values):
            continue
        score = reduce(operator.mul, values[:-1], 1)
        result = max(result, score)
    return result


def part1(data):
    return solve(data)


def part2(data):
    return solve(data, calories=500)


def test_part1():
    data = """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
"""
    assert part1(data) == 62842880
