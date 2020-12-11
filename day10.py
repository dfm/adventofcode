with open("inputs/day10", "r") as f:
    data = list(sorted(map(int, f)))

cache = {0: 1}
for n in data:
    cache[n] = sum(cache.get(e, 0) for e in [n - 1, n - 2, n - 3])

print("Part 2:", cache[data[-1]])
