import array


def part1(data):
    target = int(data)
    tot = target // 10
    grid = array.array("i", (0 for _ in range(tot)))
    for elf in range(1, tot):
        for n in range(elf, tot, elf):
            grid[n] += 10 * elf
    for n in range(tot):
        if grid[n] >= target:
            return n


def part2(data):
    target = int(data)
    tot = target // 10
    grid = array.array("i", (0 for _ in range(tot)))
    for elf in range(1, tot):
        for n in range(50):
            if elf * (n + 1) >= tot:
                break
            grid[elf + elf * n] += 11 * elf
    for n in range(tot):
        if grid[n] >= target:
            return n
