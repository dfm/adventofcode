from typing import Callable


def solve(data: str, func: Callable[[int, int], int]) -> int:
    commands = []
    xs = []
    ys = []
    for line in data.splitlines():
        cmd = {"turn on": 1, "turn off": -1, "toggle": 2}[" ".join(line.split()[:-3])]
        x1, y1 = map(int, line.split()[-3].split(","))
        x2, y2 = map(int, line.split()[-1].split(","))
        x1, x2 = min(x1, x2), max(x1, x2)
        y1, y2 = min(y1, y2), max(y1, y2)
        xs += [x1, x2 + 1]
        ys += [y1, y2 + 1]
        commands.append((cmd, x1, x2, y1, y2))

    result = 0
    xs = list(sorted(xs))
    ys = list(sorted(ys))
    for i, y in enumerate(ys[:-1]):
        for j, x in enumerate(xs[:-1]):
            value = 0
            for cmd, x1, x2, y1, y2 in commands:
                if x < x1 or x > x2 or y < y1 or y > y2:
                    continue
                value = func(cmd, value)
            result += value * (xs[j + 1] - x) * (ys[i + 1] - y)

    return result


def part1(data: str):
    def func(cmd: int, value: int) -> int:
        if cmd == 1:
            return 1
        elif cmd == -1:
            return 0
        else:
            return 1 - value

    return solve(data, func)


def part2(data: str):
    def func(cmd: int, value: int) -> int:
        return max(0, value + cmd)

    return solve(data, func)
