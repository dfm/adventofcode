neighbors = ((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))


def parse(data, corners=False):
    grid = [[False] + [x == "#" for x in line] + [False] for line in data.splitlines()]
    grid = [[False] * len(grid[0])] + grid + [[False] * len(grid[0])]
    if corners:
        grid[1][1] = True
        grid[-2][1] = True
        grid[-2][-2] = True
        grid[1][-2] = True
    return grid


def step(grid, corners=False):
    new_grid = [[False] * len(grid[0]) for _ in range(len(grid))]
    for y in range(1, len(grid) - 1):
        for x in range(1, len(grid[0]) - 1):
            count = sum(grid[y + dy][x + dx] for dx, dy in neighbors)
            if grid[y][x]:
                new_grid[y][x] = count in (2, 3)
            else:
                new_grid[y][x] = count == 3
    if corners:
        new_grid[1][1] = True
        new_grid[-2][1] = True
        new_grid[-2][-2] = True
        new_grid[1][-2] = True
    return new_grid


def part1(data):
    grid = parse(data)
    for _ in range(100):
        grid = step(grid)
    return sum(sum(row) for row in grid)


def part2(data):
    grid = parse(data, corners=True)
    for _ in range(100):
        grid = step(grid, corners=True)
    return sum(sum(row) for row in grid)
