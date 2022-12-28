from collections import Counter

test_data = """....#
#..#.
#..##
..#..
#....
"""

real_data = """..#.#
#####
.#...
...#.
##...
"""

def to_grid(data):
    grid = set()
    for y, line in enumerate(data.splitlines()):
        for x, c in enumerate(line):
            if c == "#":
                grid.add((0, x - 2, y - 2))
    return grid

def neighbors_part1(coord):
    depth, x, y = coord
    neighbors = []
    for dx, dy in ((-1, 0), (1, 0), (0, -1), (0, 1)):
        if x + dx < -2 or x + dx > 2 or y + dy < -2 or y + dy > 2:
            continue
        neighbors.append((depth, x + dx, y + dy))
    return neighbors

def neighbors_part2(coord):
    depth, x, y = coord
    neighbors = []
    for dx, dy in ((-1, 0), (1, 0), (0, -1), (0, 1)):
        xp, yp = x + dx, y + dy
        if xp == 0 and yp == 0:
            if dx == 0:
                for i in range(-2, 3):
                    neighbors.append((depth + 1, i, -2 * dy))
            else:
                for i in range(-2, 3):
                    neighbors.append((depth + 1, -2 * dx, i))
        elif xp < -2 or xp > 2:
            neighbors.append((depth - 1, dx, 0))
        elif yp < -2 or yp > 2:
            neighbors.append((depth - 1, 0, dy))
        else:
            neighbors.append((depth, xp, yp))
    return neighbors


def step(get_neighbors, grid):
    new_grid = set()
    counts = Counter()
    for coord in grid:
        count = 0
        for neighbor in get_neighbors(coord):
            if neighbor in grid:
                count += 1
            else:
                counts[neighbor] += 1
        if count == 1:
            new_grid.add(coord)
    for coord, count in counts.items():
        if count == 1 or count == 2:
            new_grid.add(coord)
    return new_grid

def show(depth, grid):
    for y in range(-2, 3):
        for x in range(-2, 3):
            print("#" if (depth, x, y) in grid else ".", end="")
        print()
    print()

def score(grid):
    result = 0
    n = 1
    for y in range(-2, 3):
        for x in range(-2, 3):
            if (0, x, y) in grid:
                result += n
            n *= 2
    return result


def part1(data):
    seen = set()
    grid = to_grid(data)
    seen.add(score(grid))
    while True:
        grid = step(neighbors_part1, grid)
        s = score(grid)
        if s in seen:
            print("Part 1:", s)
            break
        seen.add(s)

def part2(data):
    grid = to_grid(data)
    for n in range(200):
        grid = step(neighbors_part2, grid)
    print("Part 2:", len(grid))

part1(real_data)
part2(real_data)
