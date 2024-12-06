grid = [line.strip() for line in open("data/06.txt")]
start = "".join(grid).index("^")
x, y = start % len(grid[0]), start // len(grid[0])
dx, dy = 0, -1


def walk(x, y, dx, dy, x0=-1, y0=-1):
  trajectory = [(x, y, dx, dy)]
  while True:
    x += dx
    y += dy
    if x < 0 or x >= len(grid[0]) or y < 0 or y >= len(grid):
      return False, trajectory
    if (x, y, dx, dy) in trajectory:
      return True, trajectory
    if (x, y) == (x0, y0) or grid[y][x] == "#":
      x -= dx
      y -= dy
      dx, dy = -dy, dx
    trajectory.append((x, y, dx, dy))


_, trajectory = walk(x, y, dx, dy)
positions = {}
for k in trajectory[1:]:
  if k[:2] not in positions:
    positions[k[:2]] = k[2:]
print(len(positions) + 1)
print(
  sum(
    walk(x0 - dx, y0 - dy, dx, dy, x0, y0)[0]
    for (x0, y0), (dx, dy) in positions.items()
  )
)
