grid = [list(map(int, line.strip())) for line in open("data/10.txt")]
nx, ny = len(grid[0]), len(grid)

def score(x, y, rating):
  total = 0
  todo = [(x, y)]
  visited = set([(x, y)])
  while todo:
    x, y = todo.pop()
    if grid[y][x] == 9:
      total += 1
      continue
    for dx, dy in [(0, -1), (-1, 0), (1, 0), (0, 1)]:
      x_, y_ = x + dx, y + dy
      if 0 <= x_ < nx and 0 <= y_ < ny and grid[y_][x_] == grid[y][x] + 1 and (rating or (x_, y_) not in visited):
        visited.add((x_, y_))
        todo.append((x_, y_))
  return total

def solve(rating):
  total = 0
  for y in range(ny):
    for x in range(nx):
      if grid[y][x] != 0:
        continue
      total += score(x, y, False)
  return total

print(solve(False))
print(solve(True))
