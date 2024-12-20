import heapq
import math

grid = [line.strip() for line in open("data/20.txt")]
nx, ny = len(grid[0]), len(grid)
start = next((x, y) for y in range(ny) for x in range(nx) if grid[y][x] == "S")
end = next((x, y) for y in range(ny) for x in range(nx) if grid[y][x] == "E")

def initial(start):
  queue = [(0, *start)]
  dists = {start: 0}
  while queue:
    dist, x, y = heapq.heappop(queue)
    for dx, dy in [(0, -1), (-1, 0), (1, 0), (0, 1)]:
      x_, y_ = x + dx, y + dy
      dist_ = dist + 1
      if x_ < 0 or nx <= x_ or y_ < 0 or ny <= y_ or dist_ >= dists.get((x_, y_), math.inf):
        continue
      if grid[y_][x_] != "#":
        dists[(x_, y_)] = dist_
        heapq.heappush(queue, (dist_, x_, y_))
  return dists

fwd = initial(start)
bwd = initial(end)
longest = fwd[end]
# print(longest)

def solve(start, dist, min_cheats=2, max_cheats=2):
  queue = [(dist, *start, 0, start)]
  dists = {start: dist}
  results = {}
  while queue:
    dist, x, y, cheats, start = heapq.heappop(queue)
    for dx, dy in [(0, -1), (-1, 0), (1, 0), (0, 1)]:
      x_, y_ = x + dx, y + dy
      dist_ = dist + 1
      cheats_ = cheats + 1
      if x_ < 0 or nx <= x_ or y_ < 0 or ny <= y_ or dist_ >= dists.get((x_, y_), math.inf):
        continue
      if cheats_ >= min_cheats:
        if (x_, y_) in bwd:
          results[(start, (x_, y_))] = dist_ + bwd[(x_, y_)]
      if cheats_ >= max_cheats or dist_ > longest - 100:
        continue
      dists[(x_, y_)] = dist_
      heapq.heappush(queue, (dist_, x_, y_, cheats_, start))
  return results

all_paths = {}
for (x, y), d in fwd.items():
  all_paths.update(solve((x, y), d))
print(sum(v <= longest - 100 for v in all_paths.values()))

all_paths = {}
for (x, y), d in fwd.items():
  if d > longest - 100:
    continue
  all_paths.update(solve((x, y), d, min_cheats=0, max_cheats=20))
print(sum(v <= longest - 100 for v in all_paths.values()))
