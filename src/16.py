import heapq
import math
from collections import defaultdict

grid = [line.strip() for line in open("data/16.txt")]
nx, ny = len(grid[0]), len(grid)
start = next((x, y) for y in range(ny) for x in range(nx) if grid[y][x] == "S")
end = next((x, y) for y in range(ny) for x in range(nx) if grid[y][x] == "E")

def neighbors(dist, x, y, dx, dy):
  return [
      (dist + 1, x + dx, y + dy, dx, dy),
      (dist + 1000, x, y, dy, -dx),
      (dist + 1000, x, y, -dy, dx),
  ]

def search(start, end):
  x, y = start
  dx, dy = 1, 0
  queue = [(0, x, y, dx, dy)]
  distances = {(x, y, dx, dy): 0}
  graph = defaultdict(set)
  best = math.inf
  end_states = set()
  while queue:
    dist, x, y, dx, dy = heapq.heappop(queue)
    if (x, y) == end:
      if dist > best:
        return best, end_states, dict(graph)
      best = dist
      end_states.add((x, y, dx, dy))
    for dist, x_, y_, dx_, dy_ in neighbors(dist, x, y, dx, dy):
      if 0 <= x_ < nx and 0 <= y_ < ny and grid[y_][x_] != "#":
        prev_dist = distances.get((x_, y_, dx_, dy_), math.inf)
        if prev_dist >= dist:
          distances[(x_, y_, dx_, dy_)] = dist
          graph[(x_, y_, dx_, dy_)].add((x, y, dx, dy))
          heapq.heappush(queue, (dist, x_, y_, dx_, dy_))

dist, ends, graph = search(start, end)
print(dist)

def all_paths(graph, starts, end):
  todo = [(s, [s]) for s in starts]
  paths = []
  while todo:
    current, path = todo.pop()
    if (current[0], current[1]) == end:
      paths.append(path)
      continue
    for proposed in graph[current]:
      todo.append((proposed, path + [proposed]))
  return paths

def flatten(paths):
  return set((x, y) for path in paths for x, y, _, _ in path)

print(len(flatten(all_paths(graph, ends, start))))
