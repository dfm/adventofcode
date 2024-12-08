from collections import defaultdict
from itertools import combinations

data = defaultdict(list)
for y, line in enumerate(open("data/08.txt")):
  for x, c in enumerate(line.strip()):
    if c != ".":
      data[c].append((x, y))
  rows = y + 1
  cols = x + 1

def extend(a, b):
  ax, ay = a
  bx, by = b
  x, y = 2 * ax - bx, 2 * ay - by
  if 0 <= x < cols and 0 <= y < rows:
    return [(x, y)]
  return []

def nodes(args):
  return extend(*args) + extend(*args[::-1])

def nodes2(args):
  (x, y), (x_, y_)  = args
  dx, dy = x - x_, y - y_
  result = [(x, y)]
  for d in (-1, 1):
    while True:
      x += dx * d
      y += dy * d
      if x < 0 or x >= cols or y < 0 or y >= rows:
        break
      result.append((x, y))
  return result

def solve(fun):
  all_nodes = []
  for locs in data.values():
    all_nodes += [n for row in map(fun, combinations(locs, 2)) for n in row]
  return len(set(all_nodes))

print(solve(nodes))
print(solve(nodes2))
