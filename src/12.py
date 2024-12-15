from collections import defaultdict

data = [line.strip() for line in open("data/12.txt")]
nx, ny = len(data[0]), len(data)

def expand_region(data, visited, x, y):
  visited.add((x, y))
  c = data[y][x]
  region = []
  neighbors = []
  for dx, dy in [(0, -1), (-1, 0), (1, 0), (0, 1)]:
    x_, y_ = x + dx, y + dy
    if 0 <= x_ < nx and 0 <= y_ < ny and data[y_][x_] == c:
      neighbors += [(dx, dy)]
      if (x_, y_) not in visited:
        region += expand_region(data, visited, x_, y_)
  return [(x, y, neighbors)] + region

def find_regions(data):
  regions = []
  visited = set()
  for y in range(ny):
    for x in range(nx):
      if (x, y) in visited:
        continue
      region = expand_region(data, visited, x, y)
      regions.append(region)
  return regions

regions = find_regions(data)

def perim(region):
  return sum(4 - len(n) for *_, n in region)

def cost1(region):
  area = len(region)
  return area * perim(region)

print(sum(map(cost1, regions)))

def count_sides(sides):
  sides = list(sorted(sides))
  return 1 + sum(b - a > 1 for a, b in zip(sides[:-1], sides[1:]))

def sides(region):
  sx = []
  sy = []
  for x, y, ns in region:
    for dx in (-1, 1):
      if (dx, 0) not in ns:
        sx.append((x, y, dx))
    for dy in (-1, 1):
      if (0, dy) not in ns:
        sy.append((x, y, dy))
  total = 0
  sides = defaultdict(list)
  for s in sx:
    sides[(s[0], s[2])].append(s[1])
  total += sum(map(count_sides, sides.values()))
  sides = defaultdict(list)
  for s in sy:
    sides[(s[1], s[2])].append(s[0])
  total += sum(map(count_sides, sides.values()))
  return total

def cost2(region):
  area = len(region)
  return area * sides(region)

print(sum(map(cost2, regions)))
