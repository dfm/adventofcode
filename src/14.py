import re

# nx, ny = 11, 7
nx, ny = 101, 103
p = re.compile(r"p=(.+),(.+) v=(.+),(.+)")

data = [tuple(map(int, p.match(line).groups())) for line in open("data/14.txt")]

def simulate(data):
  new_data = []
  for x, y, vx, vy in data:
    x = (x + vx) % nx
    y = (y + vy) % ny
    new_data.append((x, y, vx, vy))
  return new_data

def factor(data):
  count = {(0, 0): 0, (0, 1): 0, (1, 0): 0, (1, 1): 0}
  for x, y, *_ in data:
    if x == (nx - 1) // 2 or y == (ny - 1) // 2:
      continue
    count[(x // ((nx + 1) // 2), y // ((ny + 1) // 2))] += 1
  return count[(0, 0)] * count[(1, 1)] * count[(0, 1)] * count[(1, 0)]

n = 0
while True:
  data = simulate(data)
  n += 1
  if n == 100:
    print(factor(data))

  if len(set((x, y) for x, y, *_ in data)) == len(data):
    print(n)
    for y in range(ny):
      for x in range(nx):
        count = sum(x == a and y == b for a, b, *_ in data)
        if count:
          print(count, end="")
        else:
          print(".", end="")
      print()
    break
