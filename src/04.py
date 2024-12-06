import itertools

data = [line.strip() for line in open("data/04.txt")]
rows = len(data)
cols = len(data[0])

total = 0
for y in range(rows):
  for x in range(cols):
    if data[y][x] != "X":
      continue
    for dx, dy in itertools.product([-1, 0, 1], repeat=2):
      if dx == 0 and dy == 0:
        continue
      x_, y_ = x, y
      found = True
      for c in "MAS":
        x_ += dx
        y_ += dy
        if x_ < 0 or x_ >= cols or y_ < 0 or y_ >= rows or data[y_][x_] != c:
          found = False
          break
      total += found
print(total)

total = 0
expected = set("MS")
for y in range(1, rows - 1):
  for x in range(1, cols - 1):
    if data[y][x] != "A":
      continue
    w1 = {data[y - 1][x - 1], data[y + 1][x + 1]}
    w2 = {data[y + 1][x - 1], data[y - 1][x + 1]}
    if w1 == expected and w2 == expected:
      total += 1
print(total)
