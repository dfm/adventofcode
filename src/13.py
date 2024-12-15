import re

pattern = r"""Button A: X\+([0-9]+), Y\+([0-9]+)
Button B: X\+([0-9]+), Y\+([0-9]+)
Prize: X=([0-9]+), Y=([0-9]+)"""

data = open("data/13.txt").read()
for part2 in [False, True]:
  total = 0
  for part in data.split("\n\n"):
    ax, ay, bx, by, x, y = tuple(map(int, re.match(pattern, part, re.M).groups()))
    if part2:
      x += 10000000000000
      y += 10000000000000
    num = y * bx - by * x
    denom = ay * bx - by * ax
    if num % denom == 0:
      a = num // denom
      num = x - a * ax
      if num % bx == 0:
        b = num // bx
        total += a * 3 + b
  print(total)
