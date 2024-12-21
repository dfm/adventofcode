from collections import Counter

num_pad = {
    "7": (0, 0), "8": (1, 0), "9": (2, 0),
    "4": (0, 1), "5": (1, 1), "6": (2, 1),
    "1": (0, 2), "2": (1, 2), "3": (2, 2),
    "0": (1, 3), "A": (2, 3)
}
dir_pad = {
    "^": (1, 0), "A": (2, 0), "<": (0, 1), "v": (1, 1), ">": (2, 1)
}

def walk(key_pad, src, dst):
  sx, sy = key_pad[src]
  dx, dy = key_pad[dst]
  xx = dx - sx
  yy = dy - sy
  h = ">" * xx + "<" * -xx
  v = "v" * yy + "^" * -yy
  if xx > 0 and (sx, dy) in key_pad.values():
    return v + h + "A"
  if (dx, sy) in key_pad.values():
    return h + v + "A"
  return v + h + "A"

def get_route(key_pad, path):
  start = "A"
  res = []
  for k in path:
    res.append(walk(key_pad, start, k))
    start = k
  return res

def solve(num):
  total = 0
  for line in open("data/21.txt"):
    route = {"".join(get_route(num_pad, line.strip())): 1}
    for _ in range(num):
      new_route = Counter()
      for k, v in route.items():
        c = Counter(get_route(dir_pad, k))
        for k2, v2 in c.items():
          new_route[k2] += v * v2
      route = new_route
    total += int(line[:-2]) * sum(len(k) * v for k, v in route.items())
  return total

print(solve(2))
print(solve(25))
