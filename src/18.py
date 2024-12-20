import heapq
import math

data = [tuple(map(int, line.split(","))) for line in open("data/18.txt")]
# size = 6
size = 70

def solve(n):
  obst = set(data[:n])
  queue = [(0, 0, 0)]
  dists = {}
  while queue:
    dist, x, y = heapq.heappop(queue)
    if (x, y) == (size, size):
      return dist
    for dx, dy in [(0, -1), (-1, 0), (1, 0), (0, 1)]:
      x_, y_ = x + dx, y + dy
      dist_ = dist + 1
      if 0 <= x_ <= size and 0 <= y_ <= size and dist_ < dists.get((x_, y_), math.inf) and (x_, y_) not in obst:
        dists[(x_, y_)] = dist_
        heapq.heappush(queue, (dist_, x_, y_))

print(solve(1024))

mn, mx = 0, len(data)
while mn < mx:
  mid = (mn + mx) // 2
  if solve(mid) is None:
    mx = mid
  else:
    mn = mid + 1
if solve(mid) is None:
  mid -= 1
print(",".join(map(str, data[mid])))
