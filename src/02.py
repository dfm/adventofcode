import math


def is_safe(rpt):
  diff = set(a - b for a, b in zip(rpt[:-1], rpt[1:]))
  sgn = {math.copysign(1, a) for a in diff}
  return len(sgn) == 1 and all(abs(a) in {1, 2, 3} for a in diff)


def any_is_safe(rpt):
  return any(is_safe(rpt[:i] + rpt[i + 1 :]) for i in range(len(rpt)))


reports = list(list(map(int, line.split())) for line in open("data/02.txt"))
print(sum(map(is_safe, reports)))
print(sum(map(any_is_safe, reports)))
