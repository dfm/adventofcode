from functools import cmp_to_key

header, data = open("data/05.txt").read().split("\n\n")

cmp = {}
for line in header.splitlines():
  a, b = map(int, line.split("|"))
  cmp[(a, b)] = -1
  cmp[(b, a)] = 1

def compare(a, b):
  return cmp[(a, b)]

total1 = 0
total2 = 0
for line in data.splitlines():
  row = list(map(int, line.split(",")))
  row2 = list(sorted(row, key=cmp_to_key(compare)))
  if row == row2:
    total1 += row[len(row) // 2]
  else:
    total2 += row2[len(row2) // 2]
print(total1)
print(total2)
