import re

data = open("data/03.txt").read()

total = 0
for a, b in re.findall(r"mul\(([0-9]+),([0-9]+)\)", data, re.M):
  if len(a) > 3 or len(b) > 3:
    continue
  total += int(a) * int(b)
print(total)

total = 0
flag = True
pattern = r"(do\(\))|(don't\(\))|mul\(([0-9]+),([0-9]+)\)"
for start, end, a, b in re.findall(pattern, data, re.M):
  if start:
    flag = True
  elif end:
    flag = False
  elif flag:
    if len(a) <= 3 and len(b) <= 3:
      total += int(a) * int(b)
print(total)
