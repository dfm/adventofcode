data = []
for line in open("data/07.txt"):
  k, rest = line.split(": ")
  data.append((int(k), list(map(int, rest.split()))))

def solve(target, elements, part2=False):
  todo = [(target, elements)]
  while todo:
    target, elements = todo.pop()
    *rest, value = elements
    if not rest:
      if target == value:
        return True
      continue
    if target % value == 0:
      todo.append((target // value, rest))
    if target - value > 0:
      todo.append((target - value, rest))
    if part2 and target != value and str(target).endswith(str(value)):
      todo.append((int(str(target)[:-len(str(value))]), rest))
  return False

print(sum(row[0] for row in data if solve(*row)))
print(sum(row[0] for row in data if solve(*row, part2=True)))
