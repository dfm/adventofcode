grid, steps = open("data/15.txt").read().split("\n\n")
steps = steps.replace("\n", "")

if True:
  grid = grid.replace("#", "##")
  grid = grid.replace("O", "[]")
  grid = grid.replace(".", "..")
  grid = grid.replace("@", "@.")

grid = [line.strip() for line in grid.splitlines()]

nx, ny = len(grid[0]), len(grid)

directions = {"^": (0, -1), "v": (0, 1), "<": (-1, 0), ">": (1, 0)}
walls = set((x, y) for y in range(ny) for x in range(nx) if grid[y][x] == "#")
barrels = [[(x, y)] for y in range(ny) for x in range(nx) if grid[y][x] == "O"]
barrels += [[(x, y), (x + 1, y)] for y in range(ny) for x in range(nx) if grid[y][x] == "["]
rx, ry = next((x, y) for y in range(ny) for x in range(nx) if grid[y][x] == "@")

def can_move(idx, x, y, dx, dy):
  x, y = x + dx, y + dy
  if (x, y) in walls:
    return False, []
  flag = True
  todo = []
  for i, b in enumerate(barrels):
    if i == idx:
      continue
    if (x, y) in b:
      todo.append(i)
      for x_, y_ in b:
        flag_, todo_ = can_move(i, x_, y_, dx, dy)
        flag = flag and flag_
        todo.extend(todo_)
        if not flag:
          return flag, todo
  return flag, set(todo)

def move(x, y, d):
  dx, dy = directions[d]
  should_move, todo = can_move(-1, x, y, dx, dy)
  if not should_move:
    return x, y
  for i in todo:
    barrels[i] = [(bx + dx, by + dy) for bx, by in barrels[i]]
  return x + dx, y + dy
  
def show():
  for y in range(ny):
    for x in range(nx):
      if (x, y) in walls:
        print("#", end="")
      elif (x, y) in [b[0] for b in barrels if len(b) == 1]:
        print("O", end="")
      elif (x, y) in [b[0] for b in barrels if len(b) == 2]:
        print("[", end="")
      elif (x, y) in [b[1] for b in barrels if len(b) == 2]:
        print("]", end="")
      elif x == rx and y == ry:
        print("@", end="")
      else:
        print(".", end="")
    print()
  print()

def score():
  total = 0
  for y in range(ny):
    for x in range(nx):
      if (x, y) in [b[0] for b in barrels]:
        total += 100 * y + x
  return total

for s in steps:
  rx, ry = move(rx, ry, s)
show()
print(score())
