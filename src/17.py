reg, prog = open("data/17.txt").read().split("\n\n")

reg = [int(line.split(":")[1]) for line in reg.splitlines()]
_, prog = prog.replace("\n", "").split(":")
prog = list(map(int, prog.split(",")))
cursor = 0

class Interpreter:
  reg: list[int]
  prog: list[int]
  cursor: int = 0

  def __init__(self, reg: list[int], prog: list[int]):
    self.reg = reg
    self.prog = prog
    self.stdout = []
    self.ops = [
        self.adv,
        self.bxl,
        self.bst,
        self.jnz,
        self.bxc,
        self.out,
        self.bdv,
        self.cdv,
    ]

  @property
  def literal(self):
    return self.prog[self.cursor + 1]

  @property
  def combo(self):
    instr = self.literal
    if instr <= 3:
      return instr
    elif instr <= 6:
      return self.reg[instr - 4]
    raise NotImplementedError()

  def adv(self):
    self.reg[0] //= 2 ** self.combo
    self.cursor += 2

  def bxl(self):
    self.reg[1] ^= self.literal
    self.cursor += 2

  def bst(self):
    self.reg[1] = self.combo % 8
    self.cursor += 2

  def jnz(self):
    if self.reg[0] == 0:
      self.cursor += 2
    else:
      self.cursor = self.literal

  def bxc(self):
    self.reg[1] ^= self.reg[2]
    self.cursor += 2

  def out(self):
    self.stdout.append(self.combo % 8)
    self.cursor += 2

  def bdv(self):
    self.reg[1] = self.reg[0] // 2 ** self.combo
    self.cursor += 2

  def cdv(self):
    self.reg[2] = self.reg[0] // 2 ** self.combo
    self.cursor += 2

  def run(self):
    while self.cursor < len(self.prog):
      fun = self.ops[self.prog[self.cursor]]
      # print(self.cursor, fun.__name__)
      fun()
    self.show()

  def show(self):
    print(",".join(map(str, self.stdout)))

interp = Interpreter(reg, prog)
interp.run()

def run(a):
  b = 0
  c = 0
  res = []
  while a != 0:
    b = a % 8
    b = b ^ 3
    c = a // 2 ** b
    b = b ^ c
    b = b ^ 5
    a = a // 8
    res.append(b % 8)
  return res

def test(a, depth):
  if depth == len(prog):
    return a
  for i in range(8):
    res = run(a * 8 + i)
    if res and res[0] == prog[::-1][depth]:
      if result := test(a * 8 + i, depth + 1):
        return result
  return 0

print(test(0, 0))
