from collections import Counter

data = Counter(map(int, open("data/11.txt").read().split()))

def update(stone):
  if stone == 0:
    return [1]
  s = str(stone)
  if len(s) % 2 == 0:
    return [int(s[:len(s) // 2]), int(s[len(s) // 2:])]
  return [stone * 2024]

def step(data):
  new_data = Counter()
  for stone, count in data.items():
    for s in update(stone):
      new_data.update({s: count})
  return new_data

for _ in range(25):
  data = step(data)
print(sum(data.values()))

for _ in range(50):
  data = step(data)
print(sum(data.values()))
