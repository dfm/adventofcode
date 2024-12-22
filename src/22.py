from collections import Counter

def update(n):
  n = ((n * 64) ^ n) % 16777216
  n = ((n // 32) ^ n) % 16777216
  return ((n * 2048) ^ n) % 16777216

def run(n):
  for _ in range(2000):
    n = update(n)
  return n

print(sum(run(int(line)) for line in open("data/22.txt")))

def sequence(n):
  accum = {}
  diffs = ()
  prev = n % 10
  for _ in range(2000):
    m = update(n)
    diff = m % 10 - prev
    prev = m % 10
    n = m
    diffs = diffs[-3:] + (diff,)
    if len(diffs) == 4 and diffs not in accum:
      accum[diffs] = m % 10
  return accum

accum = Counter()
for line in open("data/22.txt"):
  accum.update(sequence(int(line)))
print(accum.most_common(1)[0][1])
