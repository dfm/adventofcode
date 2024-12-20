hdr, bdy = open("data/19.txt").read().split("\n\n")
available = hdr.split(", ")
targets = bdy.splitlines()

def count(available, target):
  dp = [1] + [0] * len(target)
  for n in range(len(target)):
    for v in available:
      if target[n:].startswith(v):
        dp[n + len(v)] += dp[n]
  return dp[-1]

counts = [count(available, t) for t in targets]
print(sum(map(bool, counts)))
print(sum(counts))
