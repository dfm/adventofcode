from collections import defaultdict
from itertools import combinations

graph = defaultdict(set)
for line in open("data/23.txt"):
  a, b = line.strip().split("-")
  graph[a].add(b)
  graph[b].add(a)

def isclique(*nodes):
  while nodes:
    *nodes, a = nodes
    connections = graph[a]
    for b in nodes:
      if b not in connections:
        return False
  return True

count = 0
for a, b, c in combinations(graph, 3):
  if (a.startswith("t") or b.startswith("t") or c.startswith("t")) and isclique(a, b, c):
    count += 1
print(count)

def build_maximal_clique(start, nodes):
  nodes = list(nodes)
  nodes.remove(start)
  clique = [start]
  for n, node in enumerate(nodes):
    if all(node in graph[other] for other in clique):
      clique.append(node)
  return clique

nodes = list(graph.keys())
best = 0, None
for node in nodes:
  clique = build_maximal_clique(node, nodes)
  if len(clique) > best[0]:
    best = len(clique), ",".join(sorted(clique))
print(best[1])
