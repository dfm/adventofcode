from collections import defaultdict
from itertools import permutations


def parse(data, include_me=False):
    graph = defaultdict(dict)
    for line in data.splitlines():
        elements = line.split()
        value = int(elements[3])
        if elements[2] == "lose":
            value = -value
        graph[elements[0]][elements[-1][:-1]] = value
    if include_me:
        graph["me"] = {k: 0 for k in graph.keys()}
        for k in graph.keys():
            graph[k]["me"] = 0
    return dict(graph)


def evaluate(graph, order):
    result = graph[order[0]][order[-1]] + graph[order[-1]][order[0]]
    for i in range(len(order) - 1):
        a, b = order[i], order[i + 1]
        result += graph[a][b] + graph[b][a]
    return result


def part1(data):
    graph = parse(data)
    return max(evaluate(graph, order) for order in permutations(graph.keys()))


def part2(data):
    graph = parse(data, include_me=True)
    return max(evaluate(graph, order) for order in permutations(graph.keys()))
