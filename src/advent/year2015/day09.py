from collections import defaultdict


def search(graph, node, distance, visited):
    if len(visited) == len(graph):
        return [distance]
    results = []
    for neighbor, neighbor_distance in graph[node]:
        if neighbor in visited:
            continue
        results += search(
            graph, neighbor, distance + neighbor_distance, visited | {neighbor}
        )
    return results


def solve(data, func):
    graph = defaultdict(list)
    for line in data.splitlines():
        lhs, rhs = line.split(" = ")
        left, right = lhs.split(" to ")
        distance = int(rhs)
        graph[left].append((right, distance))
        graph[right].append((left, distance))
    graph = dict(graph)
    result = []
    for k in graph:
        result.append(func(search(graph, k, 0, set([k]))))
    return func(result)


def part1(data):
    return solve(data, min)


def part2(data):
    return solve(data, max)


def test_part1():
    data = """London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
"""
    assert part1(data) == 605


def test_part2():
    data = """London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
"""
    assert part2(data) == 982
