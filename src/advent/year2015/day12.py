import json

def recurse(data):
    if isinstance(data, int):
        return data
    elif isinstance(data, list):
        return sum(recurse(x) for x in data)
    elif isinstance(data, dict):
        return sum(recurse(x) for x in data.values())
    return 0

def part1(data):
    data = json.loads(data)
    return recurse(data)
    
def recurse2(data):
    if isinstance(data, int):
        return data
    elif isinstance(data, list):
        return sum(recurse2(x) for x in data)
    elif isinstance(data, dict):
        if "red" in data.values():
            return 0
        return sum(recurse2(x) for x in data.values())
    return 0

def part2(data):
    data = json.loads(data)
    return recurse2(data)
    