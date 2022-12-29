def parse(data):
    result = {}
    for line in data.splitlines():
        args = line.split()
        result[args[0]] = (int(args[3]), int(args[6]), int(args[13]))
    return result


def part1(data, total=2503):
    data = parse(data)
    result = []
    for speed, fly, rest in data.values():
        num_complete = total // (fly + rest)
        num_extra = min(fly, total - num_complete * (fly + rest))
        result.append((num_complete * fly + num_extra) * speed)
    return max(result)


def part2(data, total=2503):
    data = parse(data)
    scores = {k: 0 for k in data.keys()}
    state = {k: (True, 0, 0, 0) for k, v in data.items()}
    for t in range(total):
        for k, (speed, fly, rest) in data.items():
            moving, distance, fly_time, rest_time = state[k]
            if moving:
                distance += speed
                fly_time += 1
                if fly_time == fly:
                    moving = False
                    fly_time = 0
            else:
                rest_time += 1
                if rest_time == rest:
                    moving = True
                    rest_time = 0
            state[k] = (moving, distance, fly_time, rest_time)
        best_dist = max(v[1] for v in state.values())
        for k, v in state.items():
            if v[1] == best_dist:
                scores[k] += 1
    return max(scores.values())


def test_part1():
    data = """Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
"""
    assert part1(data, total=1000) == 1120


def test_part2():
    data = """Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
"""
    assert part2(data, total=1000) == 689
