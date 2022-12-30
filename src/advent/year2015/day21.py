from math import ceil
from itertools import combinations, product

weapons = [
    (8, 4, 0),
    (10, 5, 0),
    (25, 6, 0),
    (40, 7, 0),
    (74, 8, 0),
]
armor = [
    (0, 0, 0),
    (13, 0, 1),
    (31, 0, 2),
    (53, 0, 3),
    (75, 0, 4),
    (102, 0, 5),
]
rings = [
    (0, 0, 0),
    (0, 0, 0),
    (25, 1, 0),
    (50, 2, 0),
    (100, 3, 0),
    (20, 0, 1),
    (40, 0, 2),
    (80, 0, 3),
]


def simulate(player_hp, player_damage, player_armor, boss_hp, boss_damage, boss_armor):
    player_turn = max(1, player_damage - boss_armor)
    player_turns = ceil(boss_hp / player_turn)
    boss_turn = max(1, boss_damage - player_armor)
    boss_turns = ceil(player_hp / boss_turn)
    return player_turns <= boss_turns


def part1(data):
    boss_hp, boss_damage, boss_armor = (
        int(line.split()[-1]) for line in data.splitlines()
    )
    best = None
    player_hp = 100
    for inv in product(weapons, armor, *zip(*combinations(rings, 2))):
        cost = sum(i[0] for i in inv)
        player_damage = sum(i[1] for i in inv)
        player_armor = sum(i[2] for i in inv)
        if simulate(
            player_hp, player_damage, player_armor, boss_hp, boss_damage, boss_armor
        ):
            best = cost if best is None else min(best, cost)
    return best


def part2(data):
    boss_hp, boss_damage, boss_armor = (
        int(line.split()[-1]) for line in data.splitlines()
    )
    best = None
    player_hp = 100
    for inv in product(weapons, armor, *zip(*combinations(rings, 2))):
        cost = sum(i[0] for i in inv)
        player_damage = sum(i[1] for i in inv)
        player_armor = sum(i[2] for i in inv)
        if not simulate(
            player_hp, player_damage, player_armor, boss_hp, boss_damage, boss_armor
        ):
            best = cost if best is None else max(best, cost)
    return best


def test_simulate():
    assert simulate(8, 5, 5, 12, 7, 2)
