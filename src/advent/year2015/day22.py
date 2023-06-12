import heapq
from typing import List, NamedTuple


class GameState(NamedTuple):
    boss_hp: int
    boss_damage: int

    player_hp: int
    spend: int
    mana: int
    shield: int
    poison: int
    recharge: int

    @property
    def is_loss(self) -> bool:
        return self.player_hp <= 0

    @property
    def is_win(self) -> bool:
        return self.boss_hp <= 0


def apply_effects(state: GameState) -> GameState:
    if state.poison > 0:
        state = state._replace(boss_hp=state.boss_hp - 3, poison=state.poison - 1)
    if state.recharge > 0:
        state = state._replace(mana=state.mana + 101, recharge=state.recharge - 1)
    if state.shield > 0:
        state = state._replace(shield=state.shield - 1)
    return state


def boss_turn(state: GameState) -> GameState:
    state = apply_effects(state)
    if state.is_win:
        return state
    damage = max(1, (state.boss_damage - 7) if state.shield else state.boss_damage)
    return state._replace(player_hp=state.player_hp - damage)


def player_turn(state: GameState, hard: bool) -> List[GameState]:
    if hard:
        state = state._replace(player_hp=state.player_hp - 1)
        if state.is_loss:
            return []

    state = apply_effects(state)
    proposals = []
    if state.mana >= 53:
        proposals.append(
            state._replace(
                mana=state.mana - 53, boss_hp=state.boss_hp - 4, spend=state.spend + 53
            )
        )
    if state.mana >= 73:
        proposals.append(
            state._replace(
                mana=state.mana - 73,
                boss_hp=state.boss_hp - 2,
                player_hp=state.player_hp + 2,
                spend=state.spend + 73,
            ),
        )
    if state.mana >= 113 and state.shield == 0:
        proposals.append(
            state._replace(mana=state.mana - 113, shield=6, spend=state.spend + 113),
        )
    if state.mana >= 173 and state.poison == 0:
        proposals.append(
            state._replace(mana=state.mana - 173, poison=6, spend=state.spend + 173)
        )
    if state.mana >= 229 and not state.recharge:
        proposals.append(
            state._replace(mana=state.mana - 229, recharge=5, spend=state.spend + 229)
        )
    proposals = (boss_turn(p) for p in proposals)
    return [p for p in proposals if not p.is_loss]


def simulate(state: GameState, hard: bool = False) -> int:
    cost = {state: 0}
    queue = [(0, state)]
    while queue:
        _, state = heapq.heappop(queue)
        if state.is_win:
            return state.spend
        for proposal in player_turn(state, hard=hard):
            if proposal not in cost or cost[proposal] > proposal.spend:
                cost[proposal] = proposal.spend
                heapq.heappush(queue, (proposal.spend, proposal))

    raise ValueError("unsolvable")


def parse(data):
    a, b = data.splitlines()
    return GameState(
        boss_hp=int(a.split(": ")[1]),
        boss_damage=int(b.split(": ")[1]),
        player_hp=50,
        spend=0,
        mana=500,
        shield=0,
        poison=0,
        recharge=0,
    )


def part1(data):
    state = parse(data)
    return simulate(state)


def part2(data):
    state = parse(data)
    return simulate(state, hard=True)


def test_simulate1():
    state = GameState(
        boss_hp=13,
        boss_damage=8,
        player_hp=10,
        spend=0,
        mana=250,
        shield=0,
        poison=0,
        recharge=0,
    )
    assert simulate(state) == 226


def test_simulate2():
    state = GameState(
        boss_hp=14,
        boss_damage=8,
        player_hp=10,
        spend=0,
        mana=250,
        shield=0,
        poison=0,
        recharge=0,
    )
    assert simulate(state) == 641
