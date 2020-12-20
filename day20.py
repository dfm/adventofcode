import numpy as np

monster = (
    np.array(
        list(
            map(
                list,
                """                  #.
#    ##    ##    ###
 #  #  #  #  #  #...
""".splitlines(),
            )
        )
    )
    == "#"
)

with open("inputs/day20", "r") as f:
    data = f.read().split("\n\n")

dim = int(np.sqrt(len(data)))
data = [tile.splitlines() for tile in data]
ids = np.empty(len(data), dtype=int)
tiles = np.empty((len(data), len(data[0]) - 1, len(data[0][0])), dtype=int)
for n, tile in enumerate(data):
    ids[n] = int(tile[0].strip(":").split()[1])
    tiles[n] = np.array(list(map(list, tile[1:]))) == "#"


def ident(t):
    return t


def rotate_90(t):
    return t[::-1, :].T


def rotate_180(t):
    return t[::-1, ::-1]


def rotate_270(t):
    return t[:, ::-1].T


def transpose(t):
    return t.T


def flip_y(t):
    return t[::-1, :]


def flip_x(t):
    return t[:, ::-1]


def compose(a, b):
    return lambda x: a(b(x))


ops = [
    ident,
    rotate_90,
    rotate_180,
    rotate_270,
    transpose,
    flip_x,
    flip_y,
    compose(transpose, rotate_180),
]

# Find all the edges that connect
unused = set(range(1, len(data)))
placed = [0]
adj = -1 + np.zeros((len(data), 4), dtype=int)

sides = (
    (0, slice(None)),  # top
    (slice(None), -1),  # right
    (-1, slice(None)),  # bottom
    (slice(None), 0),  # left
)

while len(unused):
    for n in placed:
        for side in range(4):
            if adj[n, side] >= 0:
                continue

            to_match = tiles[(n,) + sides[side]]
            for m in range(len(data)):
                if m == n:
                    continue
                sel = sides[(side + 2) % 4]
                other = tiles[m]

                for op in ops if m in unused else [ident]:
                    if np.any(op(other)[sel] != to_match):
                        continue
                    tiles[m] = np.ascontiguousarray(op(other))
                    unused -= {m}
                    placed.append(m)
                    adj[n, side] = m
                    adj[m, (side + 2) % 4] = n
                    break


print("Part 1:", np.prod(ids[np.sum(adj == -1, axis=-1) == 2]))


# Construct the image
dx = tiles[0].shape[0] - 2
dy = tiles[0].shape[1] - 2
img = -1 + np.zeros((dim * dx, dim * dy), dtype=int)

ind = np.argmax((adj[:, 0] == -1) & (adj[:, -1] == -1))
coords = np.zeros(2, dtype=int)
unplaced = set(range(len(data)))
sides = [
    np.array([-1, 0]),
    np.array([0, 1]),
    np.array([1, 0]),
    np.array([0, -1]),
]


def place_tile_and_neighbors(coords, ind):
    global unplaced
    unplaced -= {ind}
    img[
        coords[0] * dx : (coords[0] + 1) * dx,
        coords[1] * dy : (coords[1] + 1) * dy,
    ] = tiles[ind][1:-1, 1:-1]
    for side in range(4):
        if adj[ind, side] < 0 or adj[ind, side] not in unplaced:
            continue
        place_tile_and_neighbors(coords + sides[side], adj[ind, side])


place_tile_and_neighbors(coords, ind)
# print("\n".join(map("".join, np.array([".", "#"])[flip_y(img)])))

mx, my = monster.shape


def count_monsters(img):
    count = 0
    for i in range(img.shape[0] - mx):
        for j in range(img.shape[1] - my):
            count += np.all(img[i : i + mx, j : j + my][monster] == 1)
    return count


print(
    "Part 2:",
    np.sum(img) - np.sum(monster) * max(count_monsters(op(img)) for op in ops),
)
