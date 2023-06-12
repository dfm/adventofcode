def part1(data):
    parts = data.split()
    target_row = int(parts[-3][:-1])
    target_col = int(parts[-1][:-1])
    x = 20151125
    row = 0
    col = 0
    while True:
        x = (x * 252533) % 33554393
        row -= 1
        col += 1
        if row < 0:
            row = col
            col = 0
        if row + 1 == target_row and col + 1 == target_col:
            return x
