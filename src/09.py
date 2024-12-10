data = open("data/09.txt").read().strip()

def index_files(data):
  files = []
  idx = 0
  for i in range(0, len(data), 2):
    size = int(data[i])
    files.append((i // 2, idx, size))
    if i + 1 < len(data):
      size += int(data[i + 1])
    idx += size
  return files

def shift_partial(files):
  gaps = ((files[i][1] + files[i][2], files[i + 1][1]) for i in range(len(files) - 1))
  try:
    gap, gap_start, gap_end = next((i + 1, s, e) for i, (s, e) in enumerate(gaps) if e > s)
  except StopIteration:
    return False
  id, idx, size = files.pop()
  delta = min(gap_end - gap_start, size)
  files.insert(gap, (id, gap_start, delta))
  if size > delta:
    files.append((id, idx, size - delta))
  return True

def checksum(files):
  total = 0
  for id, start, size in files:
    total += id * sum(range(start, start + size))
  return total

def solve1(data):
  files = index_files(data)
  while shift_partial(files):
    pass
  return checksum(files)

print(solve1(data))

def shift_full(files, todo):
  pos, (id, _, size) = next((i, f) for i, f in enumerate(files) if f[0] == todo)
  gaps = ((files[i][1] + files[i][2], files[i + 1][1]) for i in range(len(files) - 1))
  try:
    gap, gap_start = next((i + 1, s) for i, (s, e) in enumerate(gaps) if e - s >= size)
  except StopIteration:
    return
  if gap > pos:
    return
  files.pop(pos)
  files.insert(gap, (id, gap_start, size))

def solve2(data):
  files = index_files(data)
  ids = [i for i, *_ in files]
  for i in ids[::-1]:
    shift_full(files, i)
  return checksum(files)

print(solve2(data))
