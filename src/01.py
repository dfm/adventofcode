from collections import Counter

left, right = zip(*(map(int, line.split()) for line in open("data/01.txt")))
print(sum(abs(x - y) for x, y in zip(sorted(left), sorted(right))))

right = Counter(right)
print(sum(x * right[x] for x in left))
