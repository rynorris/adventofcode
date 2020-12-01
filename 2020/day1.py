from utils import *

inp = get_input(2020, 1)

nums = [int(n) for n in inp.strip().split("\n")]

diffs = set([2020 - n for n in nums])

for n in nums:
    if n in diffs:
        print(n, 2020-n, n * (2020 - n))


pairwise_sums = [(x, y, x + y) for x in nums for y in nums]
print(len(pairwise_sums))

for x, y, p in pairwise_sums:
    if p in diffs:
        print(x, y, 2020-p, p, x * y * (2020 - p))
