from utils import *

inp = get_input(2020, 10)

inp2 = """28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"""

nums = [int(i) for i in inp.split()]

builtin = max(nums) + 3

nums = sorted(nums) + [builtin]

c = 0
ones = 0
threes = 0
for n in nums:
    if n - c == 1:
        ones += 1
    elif n - c == 3:
        threes += 1
    c = n

print(ones * threes)

cache = {}

def arrangements(adapters):
    tgt = adapters[-1]
    if tgt in cache:
        return cache[tgt]

    if tgt == 0:
        return 1
    srcs = [a for a in adapters[-4:] if 1 <= tgt - a <= 3]
    ans = sum([arrangements([a for a in adapters if a <= s]) for s in srcs])
    cache[tgt] = ans
    return ans


arrs = arrangements([0] + nums)
print(arrs)

#print(arrangements(0, nums))

