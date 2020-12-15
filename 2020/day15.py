from utils import *

inp = get_input(2020, 15)

start = [int(x) for x in inp.split(",")]

last = {}

q = [s for s in start]

n = 0
while True:
    x = q.pop(0)
    if not q:
        l = last.get(x, None)
        q.append(n - l if l is not None else 0)
    last[x] = n
    n += 1
    if n == 30000000:
        print(x)
        break
