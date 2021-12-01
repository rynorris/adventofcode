from utils import *

inp = get_input(2021, 1)

nums = [int(x) for x in inp.split("\n")]

c = 0
for ix in range(len(nums) - 3):
    if sum(nums[ix+1:ix+4]) > sum(nums[ix:ix+3]):
        c += 1

print(c)
