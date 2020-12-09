from utils import *

inp = get_input(2020, 9)

nums = [int(l) for l in inp.split("\n")]

"""
window = nums[:25]
nums = nums[25:]


def is_valid(n, w):
    for x in w:
        if n-x in w:
            return True
    return False

while nums:
    n = nums[0]
    nums = nums[1:]
    if not is_valid(n, window):
        print(n)
        break
    window = window[1:] + [n]
"""

tgt = 373803594

window = []

while nums:
    while sum(window) > tgt:
        window = window[1:]

    if sum(window) == tgt:
        print(min(window) + max(window))
        break

    window = window + nums[:1]
    nums = nums[1:]

