from functools import reduce
from utils import *

inp = get_input(2020, 6)
groups = inp.split("\n\n")

def count_group(g):
    return len(set([c for c in g if "a" <= c <= "z"]))

print(sum([count_group(g) for g in groups]))

def count_group_2(g):
    people = [set([c for c in l if "a" <= c <= "z"]) for l in g.split("\n")]
    return len(reduce(lambda s, t: s.intersection(t), people))

print(sum([count_group_2(g) for g in groups]))

