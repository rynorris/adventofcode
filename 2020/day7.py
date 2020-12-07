import re
from utils import *

inp = get_input(2020, 7)

BAG_PAT = re.compile(r"(.*) bags")
BAG_NUM_PAT = re.compile(r"([0-9]+) (.*) bags?")
RULE_PAT = re.compile(r"(.*) contain (.*).")

def parse_rule(l):
    m = RULE_PAT.match(l)
    src = BAG_PAT.match(m.groups()[0]).groups()[0]
    if m.groups()[1] == "no other bags":
        tgts = []
    else:
        ms = [BAG_NUM_PAT.match(x) for x in m.groups()[1].split(", ")]
        tgts = [(int(m.groups()[0]), m.groups()[1]) for m in ms]

    return src, tgts


rules = [parse_rule(l) for l in inp.split("\n")]

contains = {src:tgts for src, tgts in rules}

contained_in = {}
for src, tgts in rules:
    for (_, tgt) in tgts:
        if tgt not in contained_in:
            contained_in[tgt] = []

        contained_in[tgt].append(src)


seen = set()
q = ["shiny gold"]
while q:
    bag = q.pop()
    seen.add(bag)
    parents = contained_in.get(bag, [])
    q += [p for p in parents if p not in seen]

print(seen)
print(len(seen) - 1)

def count_bags(num, col):
    children = contains.get(col, [])
    if children:
        total = 1 + sum([count_bags(n, c) for n, c in children])
    else:
        total = 1
    return num * total

print(count_bags(1, "shiny gold") - 1)

