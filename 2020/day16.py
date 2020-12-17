import re
from utils import *

inp = get_input(2020, 16)


sections = inp.split("\n\n")

RULE = re.compile("(.*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)")
def parse_rule(l):
    m = RULE.match(l)
    field = m.group(1)
    l1 = m.group(2)
    u1 = m.group(3)
    l2 = m.group(4)
    u2 = m.group(5)
    return field, (int(l1), int(u1), int(l2), int(u2))

rules = {rule[0]: rule[1] for rule in [parse_rule(l) for l in sections[0].split("\n")]}

print(rules)

tickets = [[int(x) for x in l.split(",")] for l in sections[2].split("\n")[1:]]

def matches(bounds, value):
    l1, u1, l2, u2 = bounds
    if l1 <= value <= u1:
        return True

    if l2 <= value <= u2:
        return True

    return False


def error_rate(ticket):
    err = 0
    for val in ticket:
        if all([not matches(bounds, val) for bounds in rules.values()]):
            err += val
            break
    return err

def is_valid(ticket):
    for val in ticket:
        if all([not matches(bounds, val) for bounds in rules.values()]):
            return False
    return True

print(sum([error_rate(t) for t in tickets]))

my_ticket = [[int(x) for x in l.split(",")] for l in sections[1].split("\n")[1:]][0]

valid_tickets = [t for t in tickets if is_valid(t)] + [my_ticket]

possible = {field: set(range(20)) for field in rules.keys()}


def is_possible(field, val):
    bounds = rules[field]
    return matches(bounds, val)


for ticket in valid_tickets:
    for field, cur in possible.items():
        pos = [ix for ix, val in enumerate(ticket) if matches(rules[field], val)]
        possible[field] = cur.intersection(set(pos))
        if 1 in cur and 1 not in pos:
            print("Excluding pos 1 for field {} based on bounds: {} from ticket: {}".format(field, rules[field], ticket))


print(possible)
print([len(l) for l in possible.values()])

while True:
    decided = [(f, pos) for f, pos in possible.items() if len(pos) == 1]
    if len(decided) == len(possible):
        break

    taken = set([x for d in decided for x in d[1]])
    new = {f: pos - taken if len(pos) > 1 else pos for f, pos in possible.items()}
    if new == possible:
        raise Exception("FAILED")
    else:
        possible = new

for f, ix in decided:
    print(ix, f)

vals = [pos.pop() for f, pos in decided if f.startswith("departure")]

total = 1
for ix in vals:
    total *= my_ticket[ix]

print(total)
