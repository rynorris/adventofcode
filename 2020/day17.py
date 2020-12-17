from utils import *
import itertools

inp = get_input(2020, 17)


def initial(inp, dim):
    state = {}
    for y, row in enumerate(inp.split("\n")):
        for x, c in enumerate(row):
            coord = [0] * dim
            coord[0] = x
            coord[1] = y
            state[tuple(coord)] = c
    return state


def mask_slice(state, mask):
    def matches(coord):
        return all([y is None or x == y for x, y in zip(coord, mask)])

    def project(coord):
        return tuple([c for c, m in zip(coord, mask) if m is None])

    return {project(c): v for c, v in state.items() if matches(c)}


def add_tuples(t, s):
    return tuple([a + b for a, b in zip(t, s)])


def neighbours(c):
    return [add_tuples(c, dc) for dc in itertools.product(*([[-1,0,1]] * len(c))) if not dc == tuple([0] * len(c))]


def step(state):
    new = {}

    allcoords = set([n for c in state.keys() for n in neighbours(c)])
    for c in allcoords:
        s = state.get(c, ".")
        num_active = len([1 for c in neighbours(c) if state.get(c, ".") == "#"])
        if s == "#" and num_active in [2, 3]:
            new[c] = "#"
        elif s == "." and num_active == 3:
            new[c] = "#"

    return new

def part_a():
    state = initial(inp, 3)

    for ix in range(6):
        state = step(state)
    print(len([c for c in state.values() if c == "#"]))

def part_b():
    state = initial(inp, 4)

    for ix in range(6):
        state = step(state)
    print(len([c for c in state.values() if c == "#"]))

part_a()
part_b()

