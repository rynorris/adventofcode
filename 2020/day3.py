from utils import *

inp = get_input(2020, 3)


rows = [r for r in inp.split("\n") if r != ""]

def get(x, y):
    return rows[y][x % len(rows[y])]


def calc(dx, dy):
    x = 0
    y = 0
    trees = 0
    while y < len(rows):
        trees += 1 if get(x, y) == "#" else 0
        x += dx
        y += dy
    return trees

print(calc(3, 1))

grads = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

vals = [calc(dx, dy) for dx, dy in grads]

prod = 1
for val in vals:
    prod *= val

print(prod)
