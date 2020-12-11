from utils import *

inp = get_input(2020, 11)

grid = {}
rows = inp.split("\n")
for rix, row in enumerate(rows):
    for cix, val in enumerate(row):
        grid[(rix, cix)] = val


def adjacent(grid, x, y):
    seats = [
            grid.get((x+1, y), None),
            grid.get((x-1, y), None),
            grid.get((x+1, y+1), None),
            grid.get((x, y+1), None),
            grid.get((x-1, y+1), None),
            grid.get((x+1, y-1), None),
            grid.get((x, y-1), None),
            grid.get((x-1, y-1), None),
    ]
    return [s for s in seats if s is not None]

def first_seat(grid, x, y, dx, dy):
    x += dx
    y += dy
    s = grid.get((x, y), None)
    while s is not None:
        if s != ".":
            return s
        x += dx
        y += dy
        s = grid.get((x, y), None)

    return None

def visible(grid, x, y):
    seats = [
            first_seat(grid, x, y, 1, 0),
            first_seat(grid, x, y, -1, 0),
            first_seat(grid, x, y, 1, 1),
            first_seat(grid, x, y, 0, 1),
            first_seat(grid, x, y, -1, 1),
            first_seat(grid, x, y, 1, -1),
            first_seat(grid, x, y, 0, -1),
            first_seat(grid, x, y, -1, -1),
    ]
    return [s for s in seats if s is not None]


def step(grid):
    new = {}
    changed = False
    for (x, y), val in grid.items():
        adj = adjacent(grid, x, y)
        occ = len([s for s in adj if s == "#"])
        if val == "L" and occ == 0:
            new[(x, y)] = "#"
            changed = True
        elif val == "#" and occ >= 4:
            new[(x, y)] = "L"
            changed = True
        else:
            new[(x, y)] = val

    return (new, changed)

def step2(grid):
    new = {}
    changed = False
    for (x, y), val in grid.items():
        adj = visible(grid, x, y)
        occ = len([s for s in adj if s == "#"])
        if val == "L" and occ == 0:
            new[(x, y)] = "#"
            changed = True
        elif val == "#" and occ >= 5:
            new[(x, y)] = "L"
            changed = True
        else:
            new[(x, y)] = val

    return (new, changed)



changed = True
print_grid_dict(grid)
grid, changed = step2(grid)
print_grid_dict(grid)
while changed:
    grid, changed = step2(grid)

print(len([s for s in grid.values() if s == "#"]))

