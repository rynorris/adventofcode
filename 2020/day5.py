from utils import *

inp = get_input(2020, 5)

def parse_seat(s):
    lb = 0
    ub = 128
    for c in s[:7]:
        if c == "B":
            lb = (lb + ub) / 2
        else:
            ub = (lb + ub) / 2

    row = lb

    lb = 0
    ub = 8
    for c in s[7:]:
        if c == "R":
            lb = (lb + ub) / 2
        else:
            ub = (lb + ub) / 2

    col = lb

    return (row, col)


def seat_id(row, col):
    return row * 8 + col


seats = [parse_seat(s) for s in inp.split("\n")]

ids = [seat_id(row, col) for row, col in seats]

print(max(ids))

ids = sorted(ids)
for ix in range(len(ids) - 1):
    if ids[ix+1] != ids[ix] + 1:
        print(ix, ids[ix], ids[ix+1])

