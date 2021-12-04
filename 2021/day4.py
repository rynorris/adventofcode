from utils import *

inp = get_input(2021, 4)

sections = inp.split("\n\n")

nums = [int(x) for x in sections[0].split(",")]

def parse_board(sec):
    return [[int(x) for x in l.split()] for l in sec.split("\n")]

boards = [parse_board(sec) for sec in sections[1:]]

def wins(board, nums):
    for row in board:
        if all([x in nums for x in row]):
            return True

    for ix in range(len(board[0])):
        if all([r[ix] in nums for r in board]):
            return True

    return False


def score(board, nums, last):
    return sum([x for r in board for x in r if x not in nums]) * last


for ix in range(len(nums)):
    winners = [b for b in boards if wins(b, set(nums[:ix]))]
    if winners:
        print(score(winners[0], nums[:ix], nums[:ix][-1]))
        break


for ix in range(len(nums)):
    winners = [b for b in boards if not wins(b, set(nums[:ix]))]
    if len(winners) == 0:
        winner = [b for b in boards if not wins(b, set(nums[:ix-1]))][0]
        print(score(winner, nums[:ix], nums[:ix][-1]))
        break
