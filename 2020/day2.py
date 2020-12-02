from utils import *

inp = get_input(2020, 2)

# example: 2-10 l: dxkrwjbvlsgpzcmk

lines = inp.strip().split("\n")

def parse_line(line):
    parts = line.split()
    times = parts[0]
    letter = parts[1][0]
    pw = parts[2]
    mini = int(times.split("-")[0])
    maxi = int(times.split("-")[1])

    return (mini, maxi, letter, pw)


def check_pw(mini, maxi, letter, pw):
    count = len([l for l in pw if l == letter])
    return mini <= count <= maxi

def check_pw2(mini, maxi, letter, pw):
    min_match = pw[mini-1] == letter
    max_match = pw[maxi-1] == letter
    return min_match ^ max_match


def check_line(l):
    mini, maxi, letter, pw = parse_line(l)
    return check_pw(mini, maxi, letter, pw)

def check_line2(l):
    mini, maxi, letter, pw = parse_line(l)
    return check_pw2(mini, maxi, letter, pw)


print(len([l for l in lines if check_line(l)]))
print(len([l for l in lines if check_line2(l)]))
