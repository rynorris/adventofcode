from collections import defaultdict
from utils import *

inp = get_input(2021, 3)

lines = inp.split("\n")

position_bits = [[l[ix] for l in lines] for ix in range(len(lines[0]))]

def count(bits):
    counts = defaultdict(int)
    for b in bits:
        counts[b] += 1

    return counts


def most_common(bits):
    counts = count(bits)
    if counts["0"] > counts["1"]:
        return "0"
    else:
        return "1"


def least_common(bits):
    counts = count(bits)
    if counts["0"] <= counts["1"]:
        return "0"
    else:
        return "1"


gamma_bits = "".join([most_common(bits) for bits in position_bits])
epsilon_bits = "".join([least_common(bits) for bits in position_bits])
gamma = int(gamma_bits, 2)
epsilon = int(epsilon_bits, 2)

print(gamma * epsilon)

filt = [l for l in lines]
for ix in range(len(lines[0])):
    bit = most_common([l[ix] for l in filt])
    filt = [l for l in filt if l[ix] == bit]
    if len(filt) == 1:
        oxy = int(filt[0], 2)

filt = [l for l in lines]
for ix in range(len(lines[0])):
    bit = least_common([l[ix] for l in filt])
    filt = [l for l in filt if l[ix] == bit]
    if len(filt) == 1:
        co2 = int(filt[0], 2)

print(oxy * co2)
