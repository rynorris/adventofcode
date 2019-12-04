from utils import get_input

minP = 128392
maxP = 643281


def has_dupe(s):
    for ix in range(len(s) - 1):
        if s[ix] == s[ix + 1]:
            if ix > 0 and s[ix-1] == s[ix]:
                continue
            if ix < len(s) - 2 and s[ix+2] == s[ix]:
                continue
            return True
    return False


def never_decreases(s):
    for ix in range(len(s) - 1):
        if s[ix] > s[ix+1]:
            return False
    return True

def is_in_range(s):
    intval = int(s)
    return intval <= maxP and intval >= minP


def is_valid(s):
    return has_dupe(s) and never_decreases(s) and is_in_range(s)


count = 0
for ix in range(minP, maxP+1):
    s = str(ix)
    if  is_valid(s):
        count += 1


print(count)
