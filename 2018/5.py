
def shouldCancel(c, d):
    return c.lower() == d.lower() and c != d

def findRemovalIx(s):
    for ix in range(len(s)):
        if ix < (len(s) - 1) and shouldCancel(s[ix], s[ix+1]):
            return ix
    return -1

def simplify(s):
    ix = findRemovalIx(s)
    if ix == -1:
        return s
    return s[:ix] + s[ix+2:]

def simplest(s):
    simpler = simplify(s)
    while simpler != s:
        s = simpler
        simpler = simplify(s)

    return s

def solve_5a():
    with open("inputs/5a.txt") as f:
        polymer = f.read()

    print len(simplest(polymer))

def solve_5b():
    with open("inputs/5a.txt") as f:
        polymer = f.read().strip()

    letters = set([c for c in polymer.lower()])
    print letters
    results = {}
    for c in letters:
        print("Running for: " + c)
        reduced = polymer.replace(c, "").replace(c.upper(), "")
        results[c] = len(simplest(reduced))

    for (k, v) in results.iteritems():
        print("{}: {}".format(k, v))

if __name__ == '__main__':
    solve_5b()

