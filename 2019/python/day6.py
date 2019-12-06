from utils import get_input

inp = get_input(2019, 6)
lines = inp.split("\n")

orbits = [l.split(")") for l in lines if l != ""]

objs = set(o for orb in orbits for o in orb)

orbit_map = {}

for orb in orbits:
    p = orb[0]
    c = orb[1]
    if c not in orbit_map:
        orbit_map[c] = set()

    orbit_map[c].add(p)


def num_orb(obj):
    direct = orbit_map.get(obj, set())
    count = len(direct)

    indirect_count = sum([num_orb(p) for p in direct])
    return count + indirect_count


def neighbours(obj):
    orbs = [o for o in orbits if o[0] == obj or o[1] == obj]
    os = set([o for orb in orbs for o in orb])
    os.remove(obj)
    return os


def dist(a, b):
    seen = {}

    cur = [(a, 0)]
    while cur:
        (o, d) = cur.pop()
        if o == b:
            return d

        seen[o] = d

        ns = [(n, d+1) for n in neighbours(o) if n not in seen]
        cur = cur + ns


print(sum([num_orb(o) for o in objs]))
print(dist("YOU", "SAN") - 2)
