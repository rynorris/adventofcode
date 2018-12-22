r2 = 0
r3 = 0
X = (2**24) - 1
seen = set()
while True:
    r2 = r3 | 65536
    r3 = 10736359
    while r2 > 0:
        r3 = r3 & X
        r3 = r3 + (r2 & 255)
        r3 = r3 * 65899
        r3 = r3 & X
        r2 = r2 / 256

    if r3 in seen:
        print "LOOP", r3
        break
    print r3
    seen.add(r3)
