import z3

def parse():
    with open("inputs/23a.txt") as f:
        lines = f.readlines()

    def parse_line(l):
        l = l.replace("pos=<", "")
        l = l.replace("r=", "")
        l = l.replace(">", "")
        l = l.replace(",", " ")
        ns = l.split()
        return [int(n) for n in ns]

    return [parse_line(l) for l in lines]

def diff(a, b):
    return z3.If(a >= b, a - b, b - a)

def solve(bots):
    opt = z3.Optimize()

    x = z3.Int('x')
    y = z3.Int('y')
    z = z3.Int('z')

    distance_from_origin = x + y + z

    bot_vals = []
    for bot in bots:
        in_range = diff(bot[0], x) + diff(bot[1], y) + diff(bot[2], z) <= bot[3]
        bot_vals.append(z3.If(in_range, 1, 0))

    bots_in_range = z3.Sum(bot_vals)

    print(bots_in_range)
    opt.maximize(bots_in_range)
    opt.minimize(distance_from_origin)

    print(opt.check())
    print(opt.model())

if __name__ == "__main__":
    bots = parse()
    solve(bots)
