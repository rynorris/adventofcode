from utils import *

inp = get_input(2020, 18)

example = "2 * 3 + (4 * 5)"

def chunks(l):
    chunks = []
    while l:
        if l[0] != "(":
            print(chunks)
            chunks.append(l[0])
            l = l[2:]
            continue

        depth = 0
        s = ""
        for ix, c in enumerate(l):
            s += c
            if c == "(":
                depth += 1
            elif c == ")":
                depth -= 1
                if depth == 0:
                    chunks.append(s[1:-1])
                    l = l[len(s):].strip()
                    break
    return chunks

def evaluate(ast):
    if len(ast) == 1:
        return int(ast)

    left, op, right = ast
    if op == "*":
        res = evaluate(left) * evaluate(right)
    elif op == "+":
        res = evaluate(left) + evaluate(right)
    else:
        raise Exception("Invalid op: " + op)

    return res

def hack(expr):
    return expr[::-1].replace("(", "!").replace(")", "(").replace("!", ")")

def listfind(l, item):
    for ix, v in enumerate(l):
        if v == item:
            return ix
    return -1

def ev2(l):
    if l.isdigit():
        return int(l)

    cs = chunks(l)
    while len(cs) > 1:
        opIx = listfind(cs, "+")
        if opIx == -1:
            opIx = listfind(cs, "*")

        left = ev2(cs[opIx-1])
        op = cs[opIx]
        right = ev2(cs[opIx+1])
        res = (left + right) if op == "+" else (left * right)

        cs = cs[:opIx-1] + [str(res)] + cs[opIx+2:]

    return ev2(cs[0])


print(sum([ev2(l) for l in inp.split("\n")]))
