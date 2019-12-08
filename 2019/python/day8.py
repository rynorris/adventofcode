from utils import get_input

inp = get_input(2019, 8).strip()

W = 25
H = 6

LAYER = W * H



def part_a(pic):
    layers = []
    while pic:
        layers.append(pic[:LAYER])
        pic = pic[LAYER:]
    N = 100000
    L = None
    for layer in layers:
        n = len([p for p in layer if p == "0"])
        if n < N:
            N = n
            L = layer


    ones = len([p for p in L if p == "1"])
    twos = len([p for p in L if p == "2"])

    print(ones * twos)



def part_b(pic):
    img = [None] * LAYER
    for ix, p in enumerate(pic):
        px = ix % LAYER

        if img[px] is not None:
            continue

        if p == "0":
            img[px] = " "
        elif p == "1":
            img[px] = "#"



    rows = []
    while img:
        rows.append(img[:W])
        img = img[W:]

    print("\n".join(["".join(row) for row in rows]))


part_a(inp)
part_b(inp)

