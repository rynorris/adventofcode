from utils import *

inp = get_input(2020, 4)

test_inp = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"""

def parse(s):
    fields = s.split()
    return { p.split(":")[0]: p.split(":")[1] for p in fields }


req = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

passports = [parse(s) for s in inp.split("\n\n")]

def is_valid(p):
    return len([f for f in req if f not in p]) == 0

print(len([p for p in passports if is_valid(p)]))


def validate_hgt(x):
    if x.endswith("cm"):
        return 150 <= int(x[:-2]) <= 193
    elif x.endswith("in"):
        return 59 <= int(x[:-2]) <= 76
    else:
        return False


def is_number(x):
    return len([c for c in x if c in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]]) == len(x),


validators = {
        "byr": lambda x: len(x) == 4 and 1920 <= int(x) <= 2002,
        "iyr": lambda x: len(x) == 4 and 2010 <= int(x) <= 2020,
        "eyr": lambda x: len(x) == 4 and 2020 <= int(x) <= 2030,
        "hgt": validate_hgt,
        "hcl": lambda x: x.startswith("#") and len(x) == 7 and len([c for c in x if c in ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"]]) == 6,
        "ecl": lambda x: x in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
        "pid": lambda x: len(x) == 9 and is_number(x)
}

def is_really_valid(p):
    for key, valid in validators.items():
        if key not in p:
            return False
        if not valid(p[key]):
            return False
    return True

print(len([p for p in passports if is_really_valid(p)]))
