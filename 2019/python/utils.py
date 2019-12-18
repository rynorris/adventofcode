import sys
import os

try:
    import requests
except:
    print("Requests not found.  Will not be able to retrieve remote input.")

SESSION = os.getenv("SESSION")

INPUTS_DIR = os.path.join(os.path.dirname(__file__), "inputs")

def print_grid_dict(grid, charmap=None, default=0):
    # Expects a grid of (x, y) -> val
    # Charmap maps vals to tokens for printing.  Otherwise prints str(val).
    min_x = min([x for x, y in grid.keys()])
    max_x = max([x for x, y in grid.keys()])
    min_y = min([y for x, y in grid.keys()])
    max_y = max([y for x, y in grid.keys()])
    for y in range(min_y, max_y+1):
        for x in range(min_x, max_x+1):
            v = grid.get((x, y), default)
            if charmap:
                v = charmap[v]
            sys.stdout.write(str(v))
        sys.stdout.write("\n")


def get_input(year, day):
    cached_input = _get_cached_input(year, day)
    if cached_input:
        print(f"Found cached input for {year} day {day}")
        return cached_input

    print(f"No cached input for {year} day {day}, fetching from server...")
    url = f"https://adventofcode.com/{year}/day/{day}/input"
    headers = {
        "Cookie": f"session={SESSION}",
    }

    resp = requests.get(url, headers=headers)
    resp.raise_for_status()

    print(f"Fetched input for {year} day {day}.  Caching for later.")
    _cache_input(year, day, resp.text)
    return resp.text


def _get_cached_input(year, day):
    path = _cached_input_path(year, day)
    if os.path.exists(path):
        with open(path) as f:
            return f.read()

    return None

def _cache_input(year, day, text):
    if not os.path.exists(INPUTS_DIR):
        os.mkdir(INPUTS_DIR)
    path = _cached_input_path(year, day)
    with open(path, 'w') as f:
        f.write(text)

def _cached_input_path(year, day):
    filename = f"{year}_{day}.txt"
    return os.path.join(INPUTS_DIR, filename)
