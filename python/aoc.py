import re
from itertools import chain, product


cat = ''.join
inf = float('inf')
flatten = chain.from_iterable


def mapl(f, iterable):
    return list(map(f, iterable))

def mapt(f, iterable):
    return tuple(map(f, iterable))

def filterl(f, iterable):
    return list(filter(f, iterable))


def digits(line):
    return mapl(int, line)

def ints(text, negative=True):
    return mapt(int, re.findall(r"-?\d+" if negative else r"\d+", text))

def chars(line):
    return mapl(str, line)

def words(line, word_sep=' '):
    return line.split(word_sep)

def parse_line(line, parse_func=lambda x: x, word_sep=' '):
    if parse_func == words:
        return words(line, word_sep)
    else:
        return parse_func(line)

def parse_lines(s, parse_func=str, word_sep=' ', nl_sep='\n'):
    return [parse_line(line, parse_func, word_sep)
            for line in s.split(nl_sep)]

def parse_paragraphs(s, parse_func=str, word_sep=' '):
    return [parse_lines(paragraph, parse_func, word_sep)
            for paragraph in parse_lines(s, nl_sep='\n\n')]

def read_input(filename):
    filename = f"{filename:02d}" if isinstance(filename, int) else filename
    with open(f"../inputs/{filename}.txt") as f:
        return f.read().strip()


def first_int(text):
    if (m := re.search(r"-?\d+", text)):
        return int(m.group(0))


def count_if(iterable, pred=bool):
    return sum(1 for item in iterable if pred(item))

def first(iterable, default=None):
    return next(iter(iterable), default)

def filter_first(iterable, pred):
    return first(el for el in iterable if pred(el))


def move_point(a, b):
    return tuple(p + q for p, q in zip(a, b))

def manhattan(a, b=(0, 0)):
    return sum(abs(p - q) for p, q in zip(a, b))


def sign(n):
    if n > 0: return 1
    elif n < 0: return -1
    else: return 0


def print_2d(lines):
    for line in lines:
        print(cat(line))


def maxval(d):
    return max(d.values())


def transpose(matrix):
    return list(zip(*matrix))


def bin2int(s):
    return int(s, 2)


def neighbours(x, y, amount=4):
    assert amount in {4, 5, 8, 9}
    for dy, dx in product((-1, 0, 1), repeat=2):
        if ((amount == 4 and abs(dx) != abs(dy)) or
            (amount == 5 and abs(dx) + abs(dy) <= 1) or
            (amount == 8 and not dx == dy == 0) or
             amount == 9):
            yield (x+dx, y+dy)

def complex_neighbours(pt, amount=4):
    assert amount in {4, 5, 8, 9}
    for dy, dx in product((-1, 0, 1), repeat=2):
        if ((amount == 4 and abs(dx) != abs(dy)) or
            (amount == 5 and abs(dx) + abs(dy) <= 1) or
            (amount == 8 and not dx == dy == 0) or
             amount == 9):
            yield pt + dx + dy*1j

def neighbours_3d(x, y, z):
    yield from [(x-1, y, z), (x+1, y, z),
                (x, y-1, z), (x, y+1, z),
                (x, y, z-1), (x, y, z+1)]


def list2grid(lines, pred=None):
    return {(x, y): val
            for y, line in enumerate(lines)
            for x, val in enumerate(line)
            if (pred(val) if pred else True)}

def list2complexgrid(lines, pred=None):
    return {(x + y*1j): val
            for y, line in enumerate(lines)
            for x, val in enumerate(line)
            if (pred(val) if pred else True)}

def grid2list(grid, pred=bool):
    min_x, min_y = map(min, zip(*grid))
    if min_x < 0 or min_y < 0:
        raise ValueError
    max_x, max_y = map(max, zip(*grid))
    lines = [[' ' for _ in range(max_x+1)]
                  for _ in range(max_y+1)]
    for x, y in grid:
        if pred(grid[(x, y)]):
            lines[y][x] = '█'
    return lines

def is_inside(size, pt):
    return (-1 < pt[0] < size) and (-1 < pt[1] < size)

def is_cplx_inside(size, cplx):
    return (-1 < int(cplx.real) < size) and (-1 < int(cplx.imag) < size)


def pt_add(pt1, pt2):
    return (pt1[0] + pt2[0], pt1[1] + pt2[1])

def pt_sub(pt1, pt2):
    return (pt1[0] - pt2[0], pt1[1] - pt2[1])

def pt_mul(mag, pt):
    return (mag*pt[0], mag*pt[1])
