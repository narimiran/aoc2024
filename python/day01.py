from aoc import *
from collections import Counter


def diff(a, b):
    return abs(a - b)

def part_1(columns):
    return sum(map(diff, *columns))

def part_2(l, r):
    freqs = Counter(r)
    return sum(n * freqs[n] for n in l)

def solve(filename):
    columns = mapl(sorted, transpose(parse_lines(read_input(filename), ints)))
    return part_1(columns), part_2(*columns)


solve(1)
