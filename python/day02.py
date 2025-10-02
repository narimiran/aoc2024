from aoc import *
from itertools import pairwise


def is_safe(line):
    differences = [b - a for (a, b) in pairwise(line)]
    return (all( 1 <= d <=  3 for d in differences) or
            all(-1 >= d >= -3 for d in differences))

def has_single_bad_bit(line):
    for idx in range(len(line)):
        line_ = line[:idx] + line[idx+1:]
        if is_safe(line_):
            return True
    return False

def solve(filename):
    reports = parse_lines(read_input(filename), ints)
    return (sum(is_safe(line) for line in reports),
            sum(has_single_bad_bit(line) for line in reports))


solve(2)
