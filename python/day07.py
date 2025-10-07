from aoc import *


def conc(a, b): return int(str(a) + str(b))

def is_valid_eq(res, nums, can_use_concat):
    match nums:
        case []: return False
        case [a]: return (a == res) and res
        case [a, *_] if a > res: return False
        case [a, b, *tail]:
            return (is_valid_eq(res, [a+b, *tail], can_use_concat) or
                    is_valid_eq(res, [a*b, *tail], can_use_concat) or
                    (can_use_concat and
                     is_valid_eq(res, [conc(a, b), *tail], can_use_concat)))

def sum_valid(data, can_use_concat):
    return sum(is_valid_eq(res, nums, can_use_concat)
               for res, *nums in data)

def solve(filename):
    data = parse_lines(read_input(filename), ints)
    return [sum_valid(data, concat) for concat in (0, 1)]


solve(7)
