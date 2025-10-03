from aoc import *
import re


mul_patt = r"mul\((\d+),(\d+)\)"
do_patt = r"(do\(\))"
dont_patt = r"(don't\(\))"
pattern = "|".join([mul_patt, do_patt, dont_patt])


def solve(filename):
    instructions = re.findall(pattern, read_input(filename))
    enabled = True
    p1_sum = 0
    p2_sum = 0
    for a, b, do, dont in instructions:
        if do: enabled = True
        elif dont: enabled = False
        else:
            mult = int(a) * int(b)
            p1_sum += mult
            if enabled: p2_sum += mult
    return p1_sum, p2_sum


solve(3)
