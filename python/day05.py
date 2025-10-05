from aoc import *
from itertools import groupby, pairwise
from functools import cmp_to_key


def is_valid_line(line, rules):
    return all(pair in rules for pair in pairwise(line))

def middle_page(line):
    return line[len(line)//2]

def part_1(rules, updates):
    return sum(middle_page(u)
               for u in updates
               if is_valid_line(u, rules))

def part_2(rules, updates):
    rules = set(rules)
    def cmp(a, b):
        return -1 if (a, b) in rules else 1
    return sum(middle_page(sorted(u, key=cmp_to_key(cmp)))
               for u in updates
               if not is_valid_line(u, rules))

def solve(filename):
    data = parse_paragraphs(read_input(filename), ints)
    return part_1(*data), part_2(*data)


solve(5)
