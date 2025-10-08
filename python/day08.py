from aoc import *
from itertools import combinations


def parse_input(input):
    lines = parse_lines(input)
    size = len(lines)
    antennas = list2grid(lines, lambda x: x != '.')
    return size, antennas

def calc_antinodes(size, pt1, pt2, multis):
    antis = set()
    dist = pt_sub(pt2, pt1)
    for n in multis:
        n_dist = pt_mul(n, dist)
        anti = pt_add(pt2, n_dist)
        if is_inside(size, anti):
            antis.add(anti)
        else:
            break
    return antis

def antinodes(size, antennas, multis):
    acc = set()
    for (pt1, freq1), (pt2, freq2) in combinations(antennas.items(), 2):
        if freq1 == freq2:
            acc |= calc_antinodes(size, pt1, pt2, multis) | calc_antinodes(size, pt2, pt1, multis)
    return len(acc)

def solve(filename):
    data = parse_input(read_input(filename))
    return antinodes(*data, [1]), antinodes(*data, range(100))


solve(8)
