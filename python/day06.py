from aoc import *


def parse_input(input):
    lines = parse_lines(input, chars)
    walls = set(list2complexgrid(lines, lambda x: x == '#'))
    guard = list(list2complexgrid(lines, lambda x: x == '^'))[0]
    return {'walls': walls,
            'pt': guard,
            'size': len(lines)}

def turn_right(cplx): return cplx * 1j

def guard_path(walls, pt, size):
    path = {pt}
    dr = -1j
    while True:
        pt_ = pt + dr
        if not is_cplx_inside(size, pt_):
            return path
        elif pt_ in walls:
            dr = turn_right(dr)
        else:
            pt = pt_
            path.add(pt_)

def is_loop(obstacle, walls, pt, size):
    walls = walls | {obstacle}
    turns = set()
    dr = -1j
    while True:
        pt_ = pt + dr
        if (pt, dr) in turns:
            return 1
        elif not is_cplx_inside(size, pt_):
            return 0
        elif pt_ in walls:
            turns.add((pt, dr))
            dr = turn_right(dr)
        else:
            pt = pt_

def part_2(input, path):
    return sum(is_loop(obstacle, **input) for obstacle in path)

def solve(filename):
    input = parse_input(read_input(filename))
    path = guard_path(**input)
    return len(path), part_2(input, path)


solve(6)
