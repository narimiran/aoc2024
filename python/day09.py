from aoc import *


def parse_data(input):
    return [None if i%2 else i//2
            for i, n in enumerate(parse_line(input, digits))
            for _ in range(n)]

def defragment(disk):
    res = []
    fi = 0
    bi = len(disk) - 1
    while fi <= bi:
        if disk[fi] is not None:
            res.append(disk[fi])
            fi += 1
        elif disk[bi] is not None:
            res.append(disk[bi])
            fi += 1
            bi -= 1
        else:
            bi -= 1
    return res

def calc_checksum(disk):
    return sum(idx * i for idx, i in enumerate(disk))

def solve(filename):
    data = parse_data(read_input(filename))
    return calc_checksum(defragment(data))


solve(9)
