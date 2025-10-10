from aoc import *


def find_trailheads(grid):
    return {k for k, v in grid.items() if v == 0}

def dfs(grid, start):
    ends = []
    stack = [(start, 0)]
    while stack:
        curr, h = stack.pop()
        if h == 9:
            ends.append(curr)
            continue
        nbs = [(pt, pt_h) for pt in neighbours(*curr)
               if (pt_h := grid.get(pt)) == h + 1]
        stack.extend(nbs)
    return len(set(ends)), len(ends)

def calc_score(scores, idx):
    return sum(score[idx] for score in scores)

def solve(filename):
    grid = list2grid(parse_lines(read_input(filename), digits))
    trailheads = find_trailheads(grid)
    scores = [dfs(grid, start) for start in trailheads]
    return calc_score(scores, 0), calc_score(scores, 1)


solve(10)
