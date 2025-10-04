from aoc import *


def part_1(grid):
    letters = list(enumerate("XMAS"))
    deltas = (-1, 0, 1)
    return sum(1 for (x, y), c in grid.items()
               if c == 'X'
               for dx in deltas
               for dy in deltas
               if all(l == grid.get((x+i*dx, y+i*dy))
                      for i, l in letters))

def part_2(grid):
    diags = mapt(list, ("MAS", "SAM"))
    deltas = (-1, 0, 1)
    return sum(1 for x, y in grid.keys()
               if ([grid.get((x+d, y+d)) for d in deltas] in diags and
                   [grid.get((x-d, y+d)) for d in deltas] in diags))

def solve(filename):
    grid = list2grid(parse_lines(read_input(filename)))
    return part_1(grid), part_2(grid)


solve(4)
