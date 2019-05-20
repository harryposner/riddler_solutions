#!/usr/bin/env python3
from rhettinger_pycon_2019 import Puzzle

# Dots are nodes.  Plus is the current node.  Letters are edges, where the
# letter indicates the color (b is blue and k is black).
STARTING_POS = """\
.g.p.p.w.p.w.w.g.
w r w y p g r b o
.r.b.y.k.r.k.o.b.
y w g p w y g k w
.y.r.b.r.o.w.g.r.
w k o w k p y k r
.w.o.o.g.g.p.y.g.
y g p g o b o p o
.k.o.b.p.k.g.p.k.
w p k o r p o b p
.w.g.b.g.o.b.o.p.
g r p o y b k o y
.y.p.g.k.o.b.b.g.
w k w o p r w r o
+p.k.r.b.g.w.y.p.
o g o b o k w b o
.o.p.w.w.w.r.o.g.
"""
N_ROWS = len(STARTING_POS.splitlines())
N_COLS = STARTING_POS.find("\n")


class RiddlerMaze(Puzzle):

    pos = STARTING_POS

    def __init__(self, valid_edge_colors, pos=None):
        super().__init__(pos=pos)
        self.valid_edge_colors = valid_edge_colors

    @property
    def location(self):
        return divmod(self.pos.find("+"), N_COLS + 1)

    def isgoal(self):
        return self.location == (2, 16)

    def __iter__(self):
        current_row, current_col = self.location
        grid = self.pos.splitlines()
        for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            edge_row, edge_col = current_row + dx, current_col + dy
            on_grid = 0 <= edge_row < N_ROWS and 0 <= edge_col < N_COLS
            if not on_grid:
                continue
            ok_color = grid[edge_row][edge_col] in self.valid_edge_colors
            if on_grid and ok_color:
                new_row, new_col = current_row + 2*dx, current_col + 2*dy
                new_grid = [list(row) for row in grid]
                new_grid[current_row][current_col] = "."
                new_grid[new_row][new_col] = "+"
                new_pos = "\n".join("".join(row) for row in new_grid)
                yield RiddlerMaze(self.valid_edge_colors, new_pos)


def pprint_soln(soln):
    trail = [list(row) for row in soln[0].pos.splitlines()]
    prev_row, prev_col = soln[0].location
    for step in soln:
        row, col = step.location
        edge_row, edge_col = (row + prev_row) // 2, (col + prev_col) // 2
        trail_char = "-" if row == prev_row else "|"
        trail[prev_row][prev_col] = trail_char
        trail[edge_row][edge_col] = trail_char
        prev_row, prev_col = row, col
    trail[row][col] = "+"
    print("\n".join("".join(row) for row in trail))


def solve_mazes(**kwargs):
    for color, valid_edge_colors in kwargs.items():
        print("Color", color)
        try:
            soln = RiddlerMaze(valid_edge_colors).solve()
        except IndexError:
            # Tried all possible moves
            print("No solution")
        else:
            pprint_soln(soln)
        print()


if __name__ == "__main__":
    solve_mazes(blue="bgpw", yellow="ygow", red="rpow")
