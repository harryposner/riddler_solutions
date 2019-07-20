#!/usr/bin/env python3


GRID_SIZE = 4
FILLED = "■"
BLANK = "□"


class Grid:

    def __init__(self, n):
        self.n = n
        def gen_grid(n):
            for place in reversed(range(GRID_SIZE**2)):
                mask = 1 << place
                yield FILLED if n & mask else BLANK
                if place % GRID_SIZE == 0:
                    yield "\n"
        self.as_str = "".join(gen_grid(n))[:-1]
        self.lines = self.as_str.splitlines()

    def __repr__(self):
        return f"Grid #{self.n}\n{self.as_str}"

    def select_twobytwo(self, which):
        # top-left of twobytwo
        row, col = divmod(which, GRID_SIZE-1)
        return f"{self.lines[row][col:col+2]}\n{self.lines[row+1][col:col+2]}"

    def is_solution(self):
        seen_twobytwos = set()
        for ii in range((GRID_SIZE-1) ** 2):
            sample = self.select_twobytwo(ii)
            if sample in seen_twobytwos:
                return False
            else:
                seen_twobytwos.add(sample)
        else:
            return True


solutions = []
for ii in range(2 ** (GRID_SIZE**2)):
    guess = Grid(ii)
    if guess.is_solution():
        solutions.append(guess)

print(f"There are {len(solutions)} solutions")
print("The first one is", solutions[0])
