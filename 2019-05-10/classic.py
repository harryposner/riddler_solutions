#!/usr/bin/env python3

from functools import reduce
from itertools import repeat

import numpy as np

# https://fivethirtyeight.com/features/can-the-riddler-bros-beat-joe-dimaggios-hitting-streak/

# Five brothers join the Riddler Baseball Independent Society, or RBIs.
# Each of them enjoys a lengthy career of 20 seasons, with 160 games per
# season and four plate appearances per game. (To make this simple,
# assume each plate appearance results in a hit or an out, so there are
# no sac flies or walks to complicate this math.)

# Given that their batting averages are .200, .250, .300, .350 and .400,
# what are each brother’s chances of beating DiMaggio’s 56-game hitting
# streak at some point in his career? (Streaks can span across seasons.)

# By the way, their cousin has a .500 average, but he will get tossed
# from the league after his 10th season when he tests positive for
# performance enhancers. What are his chances of beating the streak?

AT_BATS_PER_GAME = 4
STREAK_TO_BEAT = 56
PLAYERS = [(20*160, 0.200),  # n_games, batting_average
           (20*160, 0.250),
           (20*160, 0.300),
           (20*160, 0.350),
           (20*160, 0.400),
           (10*160, 0.500)]


def transition_matrix(batting_avg):
    prob_hit = 1 - (1 - batting_avg) ** AT_BATS_PER_GAME
    matrix = []
    for row in range(STREAK_TO_BEAT + 2):
        current_row = []
        for col in range(STREAK_TO_BEAT + 2):
            if row < STREAK_TO_BEAT + 1 and col == 0:
                # Streak ends
                current_row.append(1 - prob_hit)
            elif col == row + 1:
                # Streak continues
                current_row.append(prob_hit)
            elif row == col == STREAK_TO_BEAT + 1:
                # Streak has surpassed Joe's
                current_row.append(1)
            else:
                current_row.append(0)
        matrix.append(current_row)
    return np.array(matrix)


print(f"# games\tBA\tPr(beat Joe) (%)")
for n_games, batting_average in PLAYERS:
    t_game = transition_matrix(batting_average)
    t_career = reduce(np.dot, repeat(t_game, n_games))
    prob_beat_joe = 100 * t_career[0, -1]
    print(f"{n_games}\t{batting_average:.3f}\t{prob_beat_joe}")

# # games BA      Pr(beat Joe) (%)
# 3200    0.200   1.1624744863325596e-08
# 3200    0.250   3.8151937626242545e-05
# 3200    0.300   0.0120615676257413
# 3200    0.350   0.7596601275913853
# 3200    0.400   13.931512790495667
# 1600    0.500   93.38341706773635
