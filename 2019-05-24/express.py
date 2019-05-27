#!/usr/bin/env python3


# I'm running this in parallel using Python fractions and numpy.  Numpy
# runs a lot faster, but I'm also using fractions in case I miss
# something because of a floating point error.
from fractions import Fraction

import numpy as np

def check_team_frac(team):
    return sum(Fraction(1, p) for p in team)

def check_team_np(team):
    return np.sum(np.reciprocal(team))

def make_new_bounds_frac(new_weakest):
    ubound = [new_weakest] + list(range(10, 0, -1))
    lbound = list(range(new_weakest, new_weakest - 11, -1))
    return lbound, ubound

def make_new_bounds_np(new_weakest):
    ubound = np.arange(11, 0, -1, dtype=np.float)
    ubound[0] = new_weakest
    lbound = np.arange(new_weakest, new_weakest - 11, -1, dtype=np.float)
    return lbound, ubound

def guess(lbound, ubound, less_than_two):
    for ii, (lplayer, uplayer) in enumerate(zip(lbound, ubound)):
        if lplayer != uplayer:
            new_player = (lplayer + uplayer) // 2
            if new_player == uplayer:
                # lplayer and uplayer are 1 apart
                new_player = lplayer
            new_team = (lbound if less_than_two else ubound).copy()
            new_team[ii] = new_player
            return new_team
    else:
        return lbound

def teams_eq_frac(t1, t2):
    return t1 == t2

def teams_eq_np(t1, t2):
    return (t1 == t2).all()

def search_teams(team=None, with_numpy=False):
    if team is None:
        team = list(range(12, 1, -1))
    if with_numpy:
        team = np.array(team)
        check_team = check_team_np
        make_new_bounds = make_new_bounds_np
        teams_eq = teams_eq_np
    else:
        check_team = check_team_frac
        make_new_bounds = make_new_bounds_frac
        teams_eq = teams_eq_frac
    lbound, ubound = make_new_bounds(team[0])
    while True:
        check = check_team(team)
        yield team
        if check == 2:
            break
        elif check < 2:
            lbound = team
            team = guess(lbound, ubound, less_than_two=True)
        elif check > 2:
            ubound = team
            team = guess(lbound, ubound, less_than_two=False)
        if teams_eq(team, lbound) or teams_eq(team, ubound):
            lbound, ubound = make_new_bounds(team[0] + 1)
            team = guess(lbound, ubound, less_than_two=False)


init_frac = [168276, 168275, 157757, 8, 7, 6, 5, 4, 3, 2, 1]
init_np = [11282.0, 11281.0, 9722.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0]
for team in search_teams(with_numpy=False):
    pass
else:
    print(team)
