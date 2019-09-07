#!/usr/bin/env python3

import multiprocessing as mp
import random


N_PROCESSES = mp.cpu_count()
N_SIMULATIONS = 7_000_000


def first_player_wins():
    my_turn = True
    while random.randrange(4):
        my_turn = not my_turn
    return my_turn

def run_n_sims(n):
    return sum(first_player_wins() for __ in range(n))


sims_per_process = [N_SIMULATIONS // N_PROCESSES for __ in range(N_PROCESSES)]
sims_per_process[0] += N_SIMULATIONS % N_PROCESSES
with mp.Pool() as pool:
    n_my_wins = sum(pool.map(run_n_sims, sims_per_process))


print(f"{n_my_wins} wins out of {N_SIMULATIONS} simulations")
print(f"First mover wins {100 * n_my_wins / N_SIMULATIONS:.5f}% of the time")
