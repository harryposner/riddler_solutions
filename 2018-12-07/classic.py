#!/usr/bin/env python3

import multiprocessing as mp
import random

# https://fivethirtyeight.com/features/the-little-mathematically-determined-house-on-the-prairie/

# I won the shoutout for this one---here's the next week's article:
# https://fivethirtyeight.com/features/can-you-win-tic-tac-toe-blindfolded/


# This is about how many simulations it takes to get to 4 figures of precision.
# N_SIMULATIONS = 100000000
N_SIMULATIONS = 100000
N_PROCESSES = mp.cpu_count()


class Louie:


    def __init__(self):
        self._office_umbrellas = 1
        self._home_umbrellas = 2
        self.total_umbrellas = self._office_umbrellas + self.home_umbrellas
        self.got_wet = False


    @property
    def office_umbrellas(self):
        return self._office_umbrellas

    @office_umbrellas.setter
    def office_umbrellas(self, n):
        if n < 0 or n > self.total_umbrellas:
            raise ValueError
        self._office_umbrellas = n
        self._home_umbrellas = self.total_umbrellas - n

    @property
    def home_umbrellas(self):
        return self._home_umbrellas

    @home_umbrellas.setter
    def home_umbrellas(self, n):
        self.office_umbrellas = self.total_umbrellas - n

    def commute(self, direction):
        if direction == 'work':
            rain = random.random() < 0.5
            if rain:
                try:
                    self.home_umbrellas -= 1
                except ValueError:
                    self.got_wet = True
        elif direction == 'home':
            rain = random.random() < 0.4
            if rain:
                try:
                    self.office_umbrellas -= 1
                except ValueError:
                    self.got_wet = True

    def simulate_week(self):
        for trip in range(10):
            if trip % 2 == 0:
                self.commute('work')
            else:
                self.commute('home')
        return self.got_wet


def run_simulations(n_simulations):
    return sum(Louie().simulate_week() for __ in range(n_simulations))


if __name__ == '__main__':

    n_sim_per_process = [N_SIMULATIONS // N_PROCESSES] * N_PROCESSES
    n_sim_per_process[0] += N_SIMULATIONS % N_PROCESSES
    with mp.Pool(N_PROCESSES) as p:
        n_wet = sum(p.map(run_simulations, n_sim_per_process))
    pct_wet = 100 * (n_wet / N_SIMULATIONS)
    print(f'Louie gets wet {pct_wet:.2f}% of the time')
    print(f'He stays dry {100-pct_wet:.2f}% of the time')
