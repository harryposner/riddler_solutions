[Riddler Express for May 26,
2019](https://fivethirtyeight.com/features/one-small-step-for-man-one-giant-coin-flip-for-mankind/).

# Answer

The answer depends on distributional assumptions for the number of goals a
player scores in a game.

Assuming the number of goals a player gets in a game is i.i.d., then the search
algorithm in `express.py` should eventually find the solution (if it exists).
I've written it for the case where the number of goals follows a Bernoulli
distribution, but it's easy to extend it for other distributions.

# How I did it

Assume the number of goals a player scores in a game is i.i.d..  Then
regardless of the distribution of *number* of goals per game, we can model
*whether* the player scores in a game as a Bernoulli distribution with some
probability of success `p`.  That means the number of games needed to score a
goal follows a geometric distribution.  The `N` that we're given for a player
is the expectation of this geometric distribution, which is `1/p`.  So as
long as the number of goals is i.i.d., the probability that player `N` scores
at least one goal in a game must be `1/N`.

The simplest distribution for the number of goals per game that satisfies this
condition is just the same Bernoulli distribution.  Player `N` scores one goal
with probability `1/N` and no goals with probability `1 - 1/N`, averaging `1/N`
goals per game.  But if players can score more than one goal per game, then we
can get different expected goals-per-game while still satisfying the condition
of `N` expected games for at least one goal.  

Suppose the number of goals that a player scores in a game follows a Poisson
process with an arrival rate of `lambda`.  The probability that player `N`
scores no goals in a game is `exp(-lambda) * (lambda^0) / (0!) = exp(-lambda)`,
so the probability that she scores at least one goal is `1 - exp(-lambda)`.
Set this equal to `1/N` and solve for `lambda` to find the arrival rate that
satisfies the condition that player `N` averages `N` games to score a goal.
The arrival rate, `log(N) - log(N-1)`, is by definition the expected number of
goals per game.

Since the answer depends on the expected goals per game for each player, the
answer depends on our assumptions about that distribution.  I'll work with the
Bernoulli distribution, since the Poisson distribution looks really fiddly.
Anyway, I figure the Bernoulli case is what the person asking the question
intended.

My code is still running (which probably means there's a much better way to do
it, but oh well), so I don't have an answer yet, but it's in the `express.py`
file in this directory.  I'm running a binary search over the possible teams
with a given weakest player.  If I don't find a team that I expect to score
exactly two goals in a game, I increment the weakest player and try again.  I
could try other distributions by changing the `check_team` function to sum the
expected values---for the Poisson distribution, that would be
`np.sum(np.log(team) - np.log(team - 1))`
