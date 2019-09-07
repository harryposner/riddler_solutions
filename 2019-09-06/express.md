#My solution

I win four times out of seven.

#How I did it

I win on the first turn if my friend and I look in the same direction.  Each of
us can look in one of four directions, so there are sixteen permutations of
directions we can look, and I win in four of those.  That means I have a 1/4
chance of winning on the first move.

If I don't win on the first move, then my friend has a chance of winning.  He's
in the exact same spot I was in my first turn.  So at this point he has the
same probability of winning that I did at the beginning.  The probability that
I win once I'm in this scenario is the complement of the probability that I win
in general.

Putting these possibilities together, we get:

	Pr(I win) = 1/4 + 3/4 * (1 - Pr(I win))

And with a little algebra, we find that `Pr(I win) = 4/7`.  The Python code in
`express.py` provides a sanity check.
