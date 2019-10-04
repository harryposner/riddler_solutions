# My answer

With the new information, I believe **7/8**, or **87.5%**.

# How I did it

Let Φ refer to the scenario in which B is not eliminated by the 50/50.  By
Bayes' theorem, `Pr(B | Φ) = Pr(Φ | B) * Pr(B) / Pr(Φ)`.  The 50/50 lifeline
can't eliminate a correct answer, so `Pr(Φ | B) = 100%`.  From the problem
statement, our prior probability that the answer is B is `Pr(B) = 70%`.  

By the law of total probability, and substituting in the above:
```
Pr(Φ) = Pr(Φ | B) * Pr(B) + Pr(Φ | ¬B) * Pr(¬B)
      = 100%      * 70%   + Pr(Φ | ¬B) * 30%
```

There are always three incorrect answers and therefore `3 choose 2 = 3` ways to
pick two to eliminate.  When B is one of the wrong answers, two of those
combinations include B and one doesn't.  Since the 50/50 lifeline chooses from
those combinations with uniform probability, `Pr(F | ~B) = 1/3`.  Substituting
into the above, we get `Pr(F) = 80%`, and substituting back into Bayes' theorem,
we get:
```
Pr(B | Φ) = 100% * 70% / 80%
          = 7/8
```
