# Problem statement

[Original
article](https://fivethirtyeight.com/features/which-baseball-team-will-win-the-riddler-fall-classic/)

Riddler League Baseball, also known as the RLB, consists of three teams: the
Mississippi Moonwalkers, the Delaware Doubloons and the Tennessee Taters.

Each time a batter for the Moonwalkers comes to the plate, they have a 40
percent chance of getting a walk and a 60 percent chance of striking out. Each
batter for the Doubloons, meanwhile, hits a double 20 percent percent of the
time, driving in any teammates who are on base, and strikes out the remaining
80 percent of the time. Finally, each batter for the Taters has a 10 percent
chance of hitting a home run and a 90 percent chance of striking out.

During the RLB season, each team plays an equal number of games against each
opponent. Games are nine innings long and can go into extra innings just like
in other baseball leagues. Which of the three teams is most likely to have the
best record at the end of the season?


# My answer

The Taters stomp.

# How I did it

This was a pretty straightforward Monte Carlo simulation.  My code is in
[`classic.clj`](./classic.clj) in this directory.  I just ran it and got
`{:moonwalkers 3909, :doubloons 4066, :taters 7025}` in a 5000 game per matchup
(10k games per team) season.  I guess I could run more simulations, but the
Taters' win so thoroughly that it's really not necessary.
