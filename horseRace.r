# https://fivethirtyeight.com/features/who-will-win-the-lucky-derby/
# The bugle sounds, and 20 horses make their way to the starting gate for the first annual Lucky Derby. 
# These horses, all trained at the mysterious Riddler Stables, are special. 
# Each second, every Riddler-trained horse takes one step. Each step is exactly one meter long. 
# But what these horses exhibit in precision, they lack in sense of direction. 
# Most of the time, their steps are forward (toward the finish line) but the rest of the time they are backward 
# (away from the finish line). As an avid fan of the Lucky Derby, you’ve done exhaustive research on these 20 competitors. 
# You know that Horse One goes forward 52 percent of the time, Horse Two 54 percent of the time, Horse Three 56 percent, and so on, 
# up to the favorite filly, Horse Twenty, who steps forward 90 percent of the time. 
# The horses’ steps are taken independently of one another, and the finish line is 200 meters from the starting gate.

# Handicap this race and place your bets! In other words, what are the odds (a percentage is fine) that each horse wins?

# Libraries
library(tidyverse)

# Inputs
n_horses <- 20                    # horses in race
stp      <- 1                     # length of each step
dir_fwd  <- seq(52, 90, 2) / 100  # chance each horse takes step forward
dst      <- 200                   # length of race 

# Code
# Odds each horse wins
trials  <- 10000
winners <- rep(0, n_horses)
for (i in 1:trials) {
  track <- tibble(horses = 1:n_horses, 
                  pos = rep(0, n_horses),
                  fwd = dir_fwd)
  a <- runif(1000, 0, 1)
  while(max(track$pos) < dst) {
    b <- sample(a, n_horses)
    track <- track %>% 
      mutate(pos = pmax(0, pos + ifelse(fwd >= b, 1, -1)))
  }
  # Tally wins
  # Count ties as wins
  winners[which(track$pos >= dst)] <- winners[which(track$pos >= dst)] + 1
}

# Output
track <- track %>% mutate(win = winners / sum(winners))
print(track)
ggplot(track) +
  geom_point(mapping = aes(win, horses))
