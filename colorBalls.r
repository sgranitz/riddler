# https://fivethirtyeight.com/features/can-you-solve-these-colorful-puzzles/
# You play a game with four balls: One ball is red, one is blue, 
# one is green and one is yellow. They are placed in a box. You 
# draw a ball out of the box at random and note its color. Without 
# replacing the first ball, you draw a second ball and then paint 
# it to match the color of the first. Replace both balls, and repeat 
# the process. The game ends when all four balls have become the 
# same color. What is the expected number of turns to finish the game?

# Libraries
library(ggplot2)

# Input
num_colors <- 4
num_balls  <- 4
num_trials <- 25000

# Code
rpl <- num_balls > num_colors
num_turns <- numeric(num_trials)

for (i in 1:num_trials) {
  a <- sample(1:num_colors, num_balls, replace = rpl)
  count <- 0
  while(!isTRUE(all.equal(min(a), max(a)))) {
    b  <- sample(a, 2, replace = FALSE)
    c1 <- min(which(a == b[1]))
    c2 <- min(which(a == b[2]))
    a[c(c1, c2)] <- b[1]
    count <- count + 1
  }
  num_turns[i] <- count
}

# Output
ggplot(mapping = aes(num_turns)) + 
  geom_histogram(binwidth = 1) +
  theme_bw() +
  labs(title = "Riddler 4/28", x = "Num. of Turns", y = "Count")
median(num_turns)
mean(num_turns)
