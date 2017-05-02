# https://fivethirtyeight.com/features/pick-a-card-any-card/
# From a shuffled deck of 100 cards that are numbered 1 to 100, 
# you are dealt 10 cards face down. You turn the cards over one by one. 
# After each card, you must decide whether to end the game. 
# If you end the game on the highest card in the hand you were dealt, you win; 
# otherwise, you lose.
# What is the strategy that optimizes your chances of winning? 
# How does the strategy change as the sizes of the deck and the hand are changed?

# Libraries
library(tidyverse)

# Input
no_cards <- 100
no_turns <-  10

#Code
cards <- no_cards:1
turns <- no_turns:1

# If current card is not you high card then continue picking
# Otherwise if current card is your high card:

y <- matrix(NA, nrow = max(turns), ncol = max(cards))
for (i in cards) {
  x <- numeric(max(turns))
  for (j in turns) {
    lwr <- i - (max(turns) - j)
    tot <- max(cards) - (max(turns) - j)
    a <- pmax(0, (lwr - 1):(lwr - j))
    b <- (tot - 1):(tot - j)
    x[(max(turns) - j + 1)] <- tail(cumprod(a / b), 1)
  }
  y[, (max(cards) - i + 1)] <- x
}

xval <- yval <- numeric(nrow(y))
for (i in 1:nrow(y)) {
  xval[i] <- i
  yval[i] <- max(cards) - max(which(y[i, ] > 0.5))
}
yval <- c(yval[2:max(turns)], max(turns))
line <- tibble(x = xval, y = yval)

colnames(y) <- max(cards):1
y <- as_tibble(y) %>% 
  mutate(turn = 1:max(turns)) %>% 
  gather(max(cards):1, key = card, value = prob) %>% 
  mutate(card = as.numeric(card))

# Output
ggplot(y, aes(turn, card)) + 
  geom_tile(aes(fill = prob)) + 
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  layer(geom = "line", stat = "identity", 
        position = "identity", data = line,
        aes(x, y))
as_tibble(paste0("For turn ", xval, " stop if card >= ", yval))
