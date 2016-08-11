## Importing libraries
library(dplyr)
library(tidyr)


## Defining custion sum_spc function
sum_spc <- function(x){
  min_x <- min(x)
  mean_x <- mean(x)
  med_x <- median(x)
  max_x <- max(x)
  var_x <- var(x)
  sd_x <- sd(x)
  return(c(min = min_x, mean = mean_x, med = med_x, max = max_x, var = var_x, sd = sd_x))
}


## Initializing vectors for suits, cards and values
card_suits <- c("Spades", "Diamonds", "Hearts", "Clubs")

card_names <- c("Ace", "King", "Queen", "Jack", "Ten", "Nine", "Eight", "Seven", "Six", "Five", "Four", "Three", "Two")

card_values <- c(1, 10, 10, 10, 10:2)

n_suits <- length(card_suits)


## Expanding the card types into four suits

card_deck <- expand.grid(card_names, card_suits)


## Building the card deck by adding respective values

card_deck <- card_deck %>%
              unite("Cards", c(1, 2), sep = "_", remove = TRUE) %>%
              mutate(Values = rep(card_values, n_suits))


## Outputting the population in a csv file
write.csv(card_deck, file = "Deck of Cards.csv", row.names = FALSE)


## Visualization

View(card_deck)

hist(card_deck$Values, main = "Histogram of Card Population", xlab = "Values", col = "blue", breaks = 4)
box()


## Analysis

sum_pop <- sum_spc(card_deck$Values)


## Defining sampling size and number of times to be sampled 
sample_size = 3
sampling_number = 30


## Creating empty lists and vectors
cards <- list()
sample_sums <- rep(0, sampling_number)
sample_cards <- rep(0, sampling_number)


## Setting seed for reproducing the results
set.seed(110)


## Simulating sample taking and repeating for 30 times
for(i in 1:sampling_number){
  cards[i] <- list(sample(card_deck$Cards, size = sample_size))
  sample_sums[i] <- sum(card_deck$Values[card_deck$Cards %in% cards[[i]]])
  sample_cards[i] <- paste(cards[[i]], collapse = " + ")
}

samples <- data.frame(Cards = sample_cards, Sums = sample_sums)


## Outputting the samples in a csv file
write.csv(samples, file = "30 Samples of Cards.csv", row.names = FALSE)


## Visualization
View(samples)

hist(samples$Sums, main = "Histogram of Sampled Sums", xlab = "Values", col = "blue", breaks = 4)
box()
sum_sam <- sum_spc(samples$Sums)


## Comparison using boxplots
boxplot(card_deck$Values, samples$Sums, main = "Population vs. Sampled Distribution", names = c("Population", "Sampled Sums"), ylab = "Values", col = "grey")


## Approximate range of 90% values

"
From the z table, we look for the probability 90% (0.9).
The closest we get is 0.9015 for z = 1.29. Now, we have to reverse calculate the value.

"
mu_samp = as.numeric(sum_sam[2])

sigma_samp = as.numeric(sum_sam[6])

z_90 = 1.29

x_90 = mu_samp + (sigma_samp * z_90)

"
Which gives us 26.57179. Which means approximately 90% values should be less than 26.57. 

"

## Probablity for getting a draw value of at least 20

z_20 <- (20- mu_samp)/sigma_samp

prob_20 <- (1 - pnorm(z_20))

# prob_20 <- (1 - pnorm(20, mean = mean(samples$Sums), sd = sd(samples$Sums)))*100

"
Using z table we get for z = 0.0681(0.07), the probability is 0.5279.
Therefore the probability of getting at least 20 per draw is (1-0.5279) = 0.4721.
Or, inversely we could look for the probability of z = -0.07, which directly gives us 0.4721.

"

