#generated using perplexity.ai, edited

library(ggplot2)
library(dplyr)
library(tictoc)
library(progressr)
library(ggtext)

setwd("~/Documents")


# Function to simulate a single series
simulate_series <- function(p, n_wins) {
  games <- 0
  wins <- 0
  while (wins < n_wins && games - wins < n_wins) {
    games <- games + 1
    if (runif(1) < p) {
      wins <- wins + 1
    }
  }
  return(wins == n_wins)
}

# Function to run multiple simulations and calculate probability
simulate_series_probability <- function(p, n_wins, n_simulations = 100000) {
  results <- replicate(n_simulations, simulate_series(p, n_wins))
  return(mean(results))
}

# Set the probability of winning a single game
p <- 0.55

# Simulate for best-of-3, best-of-5, and best-of-7 series
best_of_3 <- simulate_series_probability(p, 2)
best_of_5 <- simulate_series_probability(p, 3)
best_of_7 <- simulate_series_probability(p, 4)

# Print results
cat("Probability of winning a best-of-3 series:", best_of_3, "\n")
cat("Probability of winning a best-of-5 series:", best_of_5, "\n")
cat("Probability of winning a best-of-7 series:", best_of_7, "\n")


######################################################################################################
# Generate data for different p values
p_values <- seq(0.5, 0.99, by = 0.01)

data <- expand.grid(p = p_values, series = c("Best of 3", "Best of 5", "Best of 7"))


data$probability <- mapply(function(p, series) {
  n_wins <- switch(series,
                   "Best of 3" = 2,
                   "Best of 5" = 3,
                   "Best of 7" = 4)
  simulate_series_probability(p, n_wins)
}, data$p, data$series)


colors <- RColorBrewer::brewer.pal(3, "Set2")

# Create the plot
ggplot(data, aes(x = p, y = probability, color = series)) +
  geom_line(size = 1) +
  labs(title = paste0("Probability of Winning ",
                      "<span style='color:", colors[1], "'>Best of 3</span>, ",
                      "<span style='color:", colors[2], "'>Best of 5</span>, and ",
                      "<span style='color:", colors[3], "'>Best of 7</span> Series"),
       x = "Single Game Win Probability",
       y = "Series Win Probability",
       subtitle="empirical probabilities from simulation (im lazy)",
       caption="graphic: @wnbadata") +
  theme_minimal() +
  theme(plot.title = element_markdown(lineheight = 1.1),
        legend.position = "none") +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(0.1, 1.0, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  coord_cartesian(ylim = c(0, 1))

# Save the plot as a PNG file
#ggsave("series_win_probability.png", width = 10, height = 6, dpi = 300)