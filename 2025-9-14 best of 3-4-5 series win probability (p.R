#best of 3-4-5 series win probability (p=0.5,...,1)
library(ggplot2)
library(dplyr)
library(purrr)
library(knitr)

p_values <- seq(0.5, 1.0, 0.01)

#alculate win probabilities using negative binomial
#"number of failures before reaching required successes"
df <- expand.grid(p = p_values, series_type = c(3, 5, 7)) %>%
  mutate(
    games_to_win = (series_type + 1) / 2,  # 2, 3, 4 respectively
    # Use dnbinom to sum probabilities for all possible series lengths
    win_probability = pmap_dbl(list(p, series_type, games_to_win), function(p, series_type, games_to_win) {
      # Alternative approach: use pbinom with the "extend to full series" method
      # Probability of winning >= games_to_win out of series_type games
      1 - pbinom(games_to_win - 1, series_type, p)
    }),
    series_name = paste("Best of", series_type)
  )


plot<- ggplot(df, aes(x = p, y = win_probability, color = series_name)) +
  geom_line(size = 1.2) +
  scale_x_continuous(
    name = "Single Game Win Probability (p)",
    breaks = seq(0.5, 1.0, 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    name = "Series Win Probability", 
    breaks = seq(0.5, 1.0, 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    name = "Series Length",
    values = c("Best of 3" = "#e31ab1", "Best of 5" = "#1F78B4", "Best of 7" = "#000000")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11)
  ) +
  labs(
    title = "Series Win Probability vs Single-Game Win Probability"
  )

print(plot)
##############################################################################
p <- 0.7
series_lengths <- c(3, 5, 7, 9, 11, 15, 21, 31, 41, 51, 71, 99, 151)

results_table <- data.frame(
  series_length = series_lengths,
  games_to_win = (series_lengths + 1) / 2
) %>%
  mutate(
    win_probability = 1 - pbinom(games_to_win - 1, series_length, p),
    percentage = paste0(round(win_probability * 100, 5), "%"), #format as percentage
    series_name = paste("Best of", series_length)
  ) %>%
  select(series_name, games_to_win, percentage)


kable(results_table, 
      col.names = c("Series Type", "Games to Win", "Percentage"),
      digits = 4,
      caption = "Series Win Probabilities for p = 0.7")