library(tidyverse)
library(ggplot2)
library(ggtext)

#data showing ranked teams' margins of victory from stathead https://stathead.com/tiny/HFCy7
data <- read.csv("~/Desktop/ranked_net_scores.csv", header = TRUE)

ggplot(data, aes(x = OppAP., y = -PTS)) +
  geom_line(y = 0, color = "red") +
  geom_point() +
  labs(
    title = "Scoring Margin of Top 25 Teams vs Unranked Teams",
    subtitle = "by Team Ranking at Time of Game, 2024-25 season",
    caption = "data: wehoop | graphic: @wnbadata",
    x = "AP Ranking at time of game",
    y = "Scoring margin"
  ) +
  scale_x_continuous(breaks = seq(min(data$OppAP.), max(data$OppAP.), by = 1)) +
  scale_y_continuous(breaks = seq(-30,90 , by = 10)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


cor(data$OppAP., data$PTS)
