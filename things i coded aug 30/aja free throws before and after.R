library(wehoop)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(tidyverse)

#wnba player full box score
wnba_player_box <- wehoop::load_wnba_player_box(season=c(2025))

allstar_date <- as.Date("2025-07-18")

aja <- wnba_player_box %>%
  filter(athlete_display_name == "A'ja Wilson") %>%
  mutate(game_date = as.Date(game_date), period = if_else(game_date < allstar_date, "Before", "After")) %>%
  group_by(period) %>%
  summarize(FTM = sum(free_throws_made, na.rm = TRUE), FTA = sum(free_throws_attempted, na.rm = TRUE), FTp = 100 * FTM / FTA, .groups = "drop")

aja <- aja %>%
  mutate(period = fct_relevel(period, "Before", "After"))

aja %>% arrange(period)

#bar chart ft% before and after
ggplot(aja, aes(x = period, y = FTp, fill = period)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", FTp)), vjust = -0.4, size = 3.8) +
  labs(title = "A'ja Wilson FT% Before vs After All-Star Break",
       x = NULL, y = "FT%") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, 100))

aja


aja_period_totals <- wnba_player_box %>%
  filter(athlete_display_name == "A'ja Wilson") %>%
  mutate(game_date = as.Date(game_date),
         period = if_else(game_date < allstar_date, "Before", "After")) %>%
  group_by(period) %>%
  summarise(
    made = sum(free_throws_made, na.rm = TRUE),
    attempts = sum(free_throws_attempted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(missed = pmax(attempts - made, 0L)) %>%
  select(period, made, missed) %>%
  pivot_longer(cols = c(made, missed), names_to = "result", values_to = "count") %>%
  mutate(period = fct_relevel(period, "Before", "After")) 

ggplot(aja_period_totals %>%
  mutate(period = factor(period, c("Before","After")),
         result = factor(result, c("made","missed"))),
aes(x = period, y = count, fill = result)) +
geom_col(width = 0.6, color = "gray20", position = position_stack(reverse = TRUE)) +
scale_fill_manual(values = c(made = "#1b779e", missed = "#d91002"),
             limits = c("made","missed"),
             labels = c("Made","Missed"),
             name = NULL) +
labs(title = "A'ja Wilson: I’ll make free throws 2nd half of the season",
subtitle = "Free Throws Before vs After 2025 All-Star Break",
x = NULL, y = "Total Free Throws") +
theme_minimal(base_size = 12) +
theme(legend.position = "top")


