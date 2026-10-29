#best of 3-5-7 series win probability (p=0.5,...,1)
#assumes every game is independent with the same single-game win probability p
library(ggplot2)
library(dplyr)
library(knitr)
library(wehoop)

p_values <- seq(0.5, 1.0, 0.01)

#calculate win probabilities using the binomial ("extend to full series" trick)
#pretend every series is played out to the full length. the team that clinches
#first still has the most wins after all games, so winning the series is the same
#as winning at least games_to_win of series_type games
#(same answer as the negative binomial: sum of dnbinom(0:(games_to_win - 1), games_to_win, p))
df <- expand.grid(p = p_values, series_type = c(3, 5, 7)) %>%
  mutate(
    games_to_win = (series_type + 1) / 2,  # 2, 3, 4 respectively
    win_probability = 1 - pbinom(games_to_win - 1, series_type, p),
    series_name = paste("Best of", series_type)
  )


plot<- ggplot(df, aes(x = p, y = win_probability, color = series_name)) +
  geom_line(linewidth = 1.2) +
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
p <- 0.56
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
      caption = paste("Series Win Probabilities for p =", p))
##############################################################################
# charts for the 10/28 script (finals at 3-2), chart index at the bottom
team_colors <- c("Valkyries" = "#6A3FA0", "Dream" = "#E31837")

# hook: the same 5 finals games crown a different champion depending on series length
# finals games 1-5, frozen at game 5 so the chart matches the script
finals_games <- load_wnba_schedule(seasons = 2026) %>%
  filter(status_type_completed,
         (home_abbreviation == "GS" & away_abbreviation == "ATL") |
           (home_abbreviation == "ATL" & away_abbreviation == "GS"),
         notes_headline != "",
         as.Date(game_date) <= as.Date("2026-10-27")) %>%
  mutate(
    home_score = as.integer(home_score),
    away_score = as.integer(away_score),
    gs_margin = ifelse(home_abbreviation == "GS", home_score - away_score, away_score - home_score),
    winner = ifelse(gs_margin > 0, "Valkyries", "Dream")
  ) %>%
  arrange(as.Date(game_date)) %>%
  transmute(game = row_number(), winner) %>%
  bind_rows(tibble(game = 6:7, winner = "Not played yet"))

series_ends <- tibble(
  x = c(1.5, 3.5, 5.5, 7.5),
  label = c("best of 1:\nValkyries", "best of 3:\nDream", "best of 5:\nValkyries", "best of 7:\n???")
)

p_flip <- ggplot(finals_games, aes(x = game, y = 1, fill = winner)) +
  geom_tile(color = "white", linewidth = 1.5, height = 0.6) +
  geom_text(aes(label = paste0("G", game)), color = "white", size = 5, fontface = "bold") +
  geom_vline(data = series_ends, aes(xintercept = x), linetype = "dashed") +
  geom_text(data = series_ends, aes(x = x - 0.08, y = 1.55, label = label), inherit.aes = FALSE, size = 3.5, hjust = 1) +
  scale_fill_manual(values = c(team_colors, "Not played yet" = "grey80"), name = NULL) +
  scale_x_continuous(breaks = NULL, limits = c(0.5, 7.6)) +
  scale_y_continuous(breaks = NULL, limits = c(0.6, 1.75)) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 14)) +
  labs(title = "Who would be champion after each series length", x = NULL, y = NULL)

# binomial: how many of 7 games a team that wins 6 out of 10 actually wins
# 4 or more of 7 = wins the series (play-it-out trick), 3 or fewer = upset
binom_7 <- tibble(wins = 0:7) %>%
  mutate(prob = dbinom(wins, 7, 0.6),
         result = ifelse(wins >= 4, "Wins the series", "Upset"),
         record = paste0(wins, "-", 7 - wins),
         bar_label = ifelse(prob < 0.01, "<1%", scales::percent(prob, accuracy = 1)))

series_win_7 <- sum(binom_7$prob[binom_7$wins >= 4])
binom_legend <- c(
  "Wins the series" = paste0("Team A wins the series: ", scales::percent(series_win_7, accuracy = 1)),
  "Upset" = paste0("Team B pulls the upset: ", scales::percent(1 - series_win_7, accuracy = 1))
)

p_binom <- ggplot(binom_7, aes(x = factor(record, levels = record), y = prob, fill = result)) +
  geom_col() +
  geom_text(aes(label = bar_label), vjust = -0.4, size = 4.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), name = "Chance of this record",
                     expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Wins the series" = "#000000", "Upset" = "#e31ab1"),
                    labels = binom_legend, name = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(size = 12), legend.text = element_text(size = 11)) +
  labs(title = "Team A wins 6 out of 10 games against Team B",
       x = "Team A's record if all 7 games were played")

# upset chance for the 4 out of 10 team, every odd series length up to a best of 151
# 40%, 35%, 32%, 29% for 1, 3, 5, 7 games, and under 1% by a best of 151
upsets <- tibble(series_length = seq(1, 151, by = 2)) %>%
  mutate(upset = pbinom((series_length + 1) / 2 - 1, series_length, 0.6))

upsets_labeled <- upsets %>%
  filter(series_length %in% c(1, 3, 5, 7, 151)) %>%
  mutate(label = paste0("Best of ", series_length, ": ",
                        ifelse(upset < 0.01, "<1%", scales::percent(upset, accuracy = 1))),
         label_hjust = ifelse(series_length == 151, 1, -0.1))  # 151's label sits left of its point so it fits

p_upset <- ggplot(upsets, aes(x = series_length, y = upset)) +
  geom_line(color = "#e31ab1", linewidth = 1.2) +
  geom_point(data = upsets_labeled, color = "#e31ab1", size = 2.5) +
  geom_text(data = upsets_labeled, aes(label = label, hjust = label_hjust), vjust = -0.6, size = 3.5) +
  scale_x_continuous(breaks = c(1, 25, 51, 75, 101, 125, 151), name = "Series length (best of ___)",
                     expand = expansion(mult = c(0.02, 0.03))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), name = "Chance of an upset",
                     limits = c(0, 0.42)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  labs(title = "Team B's upset chance when Team A wins 6 out of 10")

# same idea as plot but every odd series length from 1 to 99, marked at a 75% team
# the dots on the 75% line climb toward 100% as the series gets longer
many_lengths <- seq(1, 99, by = 2)

df_many <- expand.grid(p = p_values, series_type = many_lengths) %>%
  mutate(win_probability = 1 - pbinom((series_type + 1) / 2 - 1, series_type, p))

at_75_many <- df_many %>% filter(abs(p - 0.75) < 1e-9)
at_75_labeled <- at_75_many %>% filter(series_type %in% c(1, 3, 7))  # longer ones bunch up near 100%

p_curve_many <- ggplot(df_many, aes(x = p, y = win_probability, color = series_type, group = series_type)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = 0.75, linetype = "dashed") +
  geom_point(data = at_75_many, size = 1.5) +
  geom_text(data = at_75_labeled,
            aes(label = paste0("Best of ", series_type, ": ", scales::percent(win_probability, accuracy = 1))),
            hjust = -0.1, size = 3, show.legend = FALSE) +
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
  scale_color_gradient(name = "Series Length", low = "#e31ab1", high = "#000000") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11)
  ) +
  labs(title = "Every odd series length from 1 to 99")

# finals history: game 1 winner vs champion in every multi-game finals, 1998-2025
# hardcoded from past finals results
finals_g1 <- tribble(
  ~season, ~g1_winner,    ~champion,
  1998,    "Phoenix",     "Houston",
  1999,    "Houston",     "Houston",
  2000,    "Houston",     "Houston",
  2001,    "Los Angeles", "Los Angeles",
  2002,    "Los Angeles", "Los Angeles",
  2003,    "Los Angeles", "Detroit",
  2004,    "Connecticut", "Seattle",
  2005,    "Sacramento",  "Sacramento",
  2006,    "Sacramento",  "Detroit",
  2007,    "Detroit",     "Phoenix",
  2008,    "Detroit",     "Detroit",
  2009,    "Phoenix",     "Phoenix",
  2010,    "Seattle",     "Seattle",
  2011,    "Minnesota",   "Minnesota",
  2012,    "Indiana",     "Indiana",
  2013,    "Minnesota",   "Minnesota",
  2014,    "Phoenix",     "Phoenix",
  2015,    "Indiana",     "Minnesota",
  2016,    "Los Angeles", "Los Angeles",
  2017,    "Los Angeles", "Minnesota",
  2018,    "Seattle",     "Seattle",
  2019,    "Washington",  "Washington",
  2020,    "Seattle",     "Seattle",
  2021,    "Chicago",     "Chicago",
  2022,    "Las Vegas",   "Las Vegas",
  2023,    "Las Vegas",   "Las Vegas",
  2024,    "Minnesota",   "New York",
  2025,    "Las Vegas",   "Las Vegas"
) %>%
  mutate(result = ifelse(g1_winner == champion, "Game 1 winner won the title", "Game 1 loser won the title"))

p_finals_g1 <- ggplot(finals_g1, aes(x = season, y = 1, fill = result)) +
  geom_tile(color = "white", linewidth = 1, height = 0.8) +
  geom_text(data = finals_g1 %>% filter(result == "Game 1 loser won the title"),
            aes(label = champion), angle = 90, color = "white", size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c("Game 1 winner won the title" = "grey80", "Game 1 loser won the title" = "#e31ab1"),
                    name = NULL) +
  scale_x_continuous(breaks = 1998:2025, name = NULL, expand = expansion(add = 0.5)) +
  scale_y_continuous(breaks = NULL, name = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5), panel.grid = element_blank()) +
  labs(title = paste0("The Game 1 loser won the title ", sum(finals_g1$result == "Game 1 loser won the title"),
                      " of ", nrow(finals_g1), " times"),
       subtitle = "Every WNBA Finals that lasted more than 1 game, 1998-2025")

# best of 5 finals (2005-2024) that a best of 3 would have flipped: the team up 2-1 after 3 games lost
# 2009 is the only one where the game 1 winner still won, so it's the only true flip-flop
bo3_flips <- tribble(
  ~Finals, ~`First 3 games`,                         ~`Best of 3 champion`, ~`Actual champion`,
  2006,    "Sacramento, Detroit, Sacramento",         "Sacramento",          "Detroit",
  2007,    "Detroit, Phoenix, Detroit",               "Detroit",             "Phoenix",
  2009,    "Phoenix, Indiana, Indiana",               "Indiana",             "Phoenix",
  2017,    "Los Angeles, Minnesota, Los Angeles",     "Los Angeles",         "Minnesota"
)

# 2009: the only other finals that flip-flopped like 2026
# phoenix won g1, indiana won g2 and g3, phoenix won g4 and g5
finals_2009 <- tibble(game = 1:5, winner = c("Mercury", "Fever", "Fever", "Mercury", "Mercury"))

series_ends_2009 <- tibble(
  x = c(1.5, 3.5, 5.5),
  label = c("best of 1:\nMercury", "best of 3:\nFever", "best of 5:\nMercury")
)

p_flip_2009 <- ggplot(finals_2009, aes(x = game, y = 1, fill = winner)) +
  geom_tile(color = "white", linewidth = 1.5, height = 0.6) +
  geom_text(aes(label = paste0("G", game)), color = "white", size = 5, fontface = "bold") +
  geom_vline(data = series_ends_2009, aes(xintercept = x), linetype = "dashed") +
  geom_text(data = series_ends_2009, aes(x = x - 0.08, y = 1.55, label = label), inherit.aes = FALSE, size = 3.5, hjust = 1) +
  scale_fill_manual(values = c("Mercury" = "#E56020", "Fever" = "#002D62"), name = NULL) +
  scale_x_continuous(breaks = NULL, limits = c(0.5, 5.6)) +
  scale_y_continuous(breaks = NULL, limits = c(0.6, 1.75)) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 14)) +
  labs(title = "2009 Finals: who would be champion after each series length", x = NULL, y = NULL)

# chart index, in script order
p_flip        # hook: 1 game Valkyries, best of 3 Dream, best of 5 Valkyries, best of 7 still open
p_binom       # "it's called the binomial distribution": wins out of 7 for a 6 out of 10 team, upset 29%
p_upset       # upset chance 40% in 1 game down to 29% in a best of 7
plot          # series win chance for best of 3, 5, 7 at every p
p_curve_many  # same chart with every odd series length 1 to 99: at 75%, 1 game 75%, best of 3 84%, best of 7 93%, best of 15 98%
p_finals_g1   # "since 1998, the team that lost game 1 went on to win the title 8 times"
p_flip_2009   # "in 2009... phoenix won game 1, indiana went up 2-1, but then phoenix won it in 5"
print(bo3_flips, width = Inf)  # fun fact: 4 best of 5 finals a best of 3 would have flipped (2006, 2007, 2009, 2017)
