# ============================
# Libraries
# ============================
library(wehoop)
library(progressr)
library(tictoc)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(tidyverse)
library(scales)

# ============================
# Load data
# ============================
tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season = c(2026))
})
tictoc::toc()

tictoc::tic()
progressr::with_progress({
  wnba_player_box <- wehoop::load_wnba_player_box(season = c(2026))
})
tictoc::toc()

storm_home <- wnba_team_box %>%
  filter(team_location == "Seattle", team_home_away == "home") %>%
  mutate(game_time_pt = with_tz(game_date_time, tzone = "America/Los_Angeles")) %>%
  select(game_time_pt, team_location, opponent_team_location, team_winner)

#https://app.isthemountainout.com/
storm_home <- storm_home %>%
  mutate(
    mountain = c(
      "out", "in", "in", "out", "out", "in",
      "in", "out", "idk", "out", "idk", "in"
    )
  )

storm_home



storm_home_with_mountain <- wnba_team_box %>%
  filter(team_location == "Seattle", team_home_away == "home") %>%
  mutate(game_time_pt = with_tz(game_date_time, "America/Los_Angeles")) %>%
  select(
    game_id,
    game_time_pt,
    team_location,
    opponent_team_location,
    team_winner
  ) %>%
  mutate(
    mountain = c(
      "out", "in", "in", "out", "out", "in",
      "in", "out", "idk", "out", "idk", "in"
    )
  )

wnba_player_with_mountain <- wnba_player_box %>%
  left_join(
    storm_home_with_mountain %>% select(game_id, mountain),
    by = "game_id"
  )

natisha <- wnba_player_with_mountain %>%
  filter(
    team_location == "Seattle",
    athlete_display_name == "Natisha Hiedeman",
    !is.na(mountain)        # keep only games with mountain tagged
  )

natisha %>%
  select(game_id, game_date, opponent_team_location, points, assists, minutes, mountain)

natisha_by_mountain <- natisha %>%
  group_by(mountain) %>%
  summarise(
    n_games       = n(),
    avg_minutes   = mean(minutes, na.rm = TRUE),
    avg_points    = mean(points, na.rm = TRUE),
    avg_assists   = mean(assists, na.rm = TRUE),
    avg_rebounds  = mean(rebounds, na.rm = TRUE),
    avg_fg_pct    = sum(field_goals_made, na.rm = TRUE) /
                    sum(field_goals_attempted, na.rm = TRUE),
    avg_3p_pct    = sum(three_point_field_goals_made, na.rm = TRUE) /
                    sum(three_point_field_goals_attempted, na.rm = TRUE)
  )

natisha_by_mountain%>% select(mountain, avg_points, avg_assists, avg_rebounds, avg_fg_pct)




natisha_by_mountain <- tibble(
  mountain = c("idk", "in", "out"),
  n_games = c(2, 5, 5),
  avg_minutes = c(29, 29.2, 31.8),
  avg_points = c(22, 13.6, 16.2),
  avg_assists = c(5, 4.2, 5.4),
  avg_rebounds = c(1, 2.4, 2.8),
  avg_fg_pct = c(0.5, 0.431, 0.426),
  avg_3p_pct = c(0.467, 0.267, 0.333)
) %>%
  mutate(
    mountain = factor(mountain, levels = c("out", "in", "idk")),
    label_games = paste0("n = ", n_games)
  )

mountain_colors <- c(
  "out" = "#FBE122",
  "in"  = "#2C5234",
  "idk" = "#78BE21"
)

ggplot(natisha_by_mountain, aes(x = mountain, y = avg_3p_pct, fill = mountain)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = percent(avg_3p_pct, accuracy = 0.1)), hjust = .5, vjust=-.8, size = 5) +
  scale_fill_manual(values = mountain_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.52),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Natisha Hiedeman 3-point shooting by mtn out/in",
    subtitle = "Seattle Storm home games, 2026",
    x = NULL,
    y = "Average 3P%"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )



compare_df <- tibble(
  metric = c("Points", "Assists", "Rebounds"),
  `in` = c(13.6, 4.2, 2.4),
  `out` = c(16.2, 5.4, 2.8)
) %>%
  mutate(metric = factor(metric, levels = rev(metric)))

ggplot(compare_df, aes(y = metric)) +
  geom_segment(aes(x = `in`, xend = `out`, yend = metric), color = "gray70", linewidth = 1.5) +
  geom_point(aes(x = `in`), color = "#7A8FB8", size = 4) +
  geom_point(aes(x = `out`), color = "#5B8E7D", size = 4) +
  geom_text(aes(x = `in`, label = ifelse(metric == "3P%", percent(`in`, 0.1), round(`in`, 1))),
            nudge_y = 0.22, color = "#7A8FB8", size = 4) +
  geom_text(aes(x = `out`, label = ifelse(metric == "3P%", percent(`out`, 0.1), round(`out`, 1))),
            nudge_y = -0.22, color = "#5B8E7D", size = 4) +
  labs(
    title = "Natisha Hiedeman with Rainier in vs out",
    subtitle = "Seattle Storm home games, 2026",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )