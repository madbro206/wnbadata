pacman::p_load(wehoop, dplyr, glue, tictoc, progressr, tidyr, slider, lubridate, stringr)

#load game-by-game data
team_box <- wehoop::load_wbb_team_box(seasons = 2017:2026) %>%
  filter(!is.na(game_id), !is.na(season), !is.na(team_id)) %>%
  transmute(
    season,
    game_id,
    game_date,
    team_id,
    pts = team_score,
    fga = field_goals_attempted,
    fta = free_throws_attempted,
    orb = offensive_rebounds,
    tov = turnovers
  )

#estimate possessions
team_game_stats <- team_box %>%
  mutate(
    poss_est = fga - orb + tov + .44 * fta,
    ppp      = pts / poss_est
  ) %>%
  filter(poss_est > 0)

#create season summaries
season_summary <- team_game_stats %>%
  group_by(season) %>%
  summarise(
    n_team_games = n(),
    avg_ppg = mean(pts, na.rm = TRUE),
    med_ppg = median(pts, na.rm = TRUE),
    avg_possessions = mean(poss_est, na.rm = TRUE),
    med_possessions = median(poss_est, na.rm = TRUE),
    avg_ppp = mean(ppp, na.rm = TRUE),
    .groups = "drop"
  )

season_summary


#ppg trend ncaawbb
ggplot(season_summary, aes(x = season, y = avg_ppg)) +
  geom_line(linewidth = 2, color = "lightblue") +
  geom_point(size = 3, color = "lightblue") +
  geom_text(aes(label = round(avg_possessions, 2)), vjust = -0.5) +
  # extra label just for 2022
  geom_text(
    data = subset(season_summary, season == 2022),
    aes(label = "(3PT line moved back)"),
    vjust = 1.5,    
    color = "red",
    fontface = "bold",
    size = 3.5
  ) +
  scale_x_continuous(breaks = season_summary$season) +
  labs(
    title    = "NCAA WBB average team-game points by season",
    x = "Season",
    y = "Points per team-game",
    caption = "chart: @wnbadata | data: wehoop"
  ) +
  theme_minimal()

#possesions/pace trends
poss_long <- season_summary %>%
  select(season, avg_possessions, med_possessions) %>%
  pivot_longer(cols = c(avg_possessions, med_possessions),
               names_to = "stat",
               values_to = "possessions") %>%
  mutate(stat = recode(stat,
                       avg_possessions = "Average possessions",
                       med_possessions = "Median possessions"))

ggplot(season_summary, aes(x = season, y = avg_possessions)) +
  geom_line(linewidth = 2, color = "lightblue") +
  geom_point(size = 3, color = "lightblue") +
  geom_text(aes(label = round(avg_possessions, 2)), vjust = -0.5) +
  # extra label just for 2022
  geom_text(
    data = subset(season_summary, season == 2022),
    aes(label = "(3PT line moved back)"),
    vjust = 2,    
    color = "red",
    fontface = "bold",
    size = 3.5
  ) +
  scale_x_continuous(breaks = season_summary$season) +
  labs(
    title    = "NCAA WBB estimated average pace by season",
    subtitle = "poss = FGA - ORB + TOV + 0.44 × FTA",
    x = "Season",
    y = "Possessions per team-game",
    caption = "chart: @wnbadata | data: wehoop"
  ) +
  theme_minimal()



#create in-season cumulative to-date average scoring and pace
season_focus <- 2026 #for 2026 specifically

daily_summary <- team_game_stats %>%
  group_by(season, game_date) %>%
  summarise(
    n_team_games    = n(),
    avg_ppg         = mean(pts, na.rm = TRUE),
    med_ppg         = median(pts, na.rm = TRUE),
    avg_possessions = mean(poss_est, na.rm = TRUE),
    med_possessions = median(poss_est, na.rm = TRUE),
    .groups = "drop"
  )

daily_summary <- daily_summary %>%
  arrange(game_date) %>%
  group_by(season) %>%
  mutate(
    avg_ppg_roll7         = slider::slide_dbl(avg_ppg, mean, .before = 6, .complete = FALSE),
    avg_possessions_roll7 = slider::slide_dbl(avg_possessions, mean, .before = 6, .complete = FALSE)
  ) %>%
  ungroup()

daily_cum <- daily_summary %>%
  arrange(season, game_date) %>%
  group_by(season) %>%
  mutate(
    # cumulative sums
    cum_games          = cumsum(n_team_games),
    cum_pts_weighted   = cumsum(avg_ppg * n_team_games),
    cum_poss_weighted  = cumsum(avg_possessions * n_team_games),

    # cumulative to-date *game-weighted* averages
    cum_avg_ppg        = cum_pts_weighted  / cum_games,
    cum_avg_possessions = cum_poss_weighted / cum_games
  ) %>%
  ungroup()

daily_cum <- daily_cum %>%
  group_by(season) %>%
  mutate(
    cum_med_ppg = slider::slide_dbl(med_ppg, median, .before = Inf),
    cum_med_possessions = slider::slide_dbl(med_possessions, median, .before = Inf)
  ) %>%
  ungroup()

daily_cum %>%
  filter(season == season_focus) %>%
  ggplot(aes(x = game_date, y = cum_avg_possessions)) +
  geom_line(color = "#d62728", linewidth = 0.9) +
  labs(
    title = paste0("Cumulative to-date average pace, ", season_focus, " NCAAW season"),
    x = "Date",
    y = "Avg possessions per team-game (to date)",
    caption="chart: @wnbadata | data: wehoop"
  ) +
  theme_minimal()

daily_cum %>%
  filter(season == season_focus) %>%
  ggplot(aes(x = game_date, y = cum_avg_ppg)) +
  geom_line(color = "#1f77b4", linewidth = 0.9) +
  labs(
    title = paste0("Cumulative to-date average scoring, ", season_focus, " NCAAW season"),
    x = "Date",
    y = "Avg points per team-game (to date)",
    caption="chart: @wnbadata | data: wehoop"
  ) +
  theme_minimal()
