pacman::p_load(wehoop, dplyr, tictoc, progressr, ggplot2)

tictoc::tic()
progressr::with_progress({
  wnba_team_box <- wehoop::load_wnba_team_box(season=c(2016:2025))
})
tictoc::toc()


# Finals champion abbreviations for 2016–2025 (adjust abbreviations if needed)
champ_teams <- data.frame(
  season = 2016:2025,
  team_abbreviation = c(
    "LA",   # 2016: Los Angeles Sparks
    "MIN",   # 2017: Minnesota Lynx
    "SEA",   # 2018: Seattle Storm
    "WSH",   # 2019: Washington Mystics
    "SEA",   # 2020: Seattle Storm
    "CHI",   # 2021: Chicago Sky
    "LV",   # 2022: Las Vegas Aces
    "LV",   # 2023: Las Vegas Aces
    "NY",   # 2024: New York Liberty
    "LV"    # 2025: Las Vegas Aces
  )
)

# Filter to just games for the eventual champions in regular season
champ_games <- wnba_team_box %>%
  filter(season >= 2016, season_type == 2) %>%
  inner_join(champ_teams, by = c("season", "team_abbreviation"))

# Calculate cumulative wins and win percentage per champion and season
champ_cum <- champ_games %>%
  arrange(season, team_abbreviation, game_date) %>%
  group_by(season, team_abbreviation) %>%
  mutate(
    game_number = row_number(),
    cum_wins = cumsum(as.integer(team_winner)),
    cum_win_pct = cum_wins / game_number
  ) %>%
  ungroup()

ggplot(champ_cum, aes(x = game_number, y = cum_win_pct)) +
  geom_line(size = 1.1, color = "#0057b7") +  # You can set color or map it to team_abbreviation
  facet_wrap(~ paste0(season, " - ", team_abbreviation), ncol = 3) +
  labs(
    title = "Cumulative Win % by Game for WNBA Finals Winners",
    x = "Game Number",
    y = "Cumulative Win %"
  ) +
  scale_x_continuous(breaks = seq(1, 44, by = 10)) +
  theme_minimal()


# Calculate time below 0.500 win%
below_500_table <- champ_cum %>%
  group_by(season, team_abbreviation) %>%
  summarise(
    games_under_500 = sum(cum_win_pct < 0.5),
    total_games = n(),
    pct_under_500 = round(100 * games_under_500 / total_games, 1),
  ) %>%
  mutate(team=team_abbreviation)%>%
  select(season, team, games_under_500, pct_under_500) %>%
  arrange(desc(pct_under_500))

# Display the table
print(below_500_table)


# Calculate loss margin for each champion's losses
loss_table <- champ_cum %>%
  filter(team_winner == FALSE) %>%
  mutate(loss_margin = opponent_team_score - team_score) %>%
  group_by(season, team_abbreviation) %>%
  summarise(
    num_losses = n(),
    avg_loss_margin = round(mean(loss_margin, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(avg_loss_margin))

# Display the table
print(loss_table)